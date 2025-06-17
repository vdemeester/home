;;; whisper.el --- Record audio and transcribe using whisper-cli -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This library provides functions to record audio using ffmpeg and
;; transcribe it using the whisper-cli command-line tool.

;;; Code:

(defgroup whisper nil
  "Settings for the whisper audio transcription library."
  :group 'tools)

(defcustom whisper-cli-executable "whisper-cli"
  "Path to the whisper-cli executable."
  :type 'string
  :group 'whisper)

(defcustom whisper-model "base"
  "The whisper model to use for transcription (e.g., tiny, base, small, medium, large)."
  :type 'string
  :group 'whisper)

(defcustom whisper-language "en"
  "The language for transcription (e.g., en, es, fr, de)."
  :type 'string
  :group 'whisper)

(defcustom whisper-ffmpeg-timeout 300
  "Default timeout in seconds for ffmpeg recording."
  :type 'integer
  :group 'whisper)

(defcustom whisper-ffmpeg-audio-input-source "pulse"
  "FFmpeg audio input source. For PulseAudio, usually 'pulse'.
For ALSA, it might be 'hw:0'. For macOS, it might be ':0' (for default input).
You might need to adjust this based on your system's ffmpeg configuration."
  :type 'string
  :group 'whisper)

(defcustom whisper-ffmpeg-audio-input-device "default"
  "FFmpeg audio input device. For PulseAudio, often 'default'.
For ALSA, it might be something like 'plughw:1,0'.
For macOS, check available devices with `ffmpeg -f avfoundation -list_devices true -i \"\"`."
  :type 'string
  :group 'whisper)


(defvar whisper--recording-process nil
  "Holds the ffmpeg recording process.")
(defvar whisper--original-mode-line-format mode-line-format
  "To store the original mode-line format.")

(defun whisper--start-mode-line-indicator (indicator)
  "Display an INDICATOR in the mode line."
  (setq whisper--original-mode-line-format mode-line-format)
  (setq-default mode-line-format
                (cons (format " %s " indicator)
                      (if (listp mode-line-format) mode-line-format (list mode-line-format)))))

(defun whisper--stop-mode-line-indicator ()
  "Restore the original mode line."
  (setq-default mode-line-format whisper--original-mode-line-format))

(defun whisper--record-audio (output-file-basename timeout callback)
  "Record audio using ffmpeg.
A temporary WAV file will be created based on OUTPUT-FILE-BASENAME.
Recording runs for TIMEOUT seconds, or until the process is interrupted.
Then, execute CALLBACK function with the path to the recorded audio file.
The CALLBACK is responsible for processing and eventually deleting the audio file."
  (whisper--start-mode-line-indicator "")
  (message "Recording audio for up to %d seconds (or run 'whisper-run' again to stop early)..." timeout)
  (let* ((temp-wav-file (make-temp-file "whisper-audio-" nil ".wav"))
         (process-environment (copy-sequence process-environment))
         (ffmpeg-command
          (list "ffmpeg"
                "-y" ; Overwrite output files without asking
                "-f" whisper-ffmpeg-audio-input-source
                "-i" whisper-ffmpeg-audio-input-device
                "-t" (number-to-string timeout)
                temp-wav-file)))
    (setenv "LC_ALL" "C" process-environment) ; Ensure consistent ffmpeg output
    (setq whisper--recording-process
          (apply #'start-process "whisper-ffmpeg" "*whisper-ffmpeg-output*" ffmpeg-command))

    (set-process-sentinel
     whisper--recording-process
     (lambda (proc _event)
       (let ((status (process-status proc))
             (audio-file-processed nil)) ; Flag to track if callback was called
         (unwind-protect
             (cond
              ((memq status '(exit signal)) ; Process has definitely terminated
               (if (and (file-exists-p temp-wav-file)
                        ;; Check if file has content (size > 0)
                        (> (nth 7 (file-attributes temp-wav-file)) 0))
                   (progn
                     (message "Recording finished/stopped. Audio file: %s" temp-wav-file)
                     (setq audio-file-processed t)
                     (funcall callback temp-wav-file)) ; Pass to callback for transcription
                 (progn
                   (message "Recording failed or produced no usable audio data."))))
              (t ; Other statuses - should not happen often for a finished process
               (message "Recording process ended in unexpected state: %s" status)))
           ;; Cleanup actions
           (setq whisper--recording-process nil) ; Clear the process variable
           (whisper--stop-mode-line-indicator)   ; Always restore mode line
           ;; Delete the temp wav file only if it was not passed to the callback
           (when (and (not audio-file-processed) (file-exists-p temp-wav-file))
             (message "Deleting unused/empty temp audio file: %s" temp-wav-file)
             (delete-file temp-wav-file))
           ))))
    whisper--recording-process))

(defun whisper--transcribe (audio-file callback)
  "Transcribe AUDIO-FILE using whisper-cli and call CALLBACK with transcription."
  (whisper--start-mode-line-indicator "")
  (message "Transcribing audio...")
  (let* ((temp-output-file (make-temp-file "whisper-transcription-" nil ".txt"))
         (command (list whisper-cli-executable
                        audio-file
                        "--model" whisper-model
                        "--language" whisper-language
                        "--output_txt" ; Ensure whisper-cli outputs a .txt file
                        "--output_dir" (file-name-directory temp-output-file))))
    (message "Running command: %s" (string-join command " "))
    (let ((process (apply #'start-process "whisper-cli" "*whisper-cli-output*" command)))
      (set-process-sentinel
       process
       (lambda (_proc _event)
         (whisper--stop-mode-line-indicator)
         (unwind-protect
             (if (and (eq (process-status process) 'exit)
                      (= (process-exit-status process) 0))
                 (let* ((expected-txt-name (concat (file-name-sans-extension audio-file) ".txt"))
                        (transcription-file (expand-file-name expected-txt-name (file-name-directory temp-output-file))))
                   (if (file-exists-p transcription-file)
                       (progn
                         (message "Transcription successful.")
                         (with-temp-buffer
                           (insert-file-contents transcription-file)
                           (funcall callback (buffer-string)))
                         (delete-file transcription-file)) ; Delete the .txt file created by whisper-cli
                     (message "Transcription output file not found: %s" transcription-file)))
               (message "whisper-cli transcription failed. Check *whisper-cli-output* buffer."))
           (when (file-exists-p audio-file)
             (delete-file audio-file)) ; Clean up the audio file
           (when (file-exists-p temp-output-file)
             (delete-file temp-output-file)) ; Clean up the placeholder temp file
           ))))))

;;;###autoload
;;;###autoload
(defun whisper-run ()
  "Record audio, transcribe it, and insert the text into the current buffer.
If a recording is already in progress (started by this command),
running `whisper-run` again will stop the current recording, and
transcription will proceed on the audio captured so far.
Uses `whisper-ffmpeg-timeout` for recording duration if starting anew."
  (interactive)
  (if (and whisper--recording-process (process-live-p whisper--recording-process))
      (progn
        (message "Stopping current recording...")
        (interrupt-process whisper--recording-process)
        ;; The sentinel of the existing whisper--recording-process will handle
        ;; the audio file and initiate transcription.
        )
    ;; Else, no recording in progress, so start a new one.
    (whisper--record-audio
     "whisper-rec-" ; Base name for make-temp-file
     whisper-ffmpeg-timeout
     (lambda (audio-file) ; This is the callback from whisper--record-audio
       ;; audio-file here is the temp-wav-file from whisper--record-audio
       (if (and audio-file (file-exists-p audio-file))
           (whisper--transcribe
            audio-file ; whisper--transcribe is now responsible for this audio-file
            (lambda (transcription)
              (if (string-empty-p transcription)
                  (message "Transcription is empty.")
                (insert transcription))
              (message "Transcription inserted.")))
         (message "No valid audio file was recorded to transcribe."))))))

;;;###autoload
(defun whisper-file (file)
  "Record audio, transcribe it, and append the text to the specified FILE.
Uses `whisper-ffmpeg-timeout` for recording duration."
  (interactive "FAppend transcription to file: ")
  (unless (file-writable-p (file-name-directory file))
    (error "Directory for file %s is not writable" file))
  (whisper--record-audio
   "whisper-temp-output.wav" ; Not directly used
   whisper-ffmpeg-timeout
   (lambda (audio-file)
     (whisper--transcribe
      audio-file
      (lambda (transcription)
        (if (string-empty-p transcription)
            (message "Transcription is empty. Nothing appended to %s." file)
          (with-temp-buffer
            (insert transcription)
            (append-to-file nil nil file))
          (message "Transcription appended to %s." file)))))))

(provide 'whisper)

;;; whisper.el ends here
