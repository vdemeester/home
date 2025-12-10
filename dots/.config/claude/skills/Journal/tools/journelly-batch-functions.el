;;; journelly-batch-functions.el --- Batch functions for Journelly journal entries -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Vincent Demeester

;; Author: Vincent Demeester <vincent@demeester.fr>
;; Keywords: org-mode, journelly, batch
;; Version: 1.0.0

;;; Commentary:

;; Emacs batch mode functions for manipulating Journelly.org journal files.
;; Journelly is an iOS app that stores journal entries in org-mode format.
;;
;; Format:
;; - Single file with entries in reverse chronological order (newest first)
;; - Each entry is a top-level heading: * [YYYY-MM-DD Day HH:MM] @ Location
;; - Optional PROPERTIES drawer with GPS/weather metadata
;; - Free-form org-mode content
;;
;; Functions:
;; - journelly-batch-create-entry: Create new journal entry
;; - journelly-batch-create-entry-auto: Create entry with automatic location/weather
;; - journelly-batch-append-to-today: Append to today's entry
;; - journelly-batch-list-entries: List recent entries
;; - journelly-batch-search: Search entry content
;; - journelly-batch-get-entry: Get specific entry by date/time
;;
;; Usage:
;;   emacs --batch \
;;     --load journelly-batch-functions.el \
;;     --eval "(journelly-batch-create-entry \
;;               \"~/desktop/org/Journelly.org\" \
;;               \"Home\" \
;;               \"Entry content\")"

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

;; Load location/weather functions if available
(let ((location-weather-file
       (expand-file-name "journelly-location-weather.el"
                        (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p location-weather-file)
    (load location-weather-file)))

;; Declare functions from journelly-location-weather.el (loaded conditionally above)
(declare-function journelly-get-location "journelly-location-weather")
(declare-function journelly-get-weather "journelly-location-weather")

;;; Utility functions

(defun journelly--format-timestamp ()
  "Generate org-mode timestamp for current time: [YYYY-MM-DD Day HH:MM]."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun journelly--format-date-only ()
  "Generate date only: YYYY-MM-DD."
  (format-time-string "%Y-%m-%d"))

(defun journelly--parse-timestamp (heading)
  "Extract timestamp from HEADING.
Expected format: * [YYYY-MM-DD Day HH:MM] @ Location
Returns the timestamp string or nil."
  (when (string-match "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Z][a-z][a-z]\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]" heading)
    (match-string 0 heading)))

(defun journelly--parse-location (heading)
  "Extract location from HEADING.
Expected format: * [YYYY-MM-DD Day HH:MM] @ Location
Returns the location string or nil."
  (when (string-match "@ \\(.+\\)$" heading)
    (match-string 1 heading)))

(defun journelly--make-heading (location)
  "Create journal entry heading with current timestamp and LOCATION."
  (format "* %s @ %s" (journelly--format-timestamp) location))

(defun journelly--make-properties (latitude longitude temperature condition symbol)
  "Create PROPERTIES drawer with GPS and weather data.
LATITUDE, LONGITUDE, TEMPERATURE, CONDITION, SYMBOL are optional strings.
Returns nil if no properties provided."
  (let ((props '()))
    (when latitude
      (push (format ":LATITUDE: %s" latitude) props))
    (when longitude
      (push (format ":LONGITUDE: %s" longitude) props))
    (when temperature
      (push (format ":WEATHER_TEMPERATURE: %s" temperature) props))
    (when condition
      (push (format ":WEATHER_CONDITION: %s" condition) props))
    (when symbol
      (push (format ":WEATHER_SYMBOL: %s" symbol) props))
    (when props
      (concat ":PROPERTIES:\n"
              (mapconcat 'identity (nreverse props) "\n")
              "\n:END:\n"))))

(defun journelly--find-header-end (buffer)
  "Find the end of the Journelly header in BUFFER.
Returns the position after the :end: line, or nil if not found."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^:end:$" nil t)
      (forward-line 1)
      (point))))

(defun journelly--json-response (success data &optional message)
  "Create JSON response object.
SUCCESS is boolean, DATA is any JSON-serializable value.
MESSAGE is optional error/success message."
  (let ((response `((success . ,success)
                    (data . ,data))))
    (when message
      (push `(message . ,message) response))
    (json-encode response)))

(defun journelly--output-json (success data &optional message)
  "Output JSON response to stdout.
SUCCESS is boolean, DATA is the response data, MESSAGE is optional."
  (princ (journelly--json-response success data message))
  (terpri))

;;; Main functions

(defun journelly-batch-create-entry (file location content &optional latitude longitude temperature condition symbol content-file)
  "Create new journal entry in FILE.

Arguments:
  FILE: Path to Journelly.org file
  LOCATION: Location string (e.g., \"Home\", \"Kyushu\")
  CONTENT: Entry content (can be empty string)
  LATITUDE: Optional GPS latitude
  LONGITUDE: Optional GPS longitude
  TEMPERATURE: Optional temperature (e.g., \"15,2°C\")
  CONDITION: Optional weather condition (e.g., \"Cloudy\")
  SYMBOL: Optional weather symbol (e.g., \"cloud\")
  CONTENT-FILE: Optional path to file containing content

If CONTENT-FILE is provided, reads content from file instead of CONTENT arg.

Returns JSON with success status and entry details."
  (condition-case err
      (let ((actual-content (if content-file
                                (with-temp-buffer
                                  (insert-file-contents content-file)
                                  (buffer-string))
                              content)))
        (with-temp-buffer
          (insert-file-contents file)

          ;; Find where to insert (after header)
          (let ((insert-pos (journelly--find-header-end (current-buffer))))
            (unless insert-pos
              (error "Could not find Journelly header end marker (:end:)"))

            (goto-char insert-pos)

            ;; Build entry
            (let ((heading (journelly--make-heading location))
                  (properties (journelly--make-properties
                              latitude longitude temperature condition symbol))
                  (timestamp (journelly--format-timestamp)))

              ;; Insert entry
              (insert heading "\n")
              (when properties
                (insert properties))
              (when (and actual-content (not (string-empty-p actual-content)))
                (insert actual-content)
                (unless (string-suffix-p "\n" actual-content)
                  (insert "\n")))
              (insert "\n")  ;; Blank line after entry

              ;; Write back to file
              (write-region (point-min) (point-max) file)

              ;; Return success
              (journelly--output-json
               t
               `((timestamp . ,timestamp)
                 (location . ,location)
                 (has-properties . ,(if properties t :json-false))
                 (file . ,file))
               "Journal entry created successfully")))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

(defun journelly-batch-append-to-today (file content &optional content-file)
  "Append CONTENT to today's journal entry in FILE.

Arguments:
  FILE: Path to Journelly.org file
  CONTENT: Content to append
  CONTENT-FILE: Optional path to file containing content

If no entry exists for today, returns error.
Returns JSON with success status."
  (condition-case err
      (let ((actual-content (if content-file
                                (with-temp-buffer
                                  (insert-file-contents content-file)
                                  (buffer-string))
                              content))
            (today-date (journelly--format-date-only)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))

          ;; Find today's entry
          (let ((found nil)
                (search-pattern (format "^\\* \\[%s " today-date)))
            (while (and (not found)
                       (re-search-forward search-pattern nil t))
              (setq found t))

            (unless found
              (error "No journal entry found for today (%s)" today-date))

            ;; Move to end of this entry (before next heading or end of file)
            (forward-line 1)
            (if (re-search-forward "^\\* \\[" nil t)
                (progn
                  (beginning-of-line)
                  (backward-char 1))  ;; Before the newline
              (goto-char (point-max)))

            ;; Insert content
            (insert "\n" actual-content)
            (unless (string-suffix-p "\n" actual-content)
              (insert "\n"))

            ;; Write back
            (write-region (point-min) (point-max) file)

            ;; Return success
            (journelly--output-json
             t
             `((date . ,today-date)
               (file . ,file))
             "Content appended to today's entry"))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

(defun journelly-batch-list-entries (file &optional limit)
  "List recent journal entries from FILE.

Arguments:
  FILE: Path to Journelly.org file
  LIMIT: Optional number of entries to return (default 10)

Returns JSON with list of entries."
  (condition-case err
      (let ((max-entries (or (and limit (string-to-number limit)) 10))
            (entries '()))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))

          ;; Skip header
          (journelly--find-header-end (current-buffer))

          ;; Parse entries
          (while (and (< (length entries) max-entries)
                     (re-search-forward "^\\* \\(\\[.*?\\]\\) @ \\(.+\\)$" nil t))
            (let ((timestamp (match-string 1))
                  (location (match-string 2))
                  (has-properties nil)
                  (content-preview ""))

              ;; Check for properties
              (save-excursion
                (forward-line 1)
                (when (looking-at "^:PROPERTIES:")
                  (setq has-properties t)))

              ;; Get content preview (first 100 chars)
              (save-excursion
                (forward-line 1)
                (when has-properties
                  (re-search-forward "^:END:$" nil t)
                  (forward-line 1))
                (let ((content-start (point)))
                  (if (re-search-forward "^\\* \\[" nil t)
                      (beginning-of-line)
                    (goto-char (point-max)))
                  (setq content-preview
                        (string-trim
                         (buffer-substring-no-properties content-start (point))))
                  (when (> (length content-preview) 100)
                    (setq content-preview
                          (concat (substring content-preview 0 100) "...")))))

              (push `((timestamp . ,timestamp)
                     (location . ,location)
                     (has-properties . ,(if has-properties t :json-false))
                     (preview . ,content-preview))
                    entries)))

          ;; Return results (already in reverse chronological from file)
          (journelly--output-json t (nreverse entries))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

(defun journelly-batch-search (file query)
  "Search journal entries in FILE for QUERY.

Arguments:
  FILE: Path to Journelly.org file
  QUERY: Search string (case-insensitive)

Returns JSON with matching entries."
  (condition-case err
      (let ((matches '())
            (query-lower (downcase query)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))

          ;; Skip header
          (journelly--find-header-end (current-buffer))

          ;; Search entries
          (while (re-search-forward "^\\* \\(\\[.*?\\]\\) @ \\(.+\\)$" nil t)
            (let ((timestamp (match-string 1))
                  (location (match-string 2))
                  (entry-start (point))
                  (entry-end nil)
                  (entry-content ""))

              ;; Find entry end
              (save-excursion
                (if (re-search-forward "^\\* \\[" nil t)
                    (setq entry-end (match-beginning 0))
                  (setq entry-end (point-max))))

              ;; Get entry content
              (setq entry-content
                    (buffer-substring-no-properties entry-start entry-end))

              ;; Check if query matches
              (when (string-match-p query-lower (downcase entry-content))
                (push `((timestamp . ,timestamp)
                       (location . ,location)
                       (content . ,(string-trim entry-content)))
                      matches))))

          ;; Return results
          (journelly--output-json
           t
           (nreverse matches)
           (format "Found %d matching entries" (length matches)))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

(defun journelly-batch-get-entry (file date &optional time)
  "Get specific journal entry from FILE by DATE and optional TIME.

Arguments:
  FILE: Path to Journelly.org file
  DATE: Date string (YYYY-MM-DD)
  TIME: Optional time string (HH:MM)

Returns JSON with entry details or error if not found."
  (condition-case err
      (let ((search-pattern (if time
                               (format "^\\* \\[%s .* %s\\]" date time)
                             (format "^\\* \\[%s " date)))
            (found nil))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))

          ;; Skip header
          (journelly--find-header-end (current-buffer))

          ;; Search for entry
          (when (re-search-forward search-pattern nil t)
            (beginning-of-line)
            (when (looking-at "^\\* \\(\\[.*?\\]\\) @ \\(.+\\)$")
              (let ((timestamp (match-string 1))
                    (location (match-string 2))
                    (entry-end nil)
                    (has-properties nil)
                    (properties nil)
                    (content ""))

                (forward-line 1)

                ;; Check for properties
                (when (looking-at "^:PROPERTIES:")
                  (setq has-properties t)
                  (let ((props-start (point)))
                    (re-search-forward "^:END:$" nil t)
                    (setq properties
                          (buffer-substring-no-properties props-start (point)))
                    (forward-line 1)))

                ;; Get content
                (let ((content-start (point)))
                  (if (re-search-forward "^\\* \\[" nil t)
                      (setq entry-end (match-beginning 0))
                    (setq entry-end (point-max)))
                  (setq content
                        (string-trim
                         (buffer-substring-no-properties content-start entry-end))))

                (setq found `((timestamp . ,timestamp)
                            (location . ,location)
                            (has-properties . ,(if has-properties t :json-false))
                            (properties . ,(or properties ""))
                            (content . ,content))))))

          (if found
              (journelly--output-json t found)
            (journelly--output-json
             nil
             nil
             (format "No entry found for %s%s"
                    date
                    (if time (format " at %s" time) ""))))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

(defun journelly-batch-create-entry-auto (file content &optional content-file use-location use-weather)
  "Create journal entry with automatic location and/or weather detection.

Arguments:
  FILE: Path to Journelly.org file
  CONTENT: Entry content
  CONTENT-FILE: Optional path to file containing content
  USE-LOCATION: If non-nil, automatically detect location
  USE-WEATHER: If non-nil, automatically detect weather

Requires journelly-location-weather.el to be loaded.

Returns JSON with success status and entry details."
  (condition-case err
      (progn
        (unless (fboundp 'journelly-get-location)
          (error "Location/weather functions not available. Load journelly-location-weather.el"))

        (let ((location-data (when use-location (journelly-get-location)))
              (weather-data (when use-weather (journelly-get-weather)))
              (actual-content (if content-file
                                 (with-temp-buffer
                                   (insert-file-contents content-file)
                                   (buffer-string))
                               content)))

          ;; Extract data
          (let ((city (when location-data (cdr (assoc 'city location-data))))
                (lat (when location-data (cdr (assoc 'lat location-data))))
                (lon (when location-data (cdr (assoc 'lon location-data))))
                (temp (when weather-data (cdr (assoc 'temperature weather-data))))
                (cond (when weather-data (cdr (assoc 'condition weather-data))))
                (symbol (when weather-data (cdr (assoc 'symbol weather-data)))))

            ;; Create entry
            (journelly-batch-create-entry
             file
             (or city "Unknown")
             actual-content
             lat lon temp cond symbol nil))))
    (error
     (journelly--output-json nil nil (error-message-string err)))))

;;; Provide

(provide 'journelly-batch-functions)

;;; journelly-batch-functions.el ends here
