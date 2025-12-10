;;; journelly-location-weather.el --- Location and weather helpers for Journelly -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Vincent Demeester

;; Author: Vincent Demeester <vincent@demeester.fr>
;; Keywords: org-mode, journelly, location, weather
;; Version: 1.0.0

;;; Commentary:

;; Emacs Lisp functions to get location and weather data for Journelly journal entries.
;;
;; Location:
;; - Uses IP-based geolocation (ipinfo.io)
;; - Returns city name and GPS coordinates
;; - Caches results for 1 hour
;;
;; Weather:
;; - Uses wttr.in weather service
;; - Returns temperature, condition, and iOS SF Symbol
;; - Caches results for 30 minutes
;; - Intelligent day/night symbol mapping
;;
;; Functions:
;; - journelly-get-location: Get current location via IP geolocation
;; - journelly-get-weather: Get current weather
;; - journelly-batch-get-location: Batch mode wrapper for location
;; - journelly-batch-get-weather: Batch mode wrapper for weather
;;
;; Usage (batch mode):
;;   emacs --batch \
;;     --load journelly-location-weather.el \
;;     --eval "(journelly-batch-get-location)"
;;
;;   emacs --batch \
;;     --load journelly-location-weather.el \
;;     --eval "(journelly-batch-get-weather)"

;;; Code:

(require 'url)
(require 'json)

;;; Configuration

(defvar journelly-cache-dir
  (expand-file-name "journal" (or (getenv "XDG_CACHE_HOME")
                                   (expand-file-name ".cache" "~")))
  "Directory for caching location and weather data.")

(defvar journelly-location-cache-timeout 3600
  "Location cache timeout in seconds (default: 1 hour).")

(defvar journelly-weather-cache-timeout 1800
  "Weather cache timeout in seconds (default: 30 minutes).")

;;; Utility functions

(defun journelly--ensure-cache-dir ()
  "Ensure cache directory exists."
  (unless (file-exists-p journelly-cache-dir)
    (make-directory journelly-cache-dir t)))

(defun journelly--cache-file (key)
  "Get cache file path for KEY."
  (expand-file-name (format "%s.json" key) journelly-cache-dir))

(defun journelly--cache-valid-p (cache-file timeout)
  "Check if CACHE-FILE is valid within TIMEOUT seconds."
  (when (file-exists-p cache-file)
    (let* ((file-time (nth 5 (file-attributes cache-file)))
           (current-time (current-time))
           (age (float-time (time-subtract current-time file-time))))
      (< age timeout))))

(defun journelly--read-cache (cache-file)
  "Read JSON data from CACHE-FILE."
  (when (file-exists-p cache-file)
    (with-temp-buffer
      (insert-file-contents cache-file)
      (goto-char (point-min))
      (json-read))))

(defun journelly--write-cache (cache-file data)
  "Write DATA as JSON to CACHE-FILE."
  (journelly--ensure-cache-dir)
  (with-temp-file cache-file
    (insert (json-encode data))))

(defun journelly--fetch-url (url)
  "Fetch URL and return parsed JSON response."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("User-Agent" . "Emacs/journelly"))))
    (with-current-buffer (url-retrieve-synchronously url t nil 10)
      (goto-char (point-min))
      ;; Skip HTTP headers
      (re-search-forward "^$")
      (forward-line)
      (let ((json-data (json-read)))
        (kill-buffer)
        json-data))))

(defun journelly--is-night-p ()
  "Return t if current time is night (20:00-06:00)."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (or (>= hour 20) (< hour 6))))

;;; Location functions

(defun journelly--map-weather-symbol (description &optional is-night)
  "Map weather DESCRIPTION to iOS SF Symbol name.
If IS-NIGHT is non-nil, return night-appropriate symbols."
  (let ((desc (downcase description)))
    (if is-night
        ;; Night conditions
        (cond
         ((string-match-p "\\(clear\\|sunny\\)" desc) "moon.stars")
         ((string-match-p "partly.*cloud" desc) "cloud.moon")
         ((string-match-p "\\(rain\\|drizzle\\|shower\\)" desc) "cloud.moon.rain")
         (t "cloud.moon"))
      ;; Day conditions
      (cond
       ((string-match-p "\\(clear\\|sunny\\)" desc) "sun.max")
       ((string-match-p "partly.*cloud" desc) "cloud.sun")
       ((string-match-p "\\(cloudy\\|overcast\\)" desc) "cloud")
       ((string-match-p "heavy.*rain" desc) "cloud.heavyrain")
       ((string-match-p "\\(rain\\|shower\\)" desc) "cloud.rain")
       ((string-match-p "\\(drizzle\\|light.*rain\\)" desc) "cloud.drizzle")
       ((string-match-p "snow" desc) "cloud.snow")
       ((string-match-p "sleet" desc) "cloud.sleet")
       ((string-match-p "\\(fog\\|mist\\)" desc) "cloud.fog")
       ((string-match-p "\\(haze\\|smoke\\)" desc) "smoke")
       ((string-match-p "wind" desc) "wind")
       ((string-match-p "\\(thunder\\|storm\\)" desc) "cloud.bolt")
       (t "cloud")))))

(defun journelly-get-location (&optional no-cache)
  "Get current location via IP geolocation.
Returns alist with city, latitude, and longitude.
If NO-CACHE is non-nil, fetch fresh data ignoring cache."
  (let ((cache-file (journelly--cache-file "location")))
    (if (and (not no-cache)
             (journelly--cache-valid-p cache-file journelly-location-cache-timeout))
        ;; Return cached data
        (journelly--read-cache cache-file)
      ;; Fetch fresh data
      (let* ((response (journelly--fetch-url "https://ipinfo.io/json"))
             (city (cdr (assoc 'city response)))
             (loc (cdr (assoc 'loc response)))
             (coords (when loc (split-string loc ",")))
             (lat (when coords (car coords)))
             (lon (when coords (cadr coords)))
             (data `((city . ,(or city "Unknown"))
                    (lat . ,(or lat "0"))
                    (lon . ,(or lon "0")))))
        ;; Cache the result
        (journelly--write-cache cache-file data)
        data))))

(defun journelly-get-weather (&optional location no-cache)
  "Get current weather for LOCATION (city name or coordinates).
If LOCATION is nil, uses current location via IP.
Returns alist with temperature, condition, and symbol.
If NO-CACHE is non-nil, fetch fresh data ignoring cache."
  (let* ((loc (or location ""))
         (cache-key (if (string-empty-p loc) "weather-auto" (format "weather-%s" loc)))
         (cache-file (journelly--cache-file cache-key)))
    (if (and (not no-cache)
             (journelly--cache-valid-p cache-file journelly-weather-cache-timeout))
        ;; Return cached data
        (journelly--read-cache cache-file)
      ;; Fetch fresh data
      (let* ((url (if (string-empty-p loc)
                     "https://wttr.in/?format=j1"
                   (format "https://wttr.in/%s?format=j1" (url-hexify-string loc))))
             (response (journelly--fetch-url url))
             (current (aref (cdr (assoc 'current_condition response)) 0))
             (temp-c (cdr (assoc 'temp_C current)))
             (weather-desc-array (cdr (assoc 'weatherDesc current)))
             (weather-desc (cdr (assoc 'value (aref weather-desc-array 0))))
             (temperature (format "%s°C" temp-c))
             (is-night (journelly--is-night-p))
             (symbol (journelly--map-weather-symbol weather-desc is-night))
             (data `((temperature . ,temperature)
                    (condition . ,weather-desc)
                    (symbol . ,symbol))))
        ;; Cache the result
        (journelly--write-cache cache-file data)
        data))))

;;; Batch mode functions

(defun journelly-batch-get-location (&optional format no-cache)
  "Batch mode: Get location and print to stdout.
FORMAT can be: json (default), city, coords, lat, lon, or all.
If NO-CACHE is non-nil, ignore cache."
  (let* ((format-type (or format "json"))
         (data (journelly-get-location no-cache))
         (city (cdr (assoc 'city data)))
         (lat (cdr (assoc 'lat data)))
         (lon (cdr (assoc 'lon data))))
    (cond
     ((string= format-type "json")
      (princ (json-encode data))
      (terpri))
     ((string= format-type "city")
      (princ city)
      (terpri))
     ((string= format-type "coords")
      (princ (format "%s,%s" lat lon))
      (terpri))
     ((string= format-type "lat")
      (princ lat)
      (terpri))
     ((string= format-type "lon")
      (princ lon)
      (terpri))
     ((string= format-type "all")
      (princ (format "%s (%s,%s)" city lat lon))
      (terpri))
     (t
      (error "Unknown format: %s" format-type)))))

(defun journelly-batch-get-weather (&optional location format no-cache)
  "Batch mode: Get weather and print to stdout.
LOCATION is optional city name or coordinates.
FORMAT can be: json (default), temperature, condition, symbol, or all.
If NO-CACHE is non-nil, ignore cache."
  (let* ((format-type (or format "json"))
         (data (journelly-get-weather location no-cache))
         (temperature (cdr (assoc 'temperature data)))
         (condition (cdr (assoc 'condition data)))
         (symbol (cdr (assoc 'symbol data))))
    (cond
     ((string= format-type "json")
      (princ (json-encode data))
      (terpri))
     ((string= format-type "temperature")
      (princ temperature)
      (terpri))
     ((string= format-type "condition")
      (princ condition)
      (terpri))
     ((string= format-type "symbol")
      (princ symbol)
      (terpri))
     ((string= format-type "all")
      (princ (format "%s %s (%s)" temperature condition symbol))
      (terpri))
     (t
      (error "Unknown format: %s" format-type)))))

;;; Provide

(provide 'journelly-location-weather)

;;; journelly-location-weather.el ends here
