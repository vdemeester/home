;;; mcp.el --- Model Context Protocol                -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (jsonrpc "1.0.25"))
;; Keywords: ai, mcp
;; URL: https://github.com/lizqwerscott/mcp.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'jsonrpc)
(require 'cl-lib)
(require 'url)

(defconst *MCP-VERSION* "2024-11-05"
  "MCP support version.")

(defcustom mcp-server-start-time 60
  "The Seconds of mcp server start time."
  :group 'mcp
  :type 'integer)

(defcustom mcp-server-wait-initial-time 2
  "Seconds to wait after server init before fetching MCP resources.

This delay is applied after server initialization completes, but
before requesting tools, prompts and resources. Gives the server
time to fully initialize all components before handling requests."
  :group 'mcp
  :type 'integer)

(defcustom mcp-log-level 'info
  "The min log level for mcp server.
Available levels:
- debug: Detailed debugging information (function entry/exit points)
- info: General informational messages (operation progress updates)
- notice: Normal but significant events (configuration changes)
- warning: Warning conditions (deprecated feature usage)
- error: Error conditions (operation failures)
- critical: Critical conditions (system component failures)
- alert: Action must be taken immediately (data corruption detected)
- emergency: System is unusable (complete system failure)"
  :group 'mcp
  :type '(choice (const :tag "debug" debug)
          (const :tag "info" info)
          (const :tag "notice" notice)
          (const :tag "warning" warning)
          (const :tag "error" error)
          (const :tag "critical" critical)
          (const :tag "alert" alert)
          (const :tag "emergency" emergency)))

(defclass mcp-process-connection (jsonrpc-process-connection)
  ((connection-type
    :initarg :connection-type
    :accessor mcp--connection-type)
   (-status
    :initform 'init
    :accessor mcp--status)
   (-capabilities
    :initform nil
    :accessor mcp--capabilities)
   (-serverinfo
    :initform nil
    :accessor mcp--server-info)
   (-prompts
    :initform nil
    :accessor mcp--prompts)
   (-tools
    :initform nil
    :accessor mcp--tools)
   (-resources
    :initform nil
    :accessor mcp--resources))
  :documentation "A MCP connection over an Emacs process.")

(defclass mcp-sse-process-connection (mcp-process-connection)
  ((-host
    :initarg :host
    :accessor mcp--host)
   (-port
    :initarg :port
    :accessor mcp--port)
   (-tls
    :initarg :tls
    :accessor mcp--tls)
   (-endpoint
    :initform nil
    :accessor mcp--endpoint))
  :documentation "A sse MCP connection over an Emacs process.")

(defclass mcp-stdio-process-connection (mcp-process-connection)
  ()
  :documentation "A stdio MCP connection over an Emacs process.")

(cl-defmethod initialize-instance :after ((_ mcp-process-connection) slots)
  "Init mcp process connection."
  (cl-destructuring-bind (&key ((:process proc)) &allow-other-keys) slots
    (set-process-filter proc #'mcp--process-filter)))

(cl-defmethod jsonrpc-connection-send ((connection mcp-process-connection)
                                       &rest args
                                       &key
                                       id
                                       method
                                       _params
                                       (_result nil result-supplied-p)
                                       error
                                       _partial)
  "Send JSON-RPC message to CONNECTION.
CONNECTION is an MCP process connection instance. ARGS is a plist
containing the message components:

METHOD - Method name (string, symbol or keyword)
PARAMS - Parameters for the method (optional)
ID     - Request ID (optional)
RESULT - Response result (for replies)
error   - Error object (for error replies)
partial - Partial response flag (optional)

For requests, both :method and :id should be provided.
For notifications, only :method is required.
For replies, either :_result or :error should be provided.

The message is sent differently based on connection type:
- SSE connections use HTTP POST requests
- Stdio connections write directly to the process"
  (when method
    ;; sanitize method into a string
    (setq args
          (plist-put args :method
                     (cond ((keywordp method) (substring (symbol-name method) 1))
                           ((symbolp method) (symbol-name method))
                           ((stringp method) method)
                           (t (error "[jsonrpc] invalid method %s" method))))))
  (let* ((kind (cond ((or result-supplied-p error) 'reply)
                     (id 'request)
                     (method 'notification)))
         (converted (jsonrpc-convert-to-endpoint connection args kind))
         (json (jsonrpc--json-encode converted)))
    (pcase (mcp--connection-type connection)
      ('sse
       (let ((url-request-method "POST")
             (url-request-extra-headers
              '(("Content-Type" . "application/json")))
             (url-request-data (encode-coding-string
                                json
                                'utf-8))
             (url (format "%s://%s:%s%s"
                          (if (mcp--tls connection) "https" "http")
                          (mcp--host connection)
                          (mcp--port connection)
                          (mcp--endpoint connection))))
         (url-retrieve url
                       #'(lambda (_)
                           (when (buffer-live-p (current-buffer))
                             (goto-char (point-min))
                             ;; (when (search-forward "\n\n" nil t)
                             ;;   (let* ((headers (buffer-substring (point-min) (point)))
                             ;;          (body (buffer-substring (point) (point-max)))
                             ;;          (response-code (string-match "HTTP/.* \\([0-9]+\\)" headers)))))
                             (kill-buffer))))))
      ('stdio
       (process-send-string
        (jsonrpc--process connection)
        (format "%s\r\n" json))))
    (jsonrpc--event
     connection
     'client
     :json json
     :kind  kind
     :message args
     :foreign-message converted)))

(defvar mcp--in-process-filter nil
  "Non-nil if inside `mcp--process-filter'.")

(cl-defun mcp--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when mcp--in-process-filter
    ;; Problematic recursive process filters may happen if
    ;; `jsonrpc-connection-receive', called by us, eventually calls
    ;; client code which calls `process-send-string' (which see) to,
    ;; say send a follow-up message.  If that happens to writes enough
    ;; bytes for pending output to be received, we will lose JSONRPC
    ;; messages.  In that case, remove recursiveness by re-scheduling
    ;; ourselves to run from within a timer as soon as possible
    ;; (bug#60088)
    (run-at-time 0 nil #'mcp--process-filter proc string)
    (cl-return-from mcp--process-filter))
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((conn (process-get proc 'jsonrpc-connection))
             (queue (or (process-get proc 'jsonrpc-mqueue) nil))
             (buf (or (process-get proc 'jsonrpc-pending)
                      (plist-get (process-put
                                  proc 'jsonrpc-pending
                                  (generate-new-buffer " *mcp-jsonrpc-pending*"))
                                 'jsonrpc-pending)))
             (data (with-current-buffer buf
                     (goto-char (point-max))
                     (insert string)
                     (buffer-string)))
             (type (mcp--connection-type conn))
             (parsed-messages nil)
             (lines (split-string data "\n"))
             (parsed-index 0)
             (endpoint-waitp nil)
             (line-index 0))
        (dolist (line lines)
          (pcase type
            ('sse
             (cond
              ((and (<= (+ line-index 1) (length lines))
                    (string-prefix-p "event:" (elt lines (+ line-index 1)))))
              ((string-prefix-p "event: endpoint" line)
               (setq endpoint-waitp t))
              ((string-prefix-p "data: " line)
               (let ((json-str (if (and endpoint-waitp
                                        (string-match "http://[^/]+\\(/[^[:space:]]+\\)" line))
                                   (match-string 1 line)
                                 (string-trim (substring line 6)))))
                 (unless (string-empty-p json-str)
                   (if endpoint-waitp
                       (setf (mcp--endpoint conn) json-str)
                     (push (cons parsed-index json-str) parsed-messages)
                     (cl-incf parsed-index)))))
              ((and (mcp--endpoint conn)
                    (not (or (string-prefix-p "2d" line)
                             (string-prefix-p ": ping" line)
                             (string-prefix-p "event: message" line)))
                    (not (with-current-buffer buf (= (point-min) (point-max)))))
               (let ((json-str (string-trim line)))
                 (unless (string-empty-p json-str)
                   (push (cons parsed-index json-str) parsed-messages)
                   (cl-incf parsed-index))))))
            ('stdio
             (let ((json-str (string-trim line)))
               (unless (string-empty-p json-str)
                 (push (cons parsed-index json-str) parsed-messages)
                 (cl-incf parsed-index)))))
          (cl-incf line-index))
        (setq parsed-messages (nreverse parsed-messages))

        (with-current-buffer buf (erase-buffer))
        ;; Add messages to MQUEUE
        (dolist (msg parsed-messages)
          (pcase-let ((`(,_index . ,json-str) msg))
            (let ((json nil)
                  (json-str (with-current-buffer buf
                              (if (= (point-min) (point-max))
                                  json-str
                                (goto-char (point-max))
                                (insert json-str)
                                (buffer-string)))))
              (condition-case-unless-debug err
                  (setq json (json-parse-string json-str
                                                :object-type 'plist
                                                :null-object nil
                                                :false-object :json-false))
                (json-parse-error
                 ;; parse error and not because of incomplete json
                 (jsonrpc--warn "Invalid JSON: %s\t %s" (cdr err) json-str))
                (json-end-of-file
                 ;; Save remaining data to pending for next processing
                 (with-current-buffer buf
                   (goto-char (point-max))
                   (insert json-str)
                   (process-put proc 'jsonrpc-pending buf))))
              (when json
                (with-current-buffer buf (erase-buffer))
                (when (listp json)
                  (setq json (plist-put json :jsonrpc-json json-str))
                  (push json queue))))))

        ;; Save updated queue
        (process-put proc 'jsonrpc-mqueue queue)

        ;; Dispatch messages in timer
        (cl-loop with time = (current-time)
                 for msg = (pop queue) while msg
                 do (let ((timer (timer-create)))
                      (timer-set-time timer time)
                      (timer-set-function timer
                                          (lambda (conn msg)
                                            (with-temp-buffer
                                              (jsonrpc-connection-receive conn msg)))
                                          (list conn msg))
                      (timer-activate timer)))

        ;; Save final queue (might have been consumed by timer pop)
        (process-put proc 'jsonrpc-mqueue queue)))))

(defun mcp--sse-connect (process host port path)
  "Establish SSE connection to server.
PROCESS is the network process object. HOST and PORT specify the
server address. PATH is the endpoint path for SSE connection.
Sends HTTP GET request with SSE headers to initiate the event
stream connection. Used internally by MCP for SSE-based JSON-RPC
communication."
  (process-send-string process
                       (concat
                        (format "GET %s HTTP/1.1\r\n"
                                path)
                        (format "Host: %s:%s\r\n"
                                host
                                port)
                        "Accept: text/event-stream\r\n"
                        "Cache-Control: no-cache\r\n"
                        "Connection: keep-alive\r\n\r\n")))

(cl-defun mcp-notify (connection method &optional (params nil))
  "Send notification to CONNECTION without expecting response.
METHOD is the notification name (string or symbol). PARAMS is an
optional plist of parameters.
This is a thin wrapper around =jsonrpc-connection-send' that
omits the :id parameter to indicate it's a notification rather
than a request."
  (apply #'jsonrpc-connection-send
         `(,connection
           :method ,method
           ,@(when params
               (list :params params)))))

(defvar mcp-server-connections (make-hash-table :test #'equal)
  "Mcp server process.")

(defun mcp-request-dispatcher (name method params)
  "Default handler for MCP server requests.
NAME identifies the server connection. METHOD is the requested
method name. PARAMS contains the method parameters.

This basic implementation just logs the request. Applications
should override this to implement actual request handling."
  (message "%s Received request: method=%s, params=%s" name method params))

(defun mcp-notification-dispatcher (connection name method params)
  "Handle notifications from MCP server.
CONNECTION is the JSON-RPC connection object. NAME identifies the
server. METHOD is the notification name. PARAMS contains the
notification data."
  (pcase method
    ('notifications/message
     (cond ((or (plist-member (mcp--capabilities connection) :logging)
                (and (plist-member params :level)
                     (plist-member params :data)))
            (cl-destructuring-bind (&key level data &allow-other-keys) params
              (let ((logger (plist-get params :logger)))
                (message "[mcp][%s][%s]%s: %s"
                         name
                         level
                         (if logger
                             (format "[%s]" logger)
                           "")
                         data))))))
    (_
     (message "%s Received notification: method=%s, params=%s" name method params))))

(defun mcp-on-shutdown (name)
  "When NAME mcp server shutdown."
  (message "%s connection shutdown" name))

(defun mcp--parse-http-url (url)
  "Parse HTTP/HTTPS URL into connection components.
URL should be a string in format http(s)://host[:port][/path].

Returns a plist with connection parameters:
:tls   - Boolean indicating HTTPS (t) or HTTP (nil)
:host  - Server hostname (string)
:port  - Port number (integer, defaults to 80/443)
:path  - URL path component (string)

Returns nil if URL is invalid or not HTTP/HTTPS."
  (when-let* ((url (url-generic-parse-url url))
              (type (url-type url))
              (host (url-host url))
              (filename (url-filename url)))
    (when (or (string= type "http")
              (string= type "https"))
      (let ((port (url-port url))
            (tls (string= "https" type)))
        (list :tls tls
              :host host
              :port (if port
                        port
                      (if tls
                          443
                        80))
              :path filename)))))

;;;###autoload
(cl-defun mcp-connect-server (name &key command args url env initial-callback
                                   tools-callback prompts-callback
                                   resources-callback error-callback)
  "Connect to an MCP server with NAME, COMMAND, and ARGS or URL.

NAME is a string representing the name of the server.
COMMAND is a string representing the command to start the server
in stdio mcp server.
ARGS is a list of arguments to pass to the COMMAND.
URL is a string arguments to connect sse mcp server.
ENV is a plist argument to set mcp server env.

INITIAL-CALLBACK is a function called when the server completes
the connection.
TOOLS-CALLBACK is a function called to handle the list of tools
provided by the server.
PROMPTS-CALLBACK is a function called to handle the list of prompts
provided by the server.
RESOURCES-CALLBACK is a function called to handle the list of
resources provided by the server.
ERROR-CALLBACK is a function to call on error.

This function creates a new process for the server, initializes a connection,
and sends an initialization message to the server. The connection is stored
in the `mcp-server-connections` hash table for future reference."
  (unless (gethash name mcp-server-connections)
    (when-let* ((server-config (cond (command
                                      (list :connection-type 'stdio
                                            :command command
                                            :args args))
                                     (url
                                      (when-let* ((res (mcp--parse-http-url url)))
                                        (plist-put res
                                                   :connection-type 'sse)))))
                (connection-type (plist-get server-config :connection-type))
                (buffer-name (format "*Mcp %s server*" name))
                (process-name (format "mcp-%s-server" name))
                (process (pcase connection-type
                           ('sse
                            (get-buffer-create buffer-name)
                            (open-network-stream process-name
                                                 buffer-name
                                                 (plist-get server-config :host)
                                                 (plist-get server-config :port)
                                                 :type (if (plist-get server-config :tls)
                                                           'tls
                                                         'network)))
                           ('stdio
                            (let ((env (mapcar #'(lambda (item)
                                                   (pcase-let* ((`(,key ,value) item))
                                                     (let ((key (symbol-name key)))
                                                       (list (substring key 1)
                                                             (format "%s" value)))))
                                               (seq-partition env 2)))
                                  (process-environment (copy-sequence process-environment)))
                              (when env
                                (dolist (elem env)
                                  (setenv (car elem) (cadr elem))))
                              (make-process
                               :name name
                               :command (append (list command)
                                                (plist-get server-config :args))
                               :connection-type 'pipe
                               :coding 'utf-8-emacs-unix
                               ;; :noquery t
                               :stderr (get-buffer-create
                                        (format "*%s stderr*" name))
                               ;; :file-handler t
                               ))))))
      (when (equal connection-type 'sse)
        (mcp--sse-connect process
                          (plist-get server-config :host)
                          (plist-get server-config :port)
                          (plist-get server-config :path)))
      (let ((connection (apply #'make-instance
                               `(,(pcase connection-type
                                    ('sse
                                     'mcp-sse-process-connection)
                                    ('stdio
                                     'mcp-stdio-process-connection))
                                 :connection-type ,connection-type
                                 :name ,name
                                 :process ,process
                                 :request-dispatcher ,(lambda (_ method params)
                                                        (funcall #'mcp-request-dispatcher name method params))
                                 :notification-dispatcher ,(lambda (connection method params)
                                                             (funcall #'mcp-notification-dispatcher connection name method params))
                                 :on-shutdown ,(lambda (_)
                                                 (funcall #'mcp-on-shutdown name))
                                 ,@(when (equal connection-type 'sse)
                                     (list :host (plist-get server-config :host)
                                           :port (plist-get server-config :port)
                                           :tls (plist-get server-config :tls))))))
            (initial-use-time 0)
            (initial-timer nil))
        ;; Initialize connection
        (puthash name connection mcp-server-connections)
        (when (equal connection-type 'sse)
          (setf (mcp--status connection)
                'waitendpoint))
        ;; Send the Initialize message
        (setf initial-timer
              (run-with-idle-timer
               1
               t
               #'(lambda ()
                   (cl-incf initial-use-time)
                   (if (jsonrpc-running-p connection)
                       (when (or (equal connection-type 'stdio)
                                 (and (equal connection-type 'sse)
                                      (mcp--endpoint connection)))
                         (cancel-timer initial-timer)
                         (mcp-async-initialize-message
                          connection
                          #'(lambda (protocolVersion serverInfo capabilities)
                              (if (string= protocolVersion *MCP-VERSION*)
                                  (progn
                                    (message "[mcp] Connected! Server `MCP (%s)' now managing." (jsonrpc-name connection))
                                    (setf (mcp--capabilities connection) capabilities
                                          (mcp--server-info connection) serverInfo)
                                    ;; Notify server initialized
                                    (mcp-notify connection
                                                :notifications/initialized)
                                    ;; handle logging
                                    (when (plist-member capabilities :logging)
                                      (mcp-async-set-log-level connection mcp-log-level))
                                    (when initial-callback
                                      (funcall initial-callback connection))
                                    (run-with-idle-timer mcp-server-wait-initial-time
                                                         nil
                                                         #'(lambda ()
                                                             ;; Get prompts
                                                             (when (plist-member capabilities :prompts)
                                                               (mcp-async-list-prompts connection prompts-callback))
                                                             ;; Get tools
                                                             (when (plist-member capabilities :tools)
                                                               (mcp-async-list-tools connection tools-callback))
                                                             ;; Get resources
                                                             (when (plist-member capabilities :resources)
                                                               (mcp-async-list-resources connection resources-callback)))
                                                         )
                                    (setf (mcp--status connection)
                                          'connected))
                                (progn
                                  (message "[mcp] Error %s server protocolVersion(%s) not support, client Version: %s."
                                           (jsonrpc-name connection)
                                           protocolVersion
                                           *MCP-VERSION*)
                                  (mcp-stop-server (jsonrpc-name connection)))))
                          #'(lambda (code message)
                              (when error-callback
                                (funcall error-callback code message))
                              (setf (mcp--status connection)
                                    'error)
                              (message "Sadly, mpc server reports %s: %s"
                                       code message)))
                         (when (> initial-use-time mcp-server-start-time)
                           (mcp-stop-server name)
                           (cancel-timer initial-timer)
                           (message "Sadly: mcp server start error timeout")))
                     (cancel-timer initial-timer)
                     (when error-callback
                       (funcall error-callback -1 "mcp server process start error")
                       (setf (mcp--status connection)
                             'error)
                       (message "Sadly, %s mcp server process start error" name))))))))))

;;;###autoload
(defun mcp-stop-server (name)
  "Stop the MCP server with the given NAME.
If the server is running, it will be shutdown and its connection will be removed
from `mcp-server-connections'. If no server with the given NAME is found,
a message will be displayed indicating that the server is not running."
  (if-let* ((connection (gethash name mcp-server-connections)))
      (progn
        (jsonrpc-shutdown connection)
        (setf (gethash name mcp-server-connections) nil))
    (message "mcp %s server not started" name)))

(defun mcp--parse-tool-args (properties required)
  "Parse tool arguments from PROPERTIES and REQUIRED lists.

PROPERTIES is a plist of tool argument properties.
REQUIRED is a list of required argument names.

The function processes each argument in PROPERTIES, marking optional arguments
if they are not in REQUIRED. Each argument is parsed into a structured plist
with :name, :type, and :optional fields.

Returns a list of parsed argument plists."
  (let ((need-length (- (/ (length properties) 2)
                        (length required))))
    (cl-mapcar #'(lambda (arg-value required-name)
                   (pcase-let* ((`(,key ,value) arg-value))
                     `( :name ,(substring (symbol-name key) 1)
                        ,@value
                        ,@(unless required-name
                            `(:optional t)))))
               (seq-partition properties 2)
               (append required
                       (when (> need-length 0)
                         (make-list need-length nil))))))


(defun mcp--parse-tool-call-result (res)
  "Parse the result of a tool call from RES.

RES is a plist representing the tool call result.

The function extracts text content from the result, concatenating it into
a single string if multiple text entries are present.

Returns the concatenated text or nil if no text content is found."
  (string-join
   (cl-remove-if #'null
                 (mapcar #'(lambda (content)
                             (when (string= "text" (plist-get content :type))
                               (plist-get content :text)))
                         (plist-get res :content)))
   "\n"))

(defun mcp--generate-tool-call-args (args properties)
  "Generate tool call arguments from ARGS and PROPERTIES.

ARGS is a list of argument values provided by the caller.
PROPERTIES is a plist of tool argument properties.

The function matches ARGS to PROPERTIES, filling in default values for missing
optional arguments. It ensures the generated arguments match the tool's schema.

Returns a plist of argument names and values ready for tool invocation."
  (let ((need-length (- (/ (length properties) 2)
                        (length args))))
    (apply #'append
           (cl-mapcar #'(lambda (arg value)
                          (when-let* ((value (if value
                                                 value
                                               (plist-get (cl-second arg)
                                                          :default))))
                            (list (cl-first arg)
                                  value)))
                      (seq-partition properties 2)
                      (append args
                              (when (> need-length 0)
                                (make-list need-length nil)))))))

;;;###autoload
(defun mcp-make-text-tool (name tool-name &optional asyncp)
  "Create a `gptel' tool with the given NAME, TOOL-NAME, and ASYNCP.

NAME is the name of the server connection.
TOOL-NAME is the name of the tool to be created.

Currently, only synchronous messages are supported.

This function retrieves the tool definition from the server connection,
constructs a basic tool with the appropriate properties, and returns it.
The tool is configured to handle input arguments, call the server, and process
the response to extract and return text content."
  (when-let* ((connection (gethash name mcp-server-connections))
              (tools (mcp--tools connection))
              (tool (cl-find tool-name tools :test #'equal :key #'(lambda (tool) (plist-get tool :name)))))
    (cl-destructuring-bind (&key description ((:inputSchema input-schema)) &allow-other-keys) tool
      (cl-destructuring-bind (&key properties required &allow-other-keys) input-schema
        (list
         :function (if asyncp
                       #'(lambda (callback &rest args)
                           (when (< (length args) (length required))
                             (error "Error: args not match: %s -> %s" required args))
                           (if-let* ((connection (gethash name mcp-server-connections)))
                               (mcp-async-call-tool connection
                                                    tool-name
                                                    (mcp--generate-tool-call-args args properties)
                                                    #'(lambda (res)
                                                        (funcall callback
                                                                 (mcp--parse-tool-call-result res)))
                                                    #'(lambda (code message)
                                                        (funcall callback
                                                                 (format "call %s tool error with %s: %s"
                                                                         tool-name
                                                                         code
                                                                         message))))
                             (error "Error: %s server not connect" name)))
                     #'(lambda (&rest args)
                         (when (< (length args) (length required))
                           (error "Error: args not match: %s -> %s" required args))
                         (if-let* ((connection (gethash name mcp-server-connections)))
                             (if-let* ((res (mcp-call-tool connection
                                                           tool-name
                                                           (mcp--generate-tool-call-args args properties))))
                                 (mcp--parse-tool-call-result res)
                               (error "Error: call %s tool error" tool-name))
                           (error "Error: %s server not connect" name))))
         :name tool-name
         :async asyncp
         :description description
         :args
         (mcp--parse-tool-args properties (or required '())))))))

(defun mcp-async-set-log-level (connection log-level)
  "Asynchronously set the log level for the MCP server.

CONNECTION is the MCP connection object.
LOG-LEVEL is the desired log level, which must be one of:
- `debug': Detailed debugging information (function entry/exit points)
- `info': General informational messages (operation progress updates)
- `notice': Normal but significant events (configuration changes)
- `warning': Warning conditions (deprecated feature usage)
- `error': Error conditions (operation failures)
- `critical': Critical conditions (system component failures)
- `alert': Action must be taken immediately (data corruption detected)
- `emergency': System is unusable (complete system failure)

On success, displays a message confirming the log level change.
On error, displays an error message with the server's response code and message."
  (jsonrpc-async-request connection
                         :logging/setLevel
                         (list :level (format "%s" log-level))
                         :success-fn
                         #'(lambda (res)
                             (message "[mcp] setLevel success: %s" res))
                         :error-fn (jsonrpc-lambda (&key code message _data)
                                     (message "Sadly, mpc server reports %s: %s"
                                              code message))))

(defun mcp-async-ping (connection)
  "Send an asynchronous ping request to the MCP server via CONNECTION.

The function uses `jsonrpc-async-request' to send a ping request.
On success, it displays a message with the response.
On error, it displays an error message with the code from the server."
  (jsonrpc-async-request connection
                         :ping
                         nil
                         :success-fn
                         #'(lambda (res)
                             (message "[mcp] ping success: %s" res))
                         :error-fn (jsonrpc-lambda (&key code message _data)
                                     (message "Sadly, mpc server reports %s: %s"
                                              code message))))

(defun mcp-async-initialize-message (connection callback &optional error-callback)
  "Sending an `initialize' request to the CONNECTION.

CONNECTION is the MCP connection object.
CALLBACK is a function to call upon successful initialization.
ERROR-CALLBACK is an optional function to call if an error occurs.

This function sends an `initialize' request to the server
with the client's capabilities and version information."
  (jsonrpc-async-request connection
                         :initialize
                         (list :protocolVersion "2024-11-05"
                               :capabilities '(:roots (:listChanged t))
                               :clientInfo '(:name "mcp-emacs" :version "0.1.0"))
                         :success-fn
                         #'(lambda (res)
                             (cl-destructuring-bind (&key protocolVersion serverInfo capabilities &allow-other-keys) res
                               (funcall callback protocolVersion serverInfo capabilities)))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (if error-callback
                               (funcall error-callback code message)
                             (message "Sadly, mpc server reports %s: %s"
                                      code message)))))

(defun mcp-async-list-tools (connection &optional callback error-callback)
  "Get a list of tools from the MCP server using the provided CONNECTION.

CONNECTION is the MCP connection object.
CALLBACK is a function to call with the result of the request.
ERROR-CALLBACK is an optional function to call if the request fails.

This function sends a request to the server to list available tools.
The result is stored in the `mcp--tools' slot of the CONNECTION object."
  (jsonrpc-async-request connection
                         :tools/list
                         '(:cursor "")
                         :success-fn
                         #'(lambda (res)
                             (cl-destructuring-bind (&key tools &allow-other-keys) res
                               (setf (mcp--tools connection)
                                     tools)
                               (when callback
                                 (funcall callback connection tools))))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (if error-callback
                               (funcall error-callback code message)
                             (message "Sadly, mpc server reports %s: %s"
                                      code message)))))

(defun mcp-call-tool (connection name arguments)
  "Call a tool on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the tool to call.
ARGGUMENTS is a list of arguments to pass to the tool."
  (jsonrpc-request connection
                   :tools/call
                   (list :name name
                         :arguments (if arguments
                                        arguments
                                      #s(hash-table)))))

(defun mcp-async-call-tool (connection name arguments callback error-callback)
  "Async Call a tool on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the tool to call.
ARGUMENTS is a list of arguments to pass to the tool.
CALLBACK is a function to call on success.
ERROR-CALLBACK is a function to call on error."
  (jsonrpc-async-request connection
                         :tools/call
                         (list :name name
                               :arguments (if arguments
                                              arguments
                                            #s(hash-table)))
                         :success-fn
                         #'(lambda (res)
                             (funcall callback res))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (funcall error-callback code message))))

(defun mcp-async-list-prompts (connection &optional callback error-callback)
  "Get list of prompts from the MCP server using the provided CONNECTION.

CONNECTION is the MCP connection object. CALLBACK is an optional function to
call on success,which will receive the CONNECTION and the list of prompts.
ERROR-CALLBACK is an optional function to call on error, which will receive the
error code and message.

The result is stored in the `mcp--prompts' slot of the CONNECTION object."
  (jsonrpc-async-request connection
                         :prompts/list
                         '(:cursor "")
                         :success-fn
                         #'(lambda (res)
                             (cl-destructuring-bind (&key prompts &allow-other-keys) res
                               (setf (mcp--prompts connection)
                                     prompts)
                               (when callback
                                 (funcall callback connection prompts))))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (if error-callback
                               (funcall error-callback code message)
                             (message "Sadly, mpc server reports %s: %s"
                                      code message)))))

(defun mcp-get-prompt (connection name arguments)
  "Call a prompt on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the prompt to call.
ARGGUMENTS is a list of arguments to pass to the prompt"
  (jsonrpc-request connection
                   :prompts/get
                   (list :name name
                         :arguments (if arguments
                                        arguments
                                      #s(hash-table)))))

(defun mcp-async-get-prompt (connection name arguments callback error-callback)
  "Async Call a prompt on the remote CONNECTION with NAME and ARGUMENTS.

CONNECTION is the MCP connection object.
NAME is the name of the prompt to call.
ARGUMENTS is a list of arguments to pass to the prompt.
CALLBACK is a function to call on successful response.
ERROR-CALLBACK is a function to call on error."
  (jsonrpc-async-request connection
                         :prompts/get
                         (list :name name
                               :arguments (if arguments
                                              arguments
                                            #s(hash-table)))
                         :success-fn
                         #'(lambda (res)
                             (funcall callback res))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (funcall error-callback code message))))

(defun mcp-async-list-resources (connection &optional callback error-callback)
  "Get list of resources from the MCP server using the provided CONNECTION.

CONNECTION is the MCP connection object. CALLBACK is an optional function to
call upon successful retrieval of resources. ERROR-CALLBACK is an optional
function to call if an error occurs during the request.

The result is stored in the `mcp--resources' slot of the CONNECTION object."
  (jsonrpc-async-request connection
                         :resources/list
                         '(:cursor "")
                         :success-fn
                         #'(lambda (res)
                             (cl-destructuring-bind (&key resources &allow-other-keys) res
                               (setf (mcp--resources connection)
                                     resources)
                               (when callback
                                 (funcall callback connection resources))))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (if error-callback
                               (funcall error-callback code message)
                             (message "Sadly, mpc server reports %s: %s"
                                      code message)))))
(defun mcp-read-resource (connection uri)
  "Call a resource on the remote CONNECTION with URI.

CONNECTION is the MCP connection object.
URI is the uri of the resource to call."
  (jsonrpc-request connection
                   :resources/read
                   (list :uri uri)))

(defun mcp-async-read-resource (connection uri &optional callback error-callback)
  "Call a resource on the remote CONNECTION with URI.

CONNECTION is the MCP connection object.
URI is the URI of the resource to call.
CALLBACK is a function to call with the result on success.
ERROR-CALLBACK is a function to call with the error code and message on failure.

This function asynchronously reads a resource from the remote connection
using the specified URI. The result is passed to CALLBACK if the request
succeeds, or ERROR-CALLBACK if it fails."
  (jsonrpc-async-request connection
                         :resources/read
                         (list :uri uri)
                         :success-fn
                         #'(lambda (res)
                             (funcall callback res))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (funcall error-callback code message))))

(defun mcp-async-list-resource-templates (connection &optional callback error-callback)
  "Get list of resource templates from the MCP server using the CONNECTION.

CONNECTION is the MCP connection object. CALLBACK is an optional function to
call upon successful retrieval of resources. ERROR-CALLBACK is an optional
function to call if an error occurs during the request."
  (jsonrpc-async-request connection
                         :resources/templates/list
                         '(:cursor "")
                         :success-fn
                         #'(lambda (res)
                             (cl-destructuring-bind (&key resourceTemplates &allow-other-keys) res
                               (when callback
                                 (funcall callback connection resourceTemplates))))
                         :error-fn
                         (jsonrpc-lambda (&key code message _data)
                           (if error-callback
                               (funcall error-callback code message)
                             (message "Sadly, mpc server reports %s: %s"
                                      code message)))))

(provide 'mcp)
;;; mcp.el ends here
