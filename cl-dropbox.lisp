;;;; cl-dropbox.lisp

(in-package #:cl-dropbox)

;;; "cl-dropbox" goes here. Hacks and glory await!

(defparameter *api-root* "https://api.dropbox.com/1")
(defparameter *consumer-token* nil)

(defparameter *get-request-token-endpoint* (concatenate 'string *api-root* "/oauth/request_token"))
(defparameter *auth-request-token-endpoint* "https://www.dropbox.com/1/oauth/authorize")
(defparameter *get-access-token-endpoint* (concatenate 'string *api-root* "/oauth/access_token"))

(defparameter *account-info-uri* (concatenate 'string *api-root* "/account/info"))
(defparameter *metadata-uri* (concatenate 'string *api-root* "/metadata/dropbox"))
(defparameter *create-folder-uri* (concatenate 'string *api-root* "/fileops/create_folder"))
(defparameter *delete-uri* (concatenate 'string *api-root* "/fileops/delete"))

(defvar *request-token* nil)
(defvar *access-token* nil)

(defun set-credentials (&key key secret)
  (setf *consumer-token* (cl-oauth:make-consumer-token :key key :secret secret)))

(defun get-request-token ()
  (setf *request-token* (cl-oauth:obtain-request-token *get-request-token-endpoint* *consumer-token*)))

(defun get-access-token ()
  (assert *request-token*)
  (setf *access-token* (cl-oauth:obtain-access-token *get-access-token-endpoint* *request-token*)))

(defun authorize-app ()
  (assert *request-token*)
  (format t "~&Visit the below url to authorize your app then return and press enter.~% ~A~%"
          (cl-oauth:make-authorization-uri *auth-request-token-endpoint* *request-token*))
  (let ((result (read-line *standard-input*)))
    (when (zerop (length result))
      (cl-oauth:authorize-request-token *request-token*))))

(defun get-account-info (&optional (decode t))
  "Retrieves information about the user's account."
  (let ((result (cl-oauth:access-protected-resource *account-info-uri* *access-token*)))
    (if decode
      (json:decode-json-from-string result)
      result)))

(defun get-metadata (&key (path nil path-supplied-p) (decode t))
  "The metadata API location provides the ability to retrieve file and folder metadata and manipulate the directory structure by moving or deleting files and folders."
  (let ((merged-path (if path-supplied-p
                          (concatenate 'string *metadata-uri* path)
                          *metadata-uri*)))
    (multiple-value-bind (body code)
        (cl-oauth:access-protected-resource merged-path *access-token*)
      (case code
        (404 (values 404 "Specified file path not found."))
        (200 (if decode
                 (json:decode-json-from-string body)
                 body))))))

(defun create-folder (&key path (root "dropbox") (decode t))
  "Create a folder relative to the user's Dropbox root or the user's application sandbox folder."
  (let ((result (cl-oauth:access-protected-resource *create-folder-uri* *access-token*
                                                    :user-parameters `(("path" . ,path)
                                                                       ("root" . ,root)))))
    (if decode
        (json:decode-json-from-string result)
        result)))

(defun delete-content (&key path (root "dropbox") (decode t))
  "Deletes a file or folder."
  (multiple-value-bind (result code)
      (cl-oauth:access-protected-resource *delete-uri* *access-token*
                                          :user-parameters `(("path" . ,path)
                                                             ("root" . ,root)))
    (case code
      (404 (format t "Specified file path not found."))
      (400 (format t "Root parameter not Dropbox or sandbox. Operation attempted not allowed by token type. No path specified."))
      (200 (if decode
               (json:decode-json-from-string result)
               (values result code))))))