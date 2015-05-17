;;;; cl-dropbox.lisp

(in-package #:cl-dropbox)

;;; "cl-dropbox" goes here. Hacks and glory await!

(defparameter *api-root* "https://api.dropbox.com/1")
(defparameter *api-content* "https://api-content.dropbox.com/1")
(defparameter *consumer-token* nil)

(defparameter *get-request-token-endpoint* (concatenate 'string *api-root* "/oauth/request_token"))
(defparameter *auth-request-token-endpoint* "https://www.dropbox.com/1/oauth/authorize")
(defparameter *get-access-token-endpoint* (concatenate 'string *api-root* "/oauth/access_token"))

;; /files api calls and /thumbnails call go to the api-content server.
(defparameter *files-uri* (concatenate 'string *api-content* "/files"))
(defparameter *thumbnails-uri* (concatenate 'string *api-content*"/thumbnails"))

(defparameter *account-info-uri* (concatenate 'string *api-root* "/account/info"))
(defparameter *metadata-uri* (concatenate 'string *api-root* "/metadata"))
(defparameter *revisions-uri* (concatenate 'string *api-root* "/revisions"))
(defparameter *restore-uri* (concatenate 'string *api-root* "/restore"))
(defparameter *search-uri* (concatenate 'string *api-root* "/search"))
(defparameter *shares-uri* (concatenate 'string *api-root* "/shares"))
(defparameter *media-uri* (concatenate 'string *api-root* "/media"))
(defparameter *create-folder-uri* (concatenate 'string *api-root* "/fileops/create_folder"))
(defparameter *delete-uri* (concatenate 'string *api-root* "/fileops/delete"))

(defvar *request-token* nil)
(defvar *access-token* nil)

;; Authentication

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

;; API calls

; Dropbox accounts
(defun get-account-info (&key (decode t))
  "Retrieves information about the user's account."
  (multiple-value-bind (body status)
      (cl-oauth:access-protected-resource *account-info-uri* *access-token*)
    (handle-response body status decode)))

; Files and Metadata
(defun get-file (&key path (root "/dropbox") (rev nil rev-supplied-p))
  "Downloads a file. Note that this call goes to the api-content server."
  (let ((parameter (when rev-supplied-p
                     `(("rev" . ,rev))))
        (merged-path (concatenate 'string *files-uri* root (encode-path path))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token* :request-method :get :user-parameters parameter :drakma-args `(:parameters ,parameter))
      (handle-response body status nil))))

(defun get-metadata (&key (path nil path-supplied-p) (root "/dropbox") (decode t))
  "Retrieves file and folder metadata."
  (let ((merged-path (if path-supplied-p
                          (concatenate 'string *metadata-uri* root (encode-path path))
                          (concatenate 'string *metadata-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token*)
      (handle-response body status decode))))

(defun get-revisions (&key (path nil path-supplied-p) (root "/dropbox") (decode t))
  "Obtains metadata for the previous revisions of a file."
  (let ((merged-path (if path-supplied-p
                         (concatenate 'string *revisions-uri* root (encode-path path))
                         (concatenate 'string *revisions-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token*)
      (handle-response body status))))

(defun restore (&key (path nil path-supplied-p) rev (root "/dropbox") (decode t))
  "Restores a file path to a previous revision."
  (let ((parameter `(("rev" . ,rev)))
        (merged-path (if path-supplied-p
                         (concatenate 'string *restore-uri* root (encode-path path))
                         (concatenate 'string *restore-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token* :request-method :auth :user-parameters parameter :drakma-args `(:parameters ,parameter))
      (handle-response body status))))

(defun do-search (&key (path nil path-supplied-p) query (root "/dropbox") (decode t))
  "Returns metadata for all files and folders that match the search query."
  (let ((parameter `(("query" . ,query)))
        (merged-path (if path-supplied-p
                         (concatenate 'string *search-uri* root (encode-path path))
                         (concatenate 'string *search-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token* :request-method :auth :user-parameters parameter :drakma-args `(:parameters ,parameter))
      (handle-response body status))))

(defun shares (&key (path nil path-supplied-p) (root "/dropbox") (decode t))
  "Creates and returns a shareable link to files or folders."
  (let ((merged-path (if path-supplied-p
                         (concatenate 'string *shares-uri* root (encode-path path))
                         (concatenate 'string *shares-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token*)
      (handle-response body status))))

(defun media (&key (path nil path-supplied-p) (root "/dropbox") (decode t))
  "Returns a link directly to a file."
  (let ((merged-path (if path-supplied-p
                         (concatenate 'string *media-uri* root (encode-path path))
                         (concatenate 'string *media-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token*)
      (handle-response body status))))

(defun thumbnails (&key (path nil path-supplied-p) (root "/dropbox") (decode nil))
  "Gets a thumbnail for an image. Note that this call goes to the api-content server."
  (let ((merged-path (if path-supplied-p
                         (concatenate 'string *thumbnails-uri* root "/" (encode-path path))
                         (concatenate 'string *thumbnails-uri* root))))
    (multiple-value-bind (body status)
        (cl-oauth:access-protected-resource merged-path *access-token* :request-method :auth)
      (handle-response body status decode))))

; File Operations
(defun create-folder (&key path (root "dropbox") (decode t))
  "Creates a folder."
  (multiple-value-bind (body status)
      (cl-oauth:access-protected-resource *create-folder-uri* *access-token*
                                          :user-parameters `(("path" . ,path)
                                                             ("root" . ,root)))
    (handle-response body status decode)))

(defun delete-content (&key path (root "dropbox") (decode t))
  "Deletes a file or folder."
  (multiple-value-bind (body status)
      (cl-oauth:access-protected-resource *delete-uri* *access-token*
                                          :user-parameters `(("path" . ,path)
                                                             ("root" . ,root)))
    (handle-response body status decode)))

; Convenience functions
(defun handle-response (body status &optional (decode t))
  (if (eql status 200)
      (if decode
          (json:decode-json-from-string body)
          body)
      (values (json:decode-json-from-string (flexi-streams:octets-to-string body)) status)))

(defun encode-path (path)
  (cl-ppcre:regex-replace-all "%2F" (cl-oauth:url-encode path) "/"))