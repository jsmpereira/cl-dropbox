Common Lisp Client for the Dropbox API.

Access your dropbox account through Common Lisp.

Go to https://www.dropbox.com/developers/apps and create an app to
get credentials.

cl-oauth complains if the content type is not text.
(push '("application" . "x-www-form-urlencoded") drakma:*text-content-types*)

(ql:quickload :cl-dropbox)
(in-package :cl-dropbox)

(set-credentials :key "my-api-key" :secret "my-api-secret")

Obtain request token
(get-request-token)

Authorize your app
(authorize-app)

Obtain access token
(get-access-token)

Use the API
(get-account-info)


TODO

 - Implement missing API methods
 - Code refactoring
