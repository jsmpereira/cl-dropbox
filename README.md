# Common Lisp Client for the Dropbox API.

Access your dropbox account through Common Lisp.

Go to https://www.dropbox.com/developers/apps and create an app to
get credentials.

## cl-oauth complains if the content type is not text.

```lisp
(push '("application" . "x-www-form-urlencoded") drakma:\_text-content-types*)

(ql:quickload :cl-dropbox)
(in-package :cl-dropbox)

(set-credentials :key "my-api-key" :secret "my-api-secret")
```

### Obtain request token

```lisp
(get-request-token)
```

### Authorize your app

```lisp
(authorize-app)
```

### Obtain access token

```lisp
(get-access-token)
```

### Use the API

```lisp
(get-account-info)
```

# TODO

- Implement missing API methods
- Code refactoring
