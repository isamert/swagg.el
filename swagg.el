;;; swagg.el --- Swagger UI -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.4.0
;; Homepage: https://github.com/isamert/swagg.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0") (request "0.3.3") (dash "2.19.1") (yaml "0.5.1") (s "1.13.1"))
;; Keywords: tools,convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A practical Emacs UI for Swagger definitions.

;; swagg.el lets you fetch and interact with OpenAPI (formerly
;; Swagger) definitions.  You can think of it like the normal Swagger
;; UI that your app has but works for multiple definitions and it's
;; right inside your Emacs frame!

;;; Code:

(require 'request)
(require 'dash)
(require 'json)
(require 'yaml)
(require 'compat)
(require 'org)
(require 's)
(require 'pp)
(require 'map)
(require 'url-util)

;; Notes
;; Example Swagger JSON definitions
;; https://petstore.swagger.io/v2/swagger.json
;; https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json

;;;; Customization

(defgroup swagg nil
  "A practical swagger client for Emacs."
  :prefix "swagg-"
  :group 'utils)

;; TODO: support .servers property defined in the swagger file, like:
;; (servers
;;  ((url . "https://gitlab.com/api/")))
(defcustom swagg-definitions '()
  "A list of Swagger definitions, where each definition is a property list.
Each list may have one or more of the following keys:

- :name (required): Name of the API

- :base (required): Base URL of the API.

- :json OR :yaml (required): URL of the Swagger JSON or YAML
  file.  These can also be a function, rather than a string, that
  returns the URL.  The function takes one argument, which is the
  whole definition as a plist.

- :header (optional): An alist containing default header values.  If a
  request made to the API *contains* a header parameter named X, swagg
  first checks if the :header alist contains a value for X. If found,
  the value is used as the default value.  If not found, the user is
  prompted to enter the value for X. X being a hypothetical example
  here, works for any header keys.  The value can be a function, in that
  case that function will be executed with no parameters and the result
  will be used as the value.

- :header-all (optional): Header values to be included in every request.
  Unlike :header, which only includes the value if the Swagger
  definition specifies it, :header-all ensures the headers are included
  in every request. For instance, most Swagger definitions do not
  include the \"Authorization\" header due to security concerns; if you
  wish to add this header to each request, use :header-all.

- :query (optional): Like :header, but for query parameters.

- :query-all (optional): Like :header-all, but for query parameters.

- :path (optional): Like :header, but for path parameters.

- :any (optional): An alist containing default header, query or
  path parameter values.  Its structure is the same as :header or
  :query.  Any parameter found in the request is checked first in
  this alist.  If not found, prompts the user to enter a value.

Here's an example of a Swagger definition:

  (:name \"GitHub API\"
    :json \"https://raw.githubusercontent.com/github/rest-api-description\
/main/descriptions/api.github.com/api.github.com.json\"
    :base \"https://api.github.com\"
    :header (Authorization \"Bearer secret-token\"))

This defines an API with the name \"GitHub API\".  It uses the
Swagger JSON file located at the given URL as its documentation.
The base URL of this API is \"https://api.github.com\".  The
`:header` alist contains a default value for the
\"Authorization\" header parameter.  If a request is made to this
API and contains the \"Authorization\" header, the default value
of \"Bearer secret-token\" is used.  Otherwise, the user is
prompted to enter the value for this header parameter."
  :type 'list
  :group 'swagg)

(defcustom swagg-use-unique-buffer-per-request nil
  "Wheter to use a unique buffer name for each request.
This effects requests made with `swagg-request'.  You are, of
course, free to keep this as nil and call `rename-buffer' when
you want to keep the result of the request as a seperate buffer."
  :type 'boolean
  :group 'swagg)

(defcustom swagg-rest-block-prelude ""
  "Text to insert before the rest call inserted by `swagg-insert-rest-block'.
For example, you can set this to

  #+begin_src verb :op send\n

To wrap it into org babel block.  Also see `swagg-rest-block-postlude'."
  :type 'string
  :group 'swagg)

(defcustom swagg-rest-block-postlude ""
  "Text to insert after the rest call inserted by `swagg-insert-rest-block'.
For example, you can set this to

  \n#+end_src

To wrap it into org babel block.  Also see `swagg-rest-block-prelude'."
  :type 'string
  :group 'swagg)

(defcustom swagg-fetch-lang "javascript"
  "Babel code block language for inserted fetch calls.
For example, this can be changed into \"deno :allow net\" if you are
using the `ob-deno' package."
  :type 'string
  :group 'swagg)

(defcustom swagg-org-src-lang "verb"
  "Babel code block language for inserted verb style calls.

Used in `swagg-request-with-rest-src-block'.

By default, it's \"verb\"[^1] but you can change it to
\"restclient\"[^2] or ob-http[^3] if you are using one of those
packages.  You can also pass src block parameters too: \"http :pretty t
:select x.y.z\"

[1]: https://github.com/federicotdn/verb
[2]: https://github.com/pashky/restclient.el &
     https://github.com/alf/ob-restclient.el
[3]: https://github.com/zweifisch/ob-http
     https://github.com/ag91/ob-http"
  :type 'string
  :group 'swagg)

;; TODO: savehist integration?
(defcustom swagg-remember-inputs t
  "Whether to remember inputs for parameters you entered before.
When this is non-nil, any parameter you entered will be
remembered and will be presented as default value next time you
need to enter it.  This cache is kept only for the current
session.  Please see `swagg-definitions' to have a persistent
default value provider."
  :type 'string
  :group 'swagg)

(defcustom swagg-auto-accept-bound-values nil
  "Whether to automatically accept bound parameters.
If this is non-nil, parameters bound in :headers, :query or :any
in the `swagg-definitions' will be used for the request, without
asking any confirmation.  Otherwise swagg will use these values
as default but it will still ask you to confirm or edit this
default value first."
  :type 'boolean
  :group 'swagg)

(defcustom swagg-rest-block-org-header-tags nil
  "Tags to set for rest block org header.
This may be a tags string like \":aa:bb:cc:\", or a list of tags.
This is particularly useful if you use something like verb
package to do your requests, which requires \"verb\" tag to be
present in the header to be able to act on them."
  :type 'string
  :group 'swagg)

;;;; Internal

(defconst swagg--rest-buffer "*swagg-request*"
  "Name of the buffer where REST blocks are appended.")

(defvar swagg--result-buffer-name "*swagg-result*"
  "Default buffer name for displaying response data.")

(defvar swagg--headers-buffer-name "*swagg-headers*"
  "Default buffer name for displaying response headers.")

(defvar-local swagg--response nil
  "Current response associated with the results buffer.")

(defvar-local swagg--request nil
  "Current request associated with the results buffer.")

(defvar swagg--http-verbs '("GET" "POST" "PUT" "DELETE" "PATCH" "HEAD" "OPTIONS" "CONNECT" "TRACE")
  "List of all HTTP verbs.")

;; TODO: Persist cache between sessions?
(defvar swagg--json-cache '()
  "Cache for downloaded and parsed swagger definitions.
Set this to nil if you want invalidate the cache.")

;; TODO: savehist support?
(defvar swagg--read-string-cache '()
  "Cache for header/parameter inputs.
See `swagg-remember-inputs'.")

;;;; Helpers

(defvar swagg--dbg nil)

(defun swagg--dbg (msg &rest rest)
  "Append MSG with REST to the *swagg-log* buffer if variable `swagg--dbg' is non-nil."
  (when swagg--dbg
    (with-current-buffer (get-buffer-create "*swagg-log*")
      (goto-char (point-max))
      (insert (apply #'format "[MSG] %s" msg rest) "\n"))))

(defun swagg--tap (&rest rest)
  "Like `swagg-dbg' but the last of REST is returned as-is and logged to log buffer."
  (when swagg--dbg
    (with-current-buffer (get-buffer-create "*swagg-log*")
      (goto-char (point-max))
      (insert (apply #'format "  [TAP] %s" (-first-item (-butlast rest)) (-butlast rest))
              (format " → %s" (-last-item rest)) "\n")))
  (-last-item rest))

(defun swagg--alist-path-get (path alist)
  "Get the value associated with a specific PATH in ALIST.

>> (let ((alist `((a . ((b . ((c . d)))))))
         (path `(a b c)))
    (swagg--alist-path-get path alist))
=> d"
  (if (eq (length path) 1)
      (alist-get (car path) alist)
    (swagg--alist-path-get (seq-drop path 1) (alist-get (car path) alist))))

(cl-defun swagg--completing-read-object
    (prompt objects &key (formatter #'identity) category (sort? t) group def)
  "`completing-read' with formatter and sort control.
Applies FORMATTER to every object in OBJECTS and propertizes
candidates with the actual object so that they can be retrieved
later by embark actions.  Also adds category metadata to each
candidate, if given.  PROMPT passed to `completing-read' as is."
  (let* ((object-table
          (make-hash-table :test 'equal :size (length objects)))
         (object-strings
          (mapcar
           (lambda (object)
             (let ((formatted-object (funcall formatter object)))
               (puthash formatted-object object object-table)
               (propertize formatted-object 'swagg-item object)))
           objects))
         (selected
          (completing-read
           (format "%s " prompt)
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 `(metadata
                   ,(when category (cons 'category category))
                   ,(when group (cons 'group-function group))
                   ,@(unless sort?
                       '((display-sort-function . identity)
                         (cycle-sort-function . identity))))
               (complete-with-action
                action object-strings string predicate))))))
    (gethash selected object-table def)))

(defvar swagg--def nil)
(defmacro swagg--with-def (def &rest forms)
  "Perform FORMS with a given definition DEF.
Use `swagg--def' as an internal variable to hold DEF for the
duration of the forms.  This is useful for passing down
definition to functions called deep down.  Possibly a bad idea
but it works for now."
  (declare (indent 1))
  `(let ((swagg--def ,def))
     ,@forms))

(defun swagg--definition-parse-buffer (type)
  "Parse the contents of the buffer according to TYPE.
TYPE can be either :json or :yaml."
  (if (eq type :yaml)
      (yaml-parse-string
       (buffer-string)
       :sequence-type 'list
       :object-type 'alist)
    (json-parse-buffer
     :array-type 'list
     :object-type 'alist)))

;; TODO: Add granularity: '(definition name type)
(cl-defun swagg--read-string (prompt &key name type)
  "Like `read-string' but has swagg specific cache.
TYPE can be any symbol, possibly `:header', `:query' or `:path'.
NAME is the name of the query/header parameter that is cached.
PROMPT is passed to `read-string' as-is."
  (let* ((bound-value (alist-get
                       (intern name)
                       `(,@(plist-get swagg--def type)
                         ,@(plist-get swagg--def :any))))
         (bound (if (functionp bound-value)
                    (funcall bound-value)
                  bound-value))
         (cache-path (format "%s.%s.%s" (plist-get swagg--def :name) name type))
         (cache (alist-get cache-path swagg--read-string-cache nil nil #'equal))
         (result
          (if (and bound swagg-auto-accept-bound-values)
              bound
            (read-string prompt (or (when swagg-remember-inputs cache) bound)))))
    (setf (alist-get cache-path swagg--read-string-cache nil nil #'equal)
          (if (s-blank? result) nil result))
    result))

(defun swagg--indent (length str)
  "Indent STR with LENGTH amount of spaces."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (indent-rigidly (point) (point-max) length)
    (buffer-substring (point-min) (point-max))))

(defun swagg--security-header (scheme-def)
  "Convert OpenAPI security scheme definition to header components.
SCHEME-DEF is an alist representing the security scheme.
Returns a list of (HEADER-NAME FORMAT-STRING PLACEHOLDER)."
  (let-alist scheme-def
    (pcase .type
      ("http"
       (pcase .scheme
         ("bearer"
          '("Authorization" "Bearer %s" "<TOKEN>"))
         ("basic"
          '("Authorization" "Basic %s" "<BASE64_CREDENTIALS>"))
         (_ (list "Authorization"
                  (format "%s %%s" (capitalize .scheme))
                  "<CREDENTIALS>"))))
      ("apiKey"
       (pcase .in
         ("header"
          (list .name "%s" "<API_KEY>"))
         ("query"
          (list nil (format "%s=%%s" .name) "<API_KEY>"))
         ("cookie"
          (list "Cookie" (format "%s=%%s" .name) "<API_KEY>"))))
      ("oauth2"
       '("Authorization" "Bearer %s" "<OAUTH_TOKEN>"))
      ("openIdConnect"
       '("Authorization" "Bearer %s" "<ID_TOKEN>"))
      (_
       '("UNKNOWN_SECURITY_SCHEME" "%s" "?")))))

;;;; Core

(defun swagg--select-op (swagger)
  "Prompt the user to select an operation from the given SWAGGER spec."
  (swagg--completing-read-object
   "Select operation: "
   (--mapcat
    (-let (((path . ops) it))
      (--map
       (cons path it)
       (swagg--resolve-if-ref
        swagger
        ;; Filter out non-http verb fields (like description, see #5)
        (--filter
         (-contains? swagg--http-verbs (upcase (format "%s" (car it))))
         ops))))
    (alist-get 'paths swagger))
   :formatter
   (-lambda ((path . (op . info)))
     (s-trim
      (format "%-7s → %-50s %s"
              (propertize
               (upcase (symbol-name op)) 'face `(:foreground ,(pcase op
                                                                ('get "sky blue")
                                                                ('post "green")
                                                                ('delete "red")
                                                                ('patch "yellow")
                                                                (_ "white"))))
              path
              (propertize (or (alist-get 'summary info) "") 'face 'italic))))
   :group
   ;; TODO use swagger tag for grouping instead of path based grouping
   (lambda (cand transform)
     (if transform
         cand
       ;; Extract group from the formatted string CAND
       ;; TODO: handle /v1, /v2 etc. kind of paths. If it's a version
       ;; path, group by second path
       (or
        (-some->> cand
          (swagg--split-once "→")
          (nth 1)
          (funcall (lambda (it) (string-trim-left it "[ /]+")))
          (swagg--split-once "/")
          (car)
          (swagg--split-once " ")
          (car)
          (s-trim))
        "home")))))

(defun swagg--split-once (sep s)
  "Split string S into two substrings at the first occurrence of SEP.

>> (swagg--split-once \" \" \"hello cruel world\")
=> (\"hello\" \"cruel world\")"
  (s-split-up-to sep s 1 t))

;; NOTE: `not' keyword is not supported
;; See URL https://swagger.io/docs/specification/data-models/oneof-anyof-allof-not/
(defun swagg--build-schema (swagger schema &optional depth)
  "Build a JSON based on the provided SCHEMA.
SWAGGER is object representing the entire API."
  (unless depth
    (setq depth 2))
  (let* ((enum (alist-get 'enum schema))
         (type (alist-get 'type schema))
         ;; TODO Handle examples and descriptions
         ;; (example (alist-get 'example schema))
         (value (or
                 (when enum
                   (format "<%s>" (s-join "|" enum)))
                 (format "<%s>" type))))
    (pcase type
      ("object"
       (if-let* ((inner-indent (s-repeat depth " "))
                 (outer-indent (s-repeat (- depth 2) " "))
                 (properties (alist-get 'properties schema))
                 (props (s-join (format ", \n%s" inner-indent)
                                (-map
                                 (-lambda ((name . child))
                                   (format "\"%s\": %s" name (swagg--build-schema swagger child (+ 2 depth))))
                                 properties))))
           (format "{\n%s%s\n%s}" inner-indent props outer-indent)
         (swagg--build-schema
          swagger
          ;; If don't have .properties key in the schema, then it
          ;; means it's one of anyOf|allOf|oneOf|$ref. Removing 'type
          ;; from schema and calling `swagg--build-schema' again
          ;; enables us to handle this case
          (map-delete (map-copy schema) 'type)
          (+ 2 depth))))
      ("string" (s-wrap value "\""))
      ((or "number" "integer" "boolean" ) value)
      ("array" (format
                "[%s]"
                (swagg--build-schema swagger (alist-get 'items schema) depth)))
      (_ (let-alist schema
           (cond
            (.$ref (swagg--build-schema
                    swagger
                    (swagg--resolve-ref swagger .$ref)
                    depth))
            (.allOf (format "<%s>" (s-join "&" (--map (swagg--build-schema swagger it depth) .allOf))))
            (.anyOf (format "<%s>" (s-join "/" (--map (swagg--build-schema swagger it depth) .anyOf))))
            (.oneOf (format "<%s>" (s-join "|" (--map (swagg--build-schema swagger it depth) .oneOf))))
            (t "<?>")))))))

(defun swagg--resolve-ref (swagger ref)
  "Resolve a JSON reference in the given SWAGGER document.

Takes a SWAGGER document and a REF string (in the format
'#/path/to/reference'), and returns the referenced object in the
SWAGGER document."
  (swagg--alist-path-get
   (--map (intern it) (s-split "/" (string-trim-left ref "#/")))
   swagger))

(defun swagg--resolve-if-ref (swagger obj)
  "If OBJ is reference resolve it in SWAGGER and return.
Otherwise, return OBJ itself.  OBJ is a reference if it has a key
called '$ref'"
  (if-let (ref (alist-get '$ref obj))
      (swagg--resolve-ref swagger ref)
    obj))

(defun swagg--format-param-value (def value)
  "Return a formatted parameter value based on given DEF and VALUE."
  (let-alist def
    (if (equal .in "query")
        (format "%s=%s" .name (url-hexify-string value))
      value)))

(defun swagg--read-param-from-user (param-type def)
  "Request a value for DEF from user.
This handles formatting of the value properly (if it's an array
etc.), also there is some caching going on for the values
presented to user by default.  For that and for explanation of
PARAM-TYPE see `swagg--read-string'."
  (let-alist def
    (let ((prompt (format "Enter value for `%s' (%s, \"%s\"): "
                          .name (or .type .schema.type) .description)))
      (pcase .type
        ("array"
         (let ((results (completing-read-multiple prompt .items.enum)))
           (pcase .collectionFormat
             ("multi" (s-join "&" (--map (format "%s=%s" .name it) results)))
             ("ssv" (swagg--format-param-value def (s-join " " results)))
             ("tsv" (swagg--format-param-value def (s-join "\t" results)))
             ("pipes" (swagg--format-param-value def (s-join "|" results)))
             (_ (s-join "," results)))))
        (_
         (swagg--format-param-value def (swagg--read-string
                                         prompt
                                         :type param-type
                                         :name .name)))))))

(defun swagg--gen-body (swagger info)
  "Generate the request body for INFO using SWAGGER definition.
This uses \"requestBody.content.application/json\" or
first(parameters.in === \"body\") as the request body."
  (let-alist info
    (if-let ((body-def .requestBody.content.application/json))
        (swagg--build-schema
         swagger
         (alist-get 'schema (swagg--resolve-if-ref swagger body-def)))
      (if-let* ((def (--find (s-matches? (or (alist-get 'in it) "") "body")
                             (alist-get 'parameters info)))
                (schema (alist-get 'schema def)))
          (swagg--build-schema swagger schema)
        (if schema (swagg--read-param-from-user :body def) "")))))

(defun swagg--gen-query-params (swagger info)
  "Get query params from INFO and build an alist.
SWAGGER is the full swagger object.  INFO should be a path
operation object.  Parameters are requested from user."
  (->>
   (alist-get 'parameters info)
   (mapcar (apply-partially #'swagg--resolve-if-ref swagger))
   (--filter (s-matches? (alist-get 'in it) "query"))
   (mapcar (apply-partially #'swagg--read-param-from-user :query))))

(defun swagg--gen-path-params (swagger info)
  "Get path params from INFO and build an alist.
SWAGGER is the full swagger object.  INFO should be a path
operation object.  Parameters are requested from user."
  (->>
   (alist-get 'parameters info)
   (mapcar (apply-partially #'swagg--resolve-if-ref swagger))
   (--filter (s-matches? (alist-get 'in it) "path"))
   (--map (cons (alist-get 'name it) (swagg--read-param-from-user :path it)))))

(defun swagg--gen-headers (info)
  "Get headers from INFO and build an alist.
SWAGGER is the full swagger object.  INFO should be a path
operation object.  Parameters are requested from user."
  (->>
   (alist-get 'parameters info)
   (--filter (equal (alist-get 'in it) "header"))
   (--map (let-alist it
            (cons .name
                  (swagg--read-string
                   (format "%s (%s): " .name .schema.type)
                   :type :header
                   :name .name))))))

;;;; swagg-response-mode

;; TODO: re-send request
;; TODO: copy request as curl
;; TODO: copy as org-mode
;; TODO: inspect request (show the request plist → swagg--request)
(define-minor-mode swagg-response-mode
  "Swagg response mode."
  :lighter " SRM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c #") #'swagg-display-headers)
            map))

(defun swagg--get-buffer-create-for-request (request)
  "Create response buffer for REQUEST."
  (if swagg-use-unique-buffer-per-request
      (get-buffer-create (format "*swagg: %s %s%s*"
                                 (swagg--req-type request)
                                 (plist-get request :name)
                                 (swagg--req-make-endpoint request)))
    (get-buffer-create swagg--result-buffer-name)))

(declare-function json-mode "json-mode")
(defun swagg--display-response (request response)
  "Display RESPONSE for REQUEST.
RESPONSE is the data returned by `request' call.  This function
tries to display the RESPONSE according to it's content-type."
  (with-current-buffer (swagg--get-buffer-create-for-request request)
    (erase-buffer)
    (insert (request-response-data response))
    (pcase (-some->> (request-response-header response "content-type")
             (s-split ";")
             (car)
             (downcase))
      ("application/json"
       (json-pretty-print-buffer)
       (if (featurep 'json-ts-mode)
           (json-ts-mode)
         (require 'json-mode)
         (json-mode)))
      (_ (prog-mode)))
    (setq
     header-line-format
     (format
      "%s %s → %s"
      (plist-get (request-response-settings response) :type)
      (request-response-url response)
      (request-response-status-code response)))
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (swagg-response-mode +1)
    (setq swagg--request request)
    (setq swagg--response response)))

(defun swagg-display-headers ()
  "Display headers of current response in a different buffer."
  (interactive nil swagg-response-mode)
  (pp-display-expression
   (request-response-headers swagg--response)
   swagg--headers-buffer-name))

;;;; swagg--req

(defun swagg--req-builder ()
  "For `swagg--def', create a plist containing all relevant request info."
  (-let* ((swagger (plist-get swagg--def :swagger))
          ((endpoint . (verb . info))
           (swagg--select-op swagger))
          (body (when-let* ((it (swagg--gen-body swagger info))
                            (_ (not (s-blank? it))))
                  it))
          (has-json-body (and
                          body
                          (let-alist info
                            (or .requestBody.content.application/json
                                (--find (s-matches? (or (alist-get 'in it) "") "body")
                                        (alist-get 'parameters info))))))
          (headers (map-merge
                    'alist
                    (plist-get swagg--def :header-all)
                    (swagg--gen-headers info)))
          ;; Multiple security schemas are not supported and I'm quite
          ;; happy that I used `caaar' in a real code.
          (security (caaar (alist-get 'security info (alist-get 'security swagger))))
          (security-headers
           (-when-let* ((security-def
                         (and security
                              (alist-get
                               security
                               (let-alist swagger .components.securitySchemes))))
                        ((key val placeholder) (swagg--security-header security-def)))
             (list (cons key (format val (or (swagg--read-string
                                              (format "%s: " key)
                                              :type :header
                                              :name key)
                                             placeholder))))))
          (content-type-headers
           (when (and has-json-body
                      (not (--find (let ((case-fold-search t))
                                     (string-match-p "^content-type$" (car it)))
                                   headers)))
             '(("Content-Type" . "application/json"))))
          (final-headers
           (seq-concatenate 'list security-headers content-type-headers headers)))
    (append
     swagg--def
     (list
      :info info
      :endpoint endpoint
      :verb verb
      :query-params (let* ((default-params (plist-get swagg--def :query-all))
                           (user-params (--filter (not (s-ends-with? "=" it)) (swagg--gen-query-params swagger info)))
                           (param-names (--map (car (s-split "=" it)) user-params))
                           (non-default-used-params (--filter (not (-contains? param-names (car it))) default-params)))
                      (append
                       (--map (format "%s=%s" (car it) (url-hexify-string (cdr it))) non-default-used-params)
                       user-params))
      :path-params (swagg--gen-path-params swagger info)
      :headers final-headers
      :body body))))

(defun swagg--req-type (req)
  "Return the request type (GET, POST, PUT etc.) for REQ as string."
  (upcase (symbol-name (plist-get req :verb))))

(defun swagg--req-make-endpoint (req)
  "Generate endpoint path with filled parameters for REQ."
  (s-replace-regexp
   "{\\([a-zA-Z0-9_-]+\\)}"
   (lambda (substr)
     (let ((param-name (s-chop-prefix "{" (s-chop-suffix "}" substr))))
       (or
        (alist-get param-name (plist-get req :path-params) nil nil #'equal)
        ;; FIXME: I don't remember the reason for the following read-string
        (save-match-data
          ;; HACK ^^ fails without this for some reason. Possibly a
          ;; problem in my config
          (read-string (format "%s (?): " param-name))))))
   (symbol-name (plist-get req :endpoint))))

(defun swagg--req-gen-url (req)
  "Generate full URL with filled parameters for REQ."
  (format
   "%s%s%s%s"
   (plist-get req :base)
   (swagg--req-make-endpoint req)
   (if (plist-get req :query-params) "?" "")
   (s-join "&" (plist-get req :query-params))))

;;;; Interactive - User level

;; FIXME: This does not handle request body
;;;###autoload
(defun swagg-request (definition)
  "Select an endpoint from Swagger DEFINITION and make a request.
When called interactively, you'll be prompted to select a
definition from `swagg-definitions' first.

After selecting a definition and an endpoint, you'll be prompted
to enter all path parameters and the request will be made.  The
result will be displayed in a separate buffer.

This function does not support requests with bodies.  For that,
see `swagg-request-with-rest-block'.

Also see `swagg-use-unique-buffer-per-request'."
  (interactive (list (swagg--select-definition)))
  (swagg--with-def definition
    (let ((req (swagg--req-builder)))
      (request
        (swagg--req-gen-url req)
        :type (swagg--req-type req)
        :headers (plist-get req :headers)
        :parser #'buffer-string
        :complete (cl-function
                   (lambda (&key response &allow-other-keys)
                     (swagg--display-response req response)))))))

(defun swagg--generate-rest-block (def)
  "Generate rest block for DEF."
  (swagg--with-def def
    (let ((req (swagg--req-builder)))
      (s-trim
       (concat
        (swagg--req-type req)
        " "
        (swagg--req-gen-url req)
        (when-let* ((headers (plist-get req :headers)))
          (concat "\n"
                  (s-join "\n"
                          (mapcar (-lambda ((key . val))
                                    (format "%s: %s" key val))
                                  headers))))
        (when-let* ((body (plist-get req :body)))
          (concat "\n\n" body)))))))

(defun swagg--generate-js-fetch-call (def)
  "Generate JavaScript fetch call for DEF."
  (swagg--with-def def
    (let ((req (swagg--req-builder)))
      (swagg--indent
       2
       (format
        "await fetch('%s', {\n  method: '%s',\n%s%s})"
        (swagg--req-gen-url req)
        (swagg--req-type req)
        ;; headers
        (if-let* ((headers (plist-get req :headers))
                  (joined (s-join ",\n    "
                                  (mapcar
                                   (-lambda ((key . val))
                                     (format "'%s': '%s'" key val))
                                   headers))))
            (format "  headers: {\n    %s,\n  }" joined)
          "")
        ;; body
        (if-let* ((body (plist-get req :body)))
            (format
             "  body: JSON.stringify(%s),\n"
             (s-chop-left 2 (swagg--indent 2 body)))
          ""))))))

(defun swagg--write-block-to-swagg-buffer (block arg)
  (setq block (concat swagg-rest-block-prelude block swagg-rest-block-postlude))
  (if arg
      (insert block)
    (with-current-buffer (get-buffer-create swagg--rest-buffer)
      (org-mode)
      (goto-char (point-max))
      (org-insert-heading nil t)
      (insert (format-time-string "%F %a %R"))
      (when swagg-rest-block-org-header-tags
        (org-set-tags swagg-rest-block-org-header-tags))
      (end-of-line)
      (insert "\n\n" block "\n")))
  (unless arg
    (switch-to-buffer-other-window swagg--rest-buffer)
    (goto-char (point-max))))

;;;###autoload
(defun swagg-request-with-rest-block (definition &optional arg)
  "Select an endpoint from Swagger DEFINITION and make a request.
When called interactively, you'll be prompted to select a
definition from `swagg-definitions' first.

After selecting a definition and an endpoint, you'll be prompted
to enter all path parameters.  After that, a new (or already
existing) `org-mode' buffer will open and your request will be
inserted (with generated request body, if there is one) into this
buffer.  Now you can utilize any rest client to send your
request, like one of the following: verb, restclient.el, ob-http.
See this project's README to learn more about the rest clients.

If ARG is non-nil, then instead of inserting the request to a new
buffer, simply insert it to current buffer.

Also see `swagg-rest-block-prelude' and
`swagg-rest-block-postlude' to control the inserted rest block
surroundings and `swagg-rest-block-org-header-tags' to
automatically tag request's org-header."
  (interactive (list (swagg--select-definition) current-prefix-arg))
  (swagg--write-block-to-swagg-buffer (swagg--generate-rest-block definition) arg))

(defun swagg-request-with-rest-src-block (definition &optional arg)
  "Select an endpoint from Swagger DEFINITION and create an org src block.
When called interactively, you'll be prompted to select a definition
from `swagg-definitions' first.

After selecting a definition and an endpoint, you'll be prompted to
enter all path parameters.  After that, a new (or already existing)
`org-mode' buffer will open and your request will be inserted (with
generated request body, if there is one) into this buffer.  Now you can
utilize any rest client to send your request, like one of the following:
verb, restclient.el, ob-http.  See `swagg-org-src-lang' to control
this.

If ARG is non-nil, then instead of inserting the request to a new
buffer, simply insert it to current buffer."
  (interactive (list (swagg--select-definition) current-prefix-arg))
  (let ((swagg-rest-block-prelude (format "#+begin_src %s\n" swagg-org-src-lang))
        (swagg-rest-block-postlude "\n#+end_src\n"))
    (swagg--write-block-to-swagg-buffer (swagg--generate-rest-block definition) arg)))

;;;###autoload
(defun swagg-request-with-fetch (definition &optional arg)
  "Like `swagg-request-with-rest-src-block' uses JavaScript fetch call syntax.
Also see `swagg-fetch-lang' variable."
  (interactive (list (swagg--select-definition) current-prefix-arg))
  (let ((swagg-rest-block-prelude (format "#+begin_src %s\n" swagg-fetch-lang))
        (swagg-rest-block-postlude "\n#+end_src\n"))
    (swagg--write-block-to-swagg-buffer (swagg--generate-js-fetch-call definition) arg)))

;;;; Interactive helpers

(defun swagg--select-definition ()
  "Select a definition interactively.
Return the parsed swagger object, base API url and the name of
the definition as it's defined in `swagg-definitions'."
  (let* ((selected (swagg--completing-read-object
                    "Select: "
                    swagg-definitions
                    :formatter (lambda (it) (plist-get it :name))))
         (definition-type (--first (-contains? '(:json :yaml) it) (map-keys selected)))
         (definition (plist-get selected definition-type))
         (name (plist-get selected :name))
         (swagger
          (with-memoization (alist-get name swagg--json-cache nil nil #'equal)
            (when (functionp definition)
              (setq definition (funcall definition selected)))
            (if (file-exists-p definition)
                (with-temp-buffer
                  (insert-file-contents definition)
                  (swagg--definition-parse-buffer definition-type))
              (let (result)
                (request definition
                  :sync t
                  :parser (apply-partially #'swagg--definition-parse-buffer definition-type)
                  :complete (cl-function
                             (lambda (&key _status data &allow-other-keys)
                               ;; TODO: Handle status
                               (setq result data))))
                result)))))
    `(,@selected :swagger ,swagger)))

;;;###autoload
(defun swagg-invalidate-cache (&optional select?)
  "Invalidate swagger definition JSON cache.
Useful if your swagger JSON/YAML has been changed.

If SELECT? is non-nil, instead of invalidating all definitions,
prompt user to select a definition to invalidate."
  (interactive "P")
  (if select?
      (and-let* ((selected (swagg--select-definition)))
        (setq swagg--json-cache (map-delete swagg--json-cache (plist-get selected :name))))
    (setq swagg--json-cache '()))
  (message "Invalidated!"))

;;;; Footer

(provide 'swagg)
;;; swagg.el ends here
