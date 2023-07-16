;;; swagg.el --- Swagger UI -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Isa Mert Gurbuz

;; Author: Isa Mert Gurbuz <isamertgurbuz@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/isamert/swagg.el
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.0") (request "0.3.3") (dash "2.19.1") (json "1.5") (yaml "0.5.1"))
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

;; A practical Emacs UI for Swagger definitions

;;; Code:

(require 'request)
(require 'dash)
(require 'json)
(require 'yaml)
(require 'compat)
(require 'org)


;; Notes
;; Example Swagger JSON definitions
;; https://petstore.swagger.io/v2/swagger.json
;; https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json


;;; Customization

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
- :json OR :yaml (required): URL of the Swagger JSON or YAML file.
- :header (optional): A property list containing default header
  values.  If a request made to the API contains a header
  parameter named X, swagg first checks if the :header property
  list contains a value for X. If found, the value is used as the
  default value.  If not found, the user is prompted to enter the
  value for X. X being a hypothetical example here, works for any
  header keys.
- :query (optional): A property list containing default query
  parameter values.  Similar to the :header property list, swagg
  first checks this list for a default value for any query
  parameter encountered in a request.  If not found, prompts the
  user to enter the value for the query parameter.
- :any (optional): A property list containing default header and
  query parameter values.  Its structure is the same as :header or
  :query.  Any parameter found in the request is checked first in
  this property list.  If not found, prompts the user to enter a
  value.

Here's an example of a Swagger definition:

  (:name \"GitHub API\"
    :json \"https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json\"
    :base \"https://api.github.com\"
    :header (Authorization \"Bearer secret-token\"))

This defines an API with the name \"GitHub API\".  It uses the
Swagger JSON file located at the given URL as its documentation.
The base URL of this API is \"https://api.github.com\".  The
`:header` property list contains a default value for the
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

(defcustom swagg-remember-inputs t
  "Whether to remember inputs for paremeters you entered before.
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


;;; Internal

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

;; TODO: Persist cache between sessions?
(defvar swagg--json-cache '()
  "Cache for downloaded and parsed swagger definitions.
Set this to nil if you want invalidate the cache.")

;; TODO: savehist support?
(defvar swagg--read-string-cache '()
  "Cache for header/parameter inputs.
See `swagg-remember-inputs'.")


;;; Helpers

(defun swagg--alist-path-get (paths alist)
  (if (eq (length paths) 1)
      (alist-get (car paths) alist)
    (swagg--alist-path-get (seq-drop paths 1) (alist-get (car paths) alist))))

(cl-defun swagg--completing-read-object
    (prompt objects &key (formatter #'identity) category (sort? t) group def)
  "Same as `completing-read' but applies FORMATTER to every object
  and propertizes candidates with the actual object so that they
  can be retrieved later by embark actions. Also adds category
  metadata to each candidate, if given."
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
  "Possibly a bad idea, but here we go."
  (declare (indent 1))
  `(let ((swagg--def ,def))
     ,@forms))

(defun swagg--definition-parse-buffer (type)
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
NAME is the name of the query/header parameter that is
cached.  PROMPT is passed to `read-string' as-is."
  ;; TODO support alists for `bound-value'
  (let* ((bound-value (plist-get
                       ;; FIXME: instead of or'ing, merge the found (or nil) type-specific list with :any list
                       (or (plist-get swagg--def type)
                           (plist-get swagg--def :any))
                       (intern name)))
         (bound (if (functionp bound-value)
                    (funcall bound-value)
                  bound-value))
         (cache-path (format "%s.%s.%s" (plist-get swagg--def :name) name type))
         (cache (alist-get cache-path swagg--read-string-cache nil nil #'equal))
         (result
          (if (and bound swagg-auto-accept-bound-values)
              bound
            (read-string prompt (or (when swagg-remember-inputs cache) bound)))))
    (setf (alist-get cache-path swagg--read-string-cache nil nil #'equal) result)
    result))


;;; Core

(defun swagg--select-op (swagger)
  (swagg--completing-read-object
   "Select operation: "
   (--mapcat
    (-let (((path . ops) it))
      (--map (cons path it) (swagg--resolve-if-ref swagger ops)))
    (alist-get 'paths swagger))
   :formatter
   (-lambda ((path . (op . info)))
     (format "%-7s → %-50s %s"
             (propertize
              (upcase (symbol-name op)) 'face `(:foreground ,(pcase op
                                                               ('get "sky blue")
                                                               ('post "green")
                                                               ('delete "red")
                                                               ('patch "yellow")
                                                               (_ "white"))))
             path
             (propertize (or (alist-get 'summary info) "") 'face 'italic)))
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
          ((lambda (it) (string-trim-left it "[ /]+")))
          (swagg--split-once "/")
          (car)
          (swagg--split-once " ")
          (car)
          (s-trim))
        "home")))))

(defun swagg--split-once (sep s)
  (s-split-up-to sep s 1 t))

(defun swagg--get-obj-definition (swagger schema-path)
  (let-alist swagger
    (alist-get (intern (file-name-nondirectory (directory-file-name schema-path))) .components.schemas)))

(defun swagg--build-schema (swagger schema)
  (let* ((example (alist-get 'example schema))
         (enum (alist-get 'enum schema))
         (type (alist-get 'type schema))
         (value (or example (when enum (s-join "|" enum)) (format "<%s>" type))))
    (pcase type
      ("object"
       (format "{%s}"
               (s-join ", " (-map
                             (-lambda ((name . child))
                               (format
                                "\"%s\": %s"
                                name
                                (swagg--build-schema swagger child)))
                             (alist-get 'properties schema)))))
      ("string" (format "\"%s\"" (or value "?")))
      ("number" (or value 1))
      ("integer" (or value 1))
      ("array" (format
                "[%s]"
                (swagg--build-schema swagger (alist-get 'items schema))))
      (_ (let-alist schema
           (cond
            (.$ref (swagg--build-schema
                    swagger
                    (swagg--resolve-ref swagger .$ref)))
            (.allOf (or (json-encode (alist-get 'example .allOf))
                        (swagg--build-schema
                         swagger
                         (swagg--resolve-ref swagger .allOf.$ref))))
            (t "???")))))))

(defun swagg--resolve-ref (swagger ref)
  (swagg--alist-path-get
   (--map (intern it) (s-split "/" (string-trim-left ref "#/")))
   swagger))

(defun swagg--resolve-if-ref (swagger obj)
  (if-let (ref (alist-get '$ref obj))
      (swagg--resolve-ref swagger ref)
    obj))

(defun swagg--format-param-value (def value)
  (let-alist def
    (if (equal .in "query")
        (format "%s=%s" .name value)
      value)))

(defun swagg--read-param-from-user (param-type def)
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

;; TODO format resulting json
(defun swagg--gen-body (swagger info)
  ;; Either use requestBody.content.application/json or
  ;; first(parameters.in === "body") as the request body
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
  (->>
   (alist-get 'parameters info)
   (mapcar (apply-partially #'swagg--resolve-if-ref swagger))
   (--filter (s-matches? (alist-get 'in it) "query"))
   (mapcar (apply-partially #'swagg--read-param-from-user :query))))

(defun swagg--gen-path-params (swagger info)
  (->>
   (alist-get 'parameters info)
   (mapcar (apply-partially #'swagg--resolve-if-ref swagger))
   (--filter (s-matches? (alist-get 'in it) "path"))
   (--map (cons (alist-get 'name it) (swagg--read-param-from-user :path it)))))

(defun swagg--gen-headers (info)
  (->>
   (alist-get 'parameters info)
   (--filter (equal (alist-get 'in it) "header"))
   (--map (let-alist it
            (cons .name
                  (swagg--read-string
                   (format "%s (%s): " .name .schema.type)
                   :type :header
                   :name .name))))))


;;; swagg-response-mode

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
  (if swagg-use-unique-buffer-per-request
      (get-buffer-create (format "*swagg: %s %s%s*"
                                 (swagg--req-type request)
                                 (plist-get request :name)
                                 (swagg--req-make-endpoint request)))
    (get-buffer-create swagg--result-buffer-name)))

(defun swagg--display-response (request response)
  (with-current-buffer (swagg--get-buffer-create-for-request request)
    (erase-buffer)
    (insert (request-response-data response))
    (pcase (-some->> (request-response-header response "content-type")
             (s-split ";")
             (car)
             (downcase))
      ("application/json"
       (json-pretty-print-buffer)
       (cond
        ((require 'json-ts-mode nil t) (json-ts-mode))
        ((require 'json-mode nil t) (json-mode))
        (t (prog-mode))))
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
  (interactive nil swagg-response-mode)
  (pp-display-expression
   (request-response-headers swagg--response)
   swagg--headers-buffer-name))


;;; swagg--req

(defun swagg--req-builder (def)
  (-let* ((swagger (plist-get def :swagger))
          ((endpoint . (verb . info))
           (swagg--select-op swagger)))
    (append
     def
     (list
      :info info
      :endpoint endpoint
      :verb verb
      :query-params (swagg--gen-query-params swagger info)
      :path-params (swagg--gen-path-params swagger info)
      :headers (swagg--gen-headers info)))))

(defun swagg--req-type (req)
  "Return the request type (GET, POST, PUT etc.) for REQ as string."
  (upcase (symbol-name (plist-get req :verb))))

(defun swagg--req-make-endpoint (req)
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
  (format
   "%s%s%s%s"
   (plist-get req :base)
   (swagg--req-make-endpoint req)
   (if (plist-get req :query-params) "?" "")
   (s-join
    "&"
    ;; Filter out empty parameters. Not the greatest place to do it but anyway.
    (--filter (not (s-ends-with? "=" it)) (plist-get req :query-params)))))


;;; Interactive - User level

(defun swagg-request (def)
  (interactive (list (swagg--select-definition)))
  (swagg--with-def def
    (let ((req (swagg--req-builder def)))
      (request
        (swagg--req-gen-url req)
        :type (swagg--req-type req)
        :headers (plist-get req :headers)
        :parser #'buffer-string
        :complete (cl-function
                   (lambda (&key response &allow-other-keys)
                     (swagg--display-response req response)))))))

(defun swagg--generate-rest-block (def)
  (swagg--with-def def
    (let ((req (swagg--req-builder def)))
      (s-trim
       (concat
        swagg-rest-block-prelude
        (swagg--req-type req)
        " "
        (swagg--req-gen-url req)
        (when-let* ((headers (plist-get req :headers)))
          (concat "\n"
                  (s-join "\n"
                          (mapcar (-lambda ((key . val))
                                    (format "%s: %s" key val))
                                  headers))))
        (when-let* ((body (swagg--gen-body
                           (plist-get def :swagger)
                           (plist-get req :info))))
          (concat "\n\n" body))
        swagg-rest-block-postlude)))))

(defun swagg-request-with-rest-block (def &optional arg)
  (interactive (list (swagg--select-definition)))
  (let ((block (swagg--generate-rest-block def)))
    (if arg
        (insert block)
      (with-current-buffer (get-buffer-create swagg--rest-buffer)
        (org-mode)
        (goto-char (point-max))
        (org-insert-heading nil t)
        (insert (format-time-string "%Y-%m-%d %a %H:%M") "\n\n")
        (insert block "\n"))))
  (unless arg
    (switch-to-buffer-other-window swagg--rest-buffer)
    (goto-char (point-max))))


;;; Interactive helpers

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
            (if (file-exists-p definition)
                (with-temp-buffer
                  (insert-file-contents definition)
                  (swagg--definition-parse-buffer definition-type))
              (let (result)
                (request definition
                  :sync t
                  :parser (apply-partially #'swagg--definition-parse-buffer definition-type)
                  :complete (cl-function
                             (lambda (&key status data &allow-other-keys)
                               ;; TODO: Handle status
                               (setq result data))))
                result)))))
    `(,@selected :swagger ,swagger)))

(defun swagg-invalidate-cache ()
  "Invalidate swagger definition JSON cache.
Useful if your swagger JSON has been changed."
  (interactive)
  (setq swagg--json-cache '()))



(provide 'swagg)
;;; swagg.el ends here
