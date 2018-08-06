(import :std/sugar
        :std/iter
        :std/misc/string
        :std/text/json
        :std/net/request
        (only-in :std/net/httpd http-response-write)
        (only-in :std/db/dbi sql-exec)
        (only-in :gerbil/gambit/os current-time time->seconds)
        (only-in :gerbil/gambit/ports object->string)
        :vyzo/gxpkgd/db
        :vyzo/gxpkgd/utils)

(export #t)

(def (github-pkg-url author name)
  (string-append "https://raw.githubusercontent.com/" author "/" name
                 "/master/gerbil.pkg"))

(def (github-license-url author name)
  (string-append "https://api.github.com/repos/" author "/" name "/license"))

(def (github-forks-url author name)
  (string-append "https://api.github.com/repos/" author "/" name "/forks"))

(def (github-get-pkg author name)
  (try
   (string->sexp (request-text (http-get (github-pkg-url author name))))
   (catch (e)
     (raise e))))

(def (github-get-license author name)
  (try
   (let* ((req (http-get (github-license-url author name)))
          (json (request-json req)))
     (hash-ref (hash-ref json 'license) 'spdx_id))
   (catch (e)
     (raise e))))

(def (github-get-forks author name)
  (try
   (let* ((req (http-get (github-forks-url author name)))
          (json (request-json req)))
     (for/collect (fork json)
       (list login: (hash-ref (hash-ref fork 'owner) 'login)
             name: (hash-ref fork 'name)
             html_url: (hash-ref fork 'html_url))))))

(def (github-parse-url url)
  (let (slug (string-split (string-trim-prefix "/package/github/" url) #\/))
    (list author: (car slug) name: (cadr slug))))

(def (github-handler url db res)
  (try
   (let* ((slug (github-parse-url url))
          (author (pgetq author: slug))
          (name (pgetq name: slug))
          (repo (string-append "https://github.com/" author "/" name))
          (pkg (github-get-pkg author name))
          (description (pgetq description: pkg))
          (runtime (symbol->string (pgetq runtime: pkg))) ; currently is a symbol
          (license (github-get-license author name))
          ;; (forks (github-get-forks author name))
          (props '(author: name: description: runtime: license: last-update: repo:))
          (package (list author: author
                         name: name
                         description: description
                         runtime: runtime
                         license: license
                         last-update: (number->string (time->seconds (current-time)))
                         repo: repo)))
     (with-dbi (dbi db)
               (with-txn dbi
                         (let (insert-package (DBi-insert-package dbi))
                           (sql-bind-plist insert-package props package)
                           (sql-exec insert-package))))
     (http-response-write res 200 '(("Content-Type" . "text/plain"))
                          "success\n"))
   (catch (e)
     (http-response-write res 500 '(("Content-Type" . "text/plain"))
                          "Error.\n")
     (raise e))))