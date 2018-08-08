;;; -*- Gerbil -*-
;;; © vyzo belmarca
;;; github.com provider

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

(export github-handler)

;; Authenticate all requests if envvar is set.
;; GitHub rate limits at 60/h for unauthenticated requests, 5000/h for auth.
(def (github-get url)
  (let (token (getenv "GITHUB_ACCESS_TOKEN" #f))
    (if token
      (http-get (string-append url "?access_token=" token))
      (http-get url))))

(def (github-pkg-url author name)
  (string-append "https://raw.githubusercontent.com/" author "/" name
                 "/master/gerbil.pkg"))

(def (github-license-url author name)
  (string-append "https://api.github.com/repos/" author "/" name "/license"))

(def (github-forks-url author name)
  (string-append "https://api.github.com/repos/" author "/" name "/forks"))

(def (github-get-pkg author name)
  (try
   (string->sexp (request-text (github-get (github-pkg-url author name))))
   (catch (e)
     (raise e))))

(def (github-get-license author name)
  (try
   (let* ((req (github-get (github-license-url author name)))
          (json (request-json req)))
     (hash-ref (hash-ref json 'license) 'spdx_id))
   (catch (e)
     (raise e))))

(def (github-get-forks author name)
  (try
   (let* ((req (github-get (github-forks-url author name)))
          (json (request-json req)))
     (for/collect (fork json)
       (list author: (hash-ref (hash-ref fork 'owner) 'login)
             name: (hash-ref fork 'name)
             html-url: (hash-ref fork 'html_url)
             repo: (string-append "https://github.com/" author "/" name))))))

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
          (runtime (unless* (symbol->string (pgetq runtime: pkg)) ""))
          (license (github-get-license author name))
          (forks (github-get-forks author name))
          (fork-props '(author: name: html-url: repo:))
          (package-props '(author: name: description: runtime: license: last-update: repo:))
          (package (list author: author
                         name: name
                         description: description
                         runtime: runtime
                         license: license
                         last-update: (number->string (time->seconds (current-time)))
                         repo: repo)))

     ;; SQL transactions
     (with-dbi (dbi db)
       (with-txn dbi
         {insert-package db dbi package-props package}
         (for (fork forks)
           {insert-fork db dbi fork-props fork})))

     (http-response-write res 200 '(("Content-Type" . "text/plain"))
                          "success\n"))
   (catch (e)
     (http-response-write res 500 '(("Content-Type" . "text/plain"))
                          "Error.\n")
     (raise e))))