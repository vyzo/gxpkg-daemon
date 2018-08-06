;;; -*- Gerbil -*-
;;; © vyzo
;;; package database

(import :std/db/dbi
        :std/db/sqlite
        :std/db/conpool
        :std/iter
        :std/sugar
        :std/misc/text)

(export make-DB
        DB? DB-close DB-get DB-put
        DBi? DBi-insert-package DBi-insert-fork
        with-dbi with-txn
        sql-bind-map sql-bind-plist)

;; the database
(defstruct DB (conns #|additional state ...|#)
  constructor: :init! final: #t)

;; the database connection interface
(defstruct DBi (c insert-package insert-fork)
  constructor: :init! final: #t)

(defmethod {:init! DB}
  (lambda (self path)
    (let (path (path-expand path))
      (unless (file-exists? path)
        (create-directory* (path-directory path))
        (DB-init! path))
      (let (cp (make-conpool (cut DB-connect self path)))
        (set! (DB-conns self) cp)))))

(defmethod {:init! DBi}
  (lambda (self c insert-package insert-fork)
    (struct-instance-init! self c insert-package insert-fork)))

;; invoked by the connection pool to close the db connection
(defmethod {destroy DBi}
  (lambda (self)
    (sql-close (DBi-c self))))

;; initialize the database
(def (DB-init! path)
  (let (sqlite (sqlite-open path))
    (eval-sql-script sqlite DB-schema)
    {close sqlite}))

;; create a database connection object; invoked by the connection pool
(def (DB-connect db path)
  (let* ((c (sql-connect sqlite-open path))
         (dbi (make-DBi c
                        (sql-prepare c "INSERT INTO packages (author, name, description, runtime, license, last_update, repo) values (?, ?, ?, ?, ?, ?, ?)")
                        (sql-prepare c "INSERT INTO forks (author, name, html_url, package_id) VALUES (?, ?, ?, ?)"))))
    dbi))

;; get a database connectionn interface
(def (DB-get db timeout: (timeo absent-obj))
  (conpool-get (DB-conns db) timeo))

;; release a database connection interface
(def (DB-put db dbi)
  (conpool-put (DB-conns db) dbi))

;; close a db
(def (DB-close db)
  (conpool-close (DB-conns db)))

;; DBi interface
;; utilitiy macros
(defrules with-dbi ()
  ((_ (dbi db) body ...)
   (let (dbi (DB-get db))
     (try
      body ...
      (finally (DB-put db dbi))))))

(defrules with-txn ()
  ((_ dbi body ...)
   (begin
     (sql-txn-begin (DBi-c dbi))
     (try
      body ...
      (sql-txn-commit (DBi-c dbi))
      (catch (e)
        (sql-txn-abort (DBi-c dbi))
        (raise e))))))

(defrules sql-bind-map ()
  ((_ stmt keys data)
   (apply (cut sql-bind stmt <...>) (map (cut hash-ref data <>) keys))))

(defrules sql-bind-plist ()
  ((_ stmt props plist)
   (apply (cut sql-bind stmt <...>) (map (cut pgetq <> plist) props))))

(def (eval-sql-script dbc sql)
  (for (stmt (string-split sql #\;))
    (unless (or (string-empty? stmt) (equal? stmt "\n"))
      (sql-eval dbc stmt))))

;; TODO database api

(def DB-schema (include-text "sql/schema.sql"))
