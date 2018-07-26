;;; -*- Gerbil -*-
;;; © vyzo
;;; package database

(import :std/db/dbi
        :std/db/sqlite
        :std/db/conpool)
(export make-DB
        DB? DB-get DB-put
        DBi? ;; DBi interface ...
        )

;; the database
(defstruct DB (conns #|additional state ...|#)
  constructor: :init! final: #t)

;; the database connection interface
(defstruct DBi (db sqldb #|prepared statements ...|#)
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
  (lambda (self db sqlite)
    (struct-instance-init! self db sqlite)))

(defmethod {:destroy DBi}
  (lambda (self)
    (sql-close (DBi-db self))))

;; initialize the database
(def (DB-init! path)
  (let (sqlite (sqlite-open path))
    ;; TODO initialize the database for the schema
    {close sqlite}))

;; create a database connection object; invoked by the connection pool
(def (DB-connect db path)
  (let* ((sqlite (sql-connect sqlite-open path))
         (dbi (make-DBi db sqlite)))
    ;; TODO create prepared statements
    dbi))

;; get a database connectionn interface
(def (DB-get db timeout: (timeo (absent-obj)))
  (conpool-get (DB-conns db) timeo))

;; release a database connection interface
(def (DB-put db dbi)
  (conpool-put (DB-conns db) dbi))

;; TODO DBi interface
