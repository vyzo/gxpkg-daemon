;;; -*- Gerbil -*-
;;; © vyzo
;;; package database

(import :std/db/dbi
        :std/db/sqlite
        :std/db/conpool)
(export make-DB
        DB? DB-close DB-get DB-put
        DBi? ;; DBi interface ...
        )

;; the database
(defstruct DB (conns #|additional state ...|#)
  constructor: :init! final: #t)

;; the database connection interface
(defstruct DBi (c #|prepared statements ...|#)
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
  (lambda (self c)
    (struct-instance-init! self c)))

;; invoked by the connection pool to close the db connection
(defmethod {destroy DBi}
  (lambda (self)
    (sql-close (DBi-c self))))

;; initialize the database
(def (DB-init! path)
  (let (sqlite (sqlite-open path))
    ;; TODO initialize the database for the schema
    {close sqlite}))

;; create a database connection object; invoked by the connection pool
(def (DB-connect db path)
  (let* ((c (sql-connect sqlite-open path))
         (dbi (make-DBi c)))
    ;; TODO create prepared statements
    dbi))

;; get a database connectionn interface
(def (DB-get db timeout: (timeo (absent-obj)))
  (conpool-get (DB-conns db) timeo))

;; release a database connection interface
(def (DB-put db dbi)
  (conpool-put (DB-conns db) dbi))

;; close a db
(def (DB-close db)
  (conpool-close (DB-conns db)))

;; TODO DBi interface
