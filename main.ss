;;; -*- Gerbil -*-
;;; © vyzo
;;; The Gerbil Package Metadata Server
;;;
;;; This is a simple server supporting package metadata aggregation and search

(import :gerbil/gambit/threads
        :std/net/httpd
        :std/getopt
        :std/sugar
        :vyzo/gxpkgd/server
        :vyzo/gxpkgd/db)
(export main)

(def (run address db-path)
  (let* ((db (make-DB db-path))
         (srv (make-server db))
         (httpd (start-http-server! address mux: srv)))
    (try
     (thread-join! httpd)
     (finally
      (DB-close db)))))

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: "127.0.0.1:8080")
            (option 'db "-db" "--database"
                    help: "path for embedded database"
                    default: (path-expand "var/gxpkgd/pkg.db"
                                          (getenv "GERBIL_PATH" "~/.gerbil")))))

  (try
   (let-hash (getopt-parse gopt args)
     (run .address .db))
   (catch (getopt-error? exn)
     (getopt-display-help exn "gxpkgd" (current-error-port))
     (exit 1))))
