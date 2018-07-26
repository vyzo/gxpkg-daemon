;;; -*- Gerbil -*-
;;; © vyzo
;;; The Gerbil Package Metadata Server
;;;
;;; This is a simple server supporting package metadata aggregation and search

(import :gerbil/gambit/threads
        :std/net/httpd
        :std/getopt
        :std/sugar
        :vyzo/gxpkgd/server)
(export main)

(def (run address)
  (let (httpd (start-http-server! address mux: (make-server)))
    (thread-join! httpd)))

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: "127.0.0.1:8080")))

  (try
   (let (opt (getopt-parse gopt args))
     (run (hash-get opt 'address)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "gxpkgd" (current-error-port))
     (exit 1))))
