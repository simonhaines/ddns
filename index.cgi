#! /home/conamalgamate/local/bin/racket
#lang racket/base

(require
 net/http-client
 racket/port
 racket/file
 racket/string
 racket/list)

(define api-key (string-trim
                 (file->string
                  (build-path (find-system-path 'home-dir) ".ddns.api-key"))))

(define (api cmd . args)
  (let* ((query (cons (list "key" api-key) args))
         (query (cons (list "cmd" cmd) query))
         (query (string-join (map (lambda (q) (string-join q "=")) query) "&"))
         (path (string-append "/?" query)))
    (let-values (((resp hdrs prt)
                  (http-sendrecv #:ssl? #t "api.dreamhost.com" path)))
      (map (λ (s) (string-split s "\t"))
           (string-split (port->string prt) "\n")))))

(define host (getenv "REMOTE_USER"))
(define addr (getenv "REMOTE_ADDR"))
(define dns (findf (λ (rec)
                     (and (equal? host (third rec))
                          (equal? "1" (seventh rec))))
                   (cdr (api "dns-list_records"))))
(define ip (and dns (fifth dns)))

(display "Content-type: text/plain")
(newline)
(newline)

(if (and host addr)
    (if (equal? addr ip)
        (display (format "no change: ~a" addr))
        (begin
          (when ip (api "dns-remove_record" (list "record" host)
                        '("type" "A") (list "value" ip)))
          (let ((result (api "dns-add_record" (list "record" host)
                             '("type" "A") (list "value" addr))))
            (if (equal? "success" (caar result))
                (display (format "update: ~a → ~a" ip addr))
                (display (format "error: ~a" result))))))
    (display (format "error: host=~a ip=~a~n" host ip)))
