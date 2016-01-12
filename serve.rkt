#lang racket

(require xml net/url)


(define (dispatch str-path)
  ; Parse the request as a URL:
  (define url (string->url str-path))
  ; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
    ; Call a handler:
    (h (url-query url))
    ; No handler found:
     `(html (head (title "Error"))
        (body
        (font ((color "red"))
          "Unknown page: "
          ,str-path)))))


(define (build-request-page label)
  `(html
     (head
       (body
         (div
           ,label)))))


(define dispatch-table (make-hash))

(define (root query)
  (build-request-page "root page"))

(hash-set! dispatch-table "" root)

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "hello world"))))


(define (handle in out)
  (define req
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                 (read-line in)))
  (when req
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (let ([xexpr (dispatch (list-ref req 1))])
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))


(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
  (define-values (in out) (tcp-accept listener))
  (thread (lambda ()
    (handle in out)
    (close-input-port in)
    (close-output-port out))))

  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))


(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

