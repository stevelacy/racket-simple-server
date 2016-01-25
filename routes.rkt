#lang racket

(provide routes)


(define routes (make-hash))

(define (build-page label)
  `(html
     (head
       (body
         (div
           ,label)))))


(define (root query)
  (build-page "root"))
(define (test query)
  (build-page "test"))

(hash-set! routes "" root)
(hash-set! routes "test" test)

