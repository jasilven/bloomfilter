#lang racket/base

(require data/bit-vector
         openssl/md5
         racket/function)

(provide bloom-new bloom-add bloom-exists? bloom?)

;; bloom filter struct
(struct bloom (hash-count bits) #:transparent)

;; create new bloom filter, size = number of bits, n = items in filter
(define (bloom-new size n)
  (bloom (max 1 (inexact->exact (round (* (log 2) (/ size n)))))
         (make-bit-vector size)))

;; get bit-vector indexes to set up
(define (bloom-indexes b item)
  (for/list ([i (in-range 0 (bloom-hash-count b))])
    (let* ([s (string-append item (number->string i))]
           [num (string->number (md5 (open-input-string s)) 16)])
      (modulo num (bit-vector-length (bloom-bits b))))))

;; returns new bloom filter with items in lst added
(define (bloom-add b lst)
  (define bitvec (bit-vector-copy (bloom-bits b)))
  (for* ([item (in-list lst)]
         [i (in-list (bloom-indexes b item))])
    (bit-vector-set! bitvec i #t))
  (struct-copy bloom b [bits bitvec]))

;; check if item exists in bloom filter b
(define (bloom-exists? b item)
  (andmap (curry bit-vector-ref (bloom-bits b))
          (bloom-indexes b item)))

;; tests
(module+ test
  (require rackunit)
  (define items (for/list ([i (in-range 0 100)])
                  (string-append "test" (number->string i))))
  (test-case "test bloom?"
    (check-true (bloom? (bloom-new 1 1))))
  (test-case "test empty bloom"
    (check-false (bloom-exists? (bloom-new 1 1) "non-existing-item")))
  (test-case "adding one by one"
    (define bloom (for/fold ([b (bloom-new 300 100)])
                            ([item (in-list items)])
                    (bloom-add b (list item))))
    (check-true (andmap (curry bloom-exists? bloom) items)))
  (test-case "adding list of items"
    (define bloom (bloom-add (bloom-new 400 200) items))
    (check-true (andmap (curry bloom-exists? bloom) items)))
  (test-case "non existing item"
    (define items (for/list ([i (in-range 0 100)])
                    (string-append "test" (number->string i))))
    (check-false (bloom-exists? (bloom-new 200 500) ""))
    (check-false (bloom-exists? (bloom-new 200 500) "test1"))))
