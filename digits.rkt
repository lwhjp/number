#lang racket/base

(module+ test
  (require rackunit)
  (check-eqv? (digits->number '(1 2 3)) 123)
  (check-equal? (number->digits 123) '(1 2 3))
  (check-false (palindrome? 123))
  (check-true (palindrome? 12321))
  (check-eqv? (reverse-digits 123) 321)
  (check-eqv? (sum-digits 123) 6))

(require racket/contract/base)

(provide radix?
         (contract-out
          [digits->number (->* ((listof natural-number/c))
                               (radix?)
                               natural-number/c)]
          [foldr/digits (->* ((-> natural-number/c any/c any/c)
                              any/c
                              natural-number/c)
                             (radix?)
                             any/c)]
          [number->digits (->* (natural-number/c)
                               (radix?)
                               (listof natural-number/c))]
          [palindrome? (->* (natural-number/c)
                            (radix?)
                            boolean?)]
          [reverse-digits (->* (natural-number/c)
                               (radix?)
                               natural-number/c)]
          [sum-digits (->* (natural-number/c)
                           (radix?)
                           integer?)]))

(define (radix? v)
  (and (exact-integer? v)
       (>= v 2)))

(define (digits->number ds [radix 10])
  (for/fold ([n 0])
            ([d (in-list ds)])
    (+ (* n radix) d)))

(define (foldr/digits proc init n [radix 10])
  (if (zero? n)
      init
      (let-values ([(q r) (quotient/remainder n radix)])
        (foldr/digits proc
                      (proc r init)
                      q
                      radix))))

(define (number->digits n [radix 10])
  (foldr/digits cons '() n radix))

(define (palindrome? n [radix 10])
  (= n (reverse-digits n radix)))

(define (reverse-digits n [radix 10])
  (foldr/digits
   (λ (d a)
     (+ (* a radix) d))
   0
   n
   radix))

(define (sum-digits n [radix 10])
  (foldr/digits + 0 n radix))
