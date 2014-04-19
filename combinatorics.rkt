#lang racket/base

(module+ test
  (require racket/sequence
           rackunit)
  (check-eqv? (factorial 10) 3628800)
  (check-eqv? (combinations 52 5) 2598960)
  (check-eqv? (sequence-length (in-permutations '(1 2 3 4 5))) 120))

(require racket/contract/base
         racket/generator
         racket/list)

(provide (contract-out
          [factorial (-> natural-number/c integer?)]
          [combinations (-> natural-number/c natural-number/c integer?)]
          [permutations (-> natural-number/c natural-number/c integer?)]
          [in-permutations (-> list? sequence?)]))

(define (factorial n)
  (for/product ([i (in-range 2 (add1 n))])
    i))

(define (combinations n k)
  (if (<= (* 2 k) n)
      (quotient
       (permutations n k)
       (factorial k))
      (combinations n (- n k))))

(define (permutations n k)
  (for/product ([i (in-range (add1 (- n k))
                             (add1 n))])
    i))

(define (in-permutations lst)
  (in-generator
   (let loop ([lst lst]
              [tail '()])
     (if (null? lst)
         (yield tail)
         (for ([(x i) (in-indexed lst)])
           (define-values (front back) (split-at lst i))
           (loop (append front (cdr back))
                 (cons x tail)))))))
