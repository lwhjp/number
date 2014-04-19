#lang racket/base

(module+ test
  (require rackunit)
  (define primes (in-primes))
  (check-equal? (factors 1234567890 primes)
                '(2 3 3 5 3607 3803))
  (for ([i (in-range 1000)]
        [p primes])
    (check-pred prime? p))
  (define thousandth-prime
    (sequence-ref primes (sub1 1000)))
  (check-eqv? thousandth-prime 7919))

(require racket/contract/base
         racket/promise
         racket/sequence
         racket/stream)

(provide (contract-out
          [factors
           (->* (exact-positive-integer?)
                (sequence?)
                (listof exact-positive-integer?))]
          [prime? (-> exact-positive-integer? boolean?)]
          [in-primes (-> sequence?)]))

(define (factors n [primes (in-naturals 2)])
  (let loop ([n n]
             [ps (sequence->stream primes)])
    (define p (stream-first ps))
    (define-values (q r) (quotient/remainder n p))
    (cond
      [(= 1 n) '()]
      [(zero? r) (cons p (loop q ps))]
      [else (loop n (stream-rest ps))])))

(define (prime? n)
  (cond
    [(= n 1) #f]
    [(< n 4) #t]
    [(even? n) #f]
    [(< n 9) #t]
    [(zero? (remainder n 3)) #f]
    [else
     (for/and ([i (in-range 5
                            (add1 (integer-sqrt n))
                            6)])
       (not (or (zero? (remainder n i))
                (zero? (remainder n (+ i 2))))))]))

(struct prime-sieve (chunk next))

(define (make-prime-sieve [initial-chunk-size 1000]
                          [max-chunk-size 10000000])
  (define first-chunk #f)
  (define (make-chunk begin chunk-size)
    (define end (+ begin (* chunk-size 2)))
    (prime-sieve
     (filter-odd-primes begin end first-chunk)
     (delay
       (make-chunk end
                   (min (* chunk-size 10)
                        max-chunk-size)))))
  (set! first-chunk (make-chunk 3 initial-chunk-size))
  first-chunk)

(define (filter-odd-primes begin end first-chunk)
  (define chunk
    (make-vector (quotient (add1 (- end begin)) 2) #t))
  ;; Filter multiples of known primes
  (let/ec done
    (define last (quotient end 2))
    (unless first-chunk
      (done))
    (let filter-chunks ([ch first-chunk])
      (for ([p (in-vector (prime-sieve-chunk ch))])
        (when (> p last)
          (done))
        (define r (remainder begin p))
        (define first-odd-multiple
          (cond
            [(zero? r) begin]
            [(odd? r) (+ begin (- p r))]
            [else (+ begin p (- p r))]))
        (for ([i (in-range (quotient (- first-odd-multiple begin) 2)
                           (vector-length chunk)
                           p)])
          (vector-set! chunk i #f)))
      (let ([next (prime-sieve-next ch)])
        (unless (promise-running? next)
          (filter-chunks (force next))))))
  ;; Record and filter multiples of remaining numbers
  (for/vector ([i (in-range (vector-length chunk))]
               [n (in-range begin end 2)]
               #:when (vector-ref chunk i))
    (for ([j (in-range (+ i n)
                       (vector-length chunk)
                       n)])
      (vector-set! chunk j #f))
    n))

(define (in-primes)
  (make-do-sequence
   (λ ()
     (values
      car
      (λ (pos)
        (if (promise? (cdr pos))
            (let ([sieve (force (cdr pos))])
              (append (vector->list (prime-sieve-chunk sieve))
                      (prime-sieve-next sieve)))
            (cdr pos)))
      (cons 2 (delay (make-prime-sieve)))
      #f
      #f
      #f))))
