#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low)
      '()
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs
                              (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (let ([s (s)])
        (cons (car s)
              (stream-for-n-steps (cdr s) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                    (- x)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) (cons s (lambda () (if (equal? s "dog.jpg")
                                                 (f "dan.jpg")
                                                 (f "dog.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (let ([s (s)])
    (lambda () (cons (cons 0 (car s))
                     (stream-add-zero (cdr s))))))

; ASSUMES: xs, ys non-empty
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                      (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n) (cond [(= n len) #f]
                                [(and (pair? (vector-ref vec n))
                                      (equal? (car (vector-ref vec n))
                                              v))
                                 (vector-ref vec n)]
                                [else (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]
           [index 0]
           [f (lambda (v) (cond [(vector-assoc v vec) (vector-assoc v vec)]  ; we could compute it only once and store the result
                                [else (let ([result (assoc v xs)])
                                        (if result
                                            (begin (vector-set! vec index result)
                                                   (set! index (remainder (+ index 1) (vector-length vec)))
                                                   result)
                                            #f))    ; result is false
                                            ]))])
    f))


(define-syntax while-less
  (syntax-rules (while-less do)
    [(while-less e1 do e2)
     (letrec ([high e1]
              [f (lambda () (or (>= e2 high) (f)))])
       (f))]))