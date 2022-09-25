;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs)
             (racketlist->mupllist (cdr xs)))))

;; Problem 2

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      '()
      (cons (apair-e1 xs)
            (mupllist->racketlist (apair-e2 xs)))))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "Numerical comparision between non-numbers")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e)
                                                     v)
                                               env)))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let ([f (closure-fun c)])
                 (eval-under-env (fun-body f) (cons (cons (fun-formal f) arg)
                                                    (if (fun-nameopt f)
                                                        (cons (cons (fun-nameopt f) c)
                                                              (closure-env c))
                                                        (closure-env c)))))
               (error "First argument to call is not a function")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Applying fst to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Applying snd to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst)
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1)
               (cons "_y" e2))
         ; check if max(e1, e2) > min(e1, e2)
         (ifgreater (ifgreater (var "_x") (var "_y")
                               (var "_x") (var "_y"))
                    (ifgreater (var "_x") (var "_y")
                               (var "_y") (var "_x"))
                    e4
                    e3)))

;; Problem 4

(define mupl-map
  (fun #f "f"
       (fun "map" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "f") (fst (var "xs")))
                            (call (var "map") (snd (var "xs")))
                            )))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map") (fun #f "x"
                                    (add (var "x") (var "i"))))
             )))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(fun? e)
         (letrec ([f (lambda (exp)
                       (cond [(fun? exp)
                              (set-remove (set-remove (f (fun-body exp))
                                                      (fun-formal exp))
                                          (fun-nameopt exp))]      ;; remove #f when anonymous, so doing nothing
                             [(var? exp)
                              (set (var-string exp))]
                             [(add? exp)
                              (set-union (f (add-e1 exp))
                                         (f (add-e2 exp)))]
                             [(ifgreater? exp)
                              (set-union (f (ifgreater-e1 exp))
                                         (f (ifgreater-e2 exp))
                                         (f (ifgreater-e3 exp))
                                         (f (ifgreater-e4 exp)))]
                             [(mlet? exp)
                              (set-union (f (mlet-e exp))
                                         (set-remove (f (mlet-body exp))
                                                     (mlet-var exp)))]
                             [(call? exp)
                              (set-union (f (call-funexp exp))
                                         (f (call-actual exp)))]
                             [(apair? exp)
                              (set-union (f (apair-e1 exp))
                                         (f (apair-e2 exp)))]
                             [(fst? exp)
                              (f (fst-e exp))]
                             [(snd? exp)
                              (f (snd-e exp))]
                             [(isaunit? e)
                              (f (isaunit-e exp))]
                             [else (set)]))])
           (fun-challenge (fun-nameopt e)
                          (fun-formal e)
                          (compute-free-vars (fun-body e))
                          (f e)))]
        ; [(var? e) ]
        [(add? e)
         (add (compute-free-vars (add-e1 e))
              (compute-free-vars (add-e2 e)))]
        [(ifgreater? e)
         (ifgreater (compute-free-vars (ifgreater-e1 e))
                    (compute-free-vars (ifgreater-e2 e))
                    (compute-free-vars (ifgreater-e3 e))
                    (compute-free-vars (ifgreater-e4 e)))]
        [(mlet? e)
         (mlet (mlet-var e)
               (compute-free-vars (mlet-e e))
               (compute-free-vars (mlet-body e)))]
        [(apair? e)
         (apair (compute-free-vars (apair-e1 e))
                (compute-free-vars (apair-e2 e)))]
        [(fst? e)
         (fst (compute-free-vars (fst-e e)))]
        [(snd? e)
         (snd (compute-free-vars (snd-e e)))]
        [(isaunit? e)
         (isaunit (compute-free-vars (isaunit-e e)))]
        [(call? e)
         (call (compute-free-vars (call-funexp e))
               (compute-free-vars (call-actual e)))]
        [(closure? e)
         (closure (closure-env e)
                  (compute-free-vars (closure-fun e)))]
        [else e]
        ))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [(fun-challenge? e) (closure (map (lambda (i)
                                            (cons i (envlookup env i)))
                                          (set->list (fun-challenge-freevars e))) e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)
                      (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "Numerical comparision between non-numbers")))]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e)
                                                       v)
                                                 env)))]
        [(call? e)
         (let ([c (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? c)
               (let ([f (closure-fun c)])
                 (eval-under-env-c (fun-challenge-body f) (cons (cons (fun-challenge-formal f) arg)
                                                                (if (fun-challenge-nameopt f)
                                                                    (cons (cons (fun-challenge-nameopt f) c)
                                                                          (closure-env c))
                                                                    (closure-env c)))))
               (error "First argument to call is not a function")))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Applying fst to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Applying snd to non-pair")))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
