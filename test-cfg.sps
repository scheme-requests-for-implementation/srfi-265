#!r6rs

;; © 2025 Marc Nieper-Wißkirchen.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice (including
;; the next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import
  (rnrs)
  (srfi :265 cfg))

(assert (equal? 1
          (cfg (halt)
            1)))

(assert (equal? 2
                (cfg (finally (x) 2 (halt))
                  x)))

(assert (equal? '(1 . 2)
                (let ([x 2])
                  (cfg (finally (x) (cons 1 x)
                         (halt))
                    x))))

(assert (equal? '(3 . 3)
                (cfg
                    (letrec ([l (finally (x) 3 (halt))])
                      (finally (x) (cons x x)
                        (go l)))
                  x)))

(assert (equal? '(4 . 5)
                (cfg (do
                         (lambda (e)
                           (e 4))
                       [(x) (finally (x) (cons x 5) (halt))])
                  x)))

(assert (equal? '(6 . 7)
                (let ([x 6])
                  (cfg (do
                           (lambda (e)
                             (e (cons x 7)))
                         [(x) (finally (x) x (halt))])
                    x))))

(assert (equal? 7
                (let ([x 7])
                  (cfg (do
                           (lambda (e1 e2)
                             (e1))
                         [() (finally (x) x (halt))]
                         [(x) (finally (x) x (halt))])
                    x))))

(assert (equal? '(8 . 10)
                (let ([x 8])
                  (cfg (letrec ([l (finally (x) (cons x y) (halt))])
                         (do (lambda (e1 e2)
                                    (e2 9 10))
                           [(y) (go l)]
                           [(x y) (go l)]))
                    x))))

(assert (equal? '(12 . 13)
                (cfg (do (lambda (e1)
                                (e1 11))
                       [(x) (finally (x) (cons x 13)
                              (finally (x) (+ x 1) (halt)))])
                  x)))

(assert (equal? 14
                (cfg (permute ([l (go l)])
                       (finally (x) 14 (halt)))
                  x)))

(assert (equal? '(15 . 15)
                (let ([x 15])
                  (cfg (permute ([l (do
                                        (lambda (e)
                                          (e x))
                                      [(y) (go l)])])
                         (finally (x) (cons y x)
                           (halt)))
                    x))))

(assert (equal? '(15 . 16)
                (let ([x 15])
                  (cfg (permute/tail ([l (do
                                        (lambda (e)
                                          (e 16))
                                        [(x) (go l)])])
                         (permute ([l (do
                                          (lambda (e)
                                            (e x))
                                        [(y) (go l)])])
                           (finally (x) (cons y x)
                             (halt))))
                    x))))

(assert (equal? '(20 (0 . 17) (17 18 20))
                (let ([x 17]
                      [z 0])
                  (cfg
                      (permute/tail ([l1 (do (lambda (e)
                                               (e 21))
                                           [(z) (go l1)])])
                        (permute/tail ([l1 (permute/tail ([l2 (do
                                                                  (lambda (e)
                                                                    (e 18))
                                                                [(x) (go l2)])])
                                             (permute ([l2 (do
                                                               (lambda (e)
                                                                 (e x 20))
                                                             [(y z) (go l2)])])
                                               (do (lambda (e)
                                                          (e (list y x z)))
                                                 [(x) (go l1)])))])
                          (permute ([l1 (do
                                            (lambda (e)
                                              (e (cons z x)))
                                          [(y) (go l1)])])
                            (finally (x) (list z y x)
                              (halt)))))
                    x))))

(assert (equal? 21
                (cfg (letrec ([l (finally (x) x (halt))])
                       (permute ([l (do
                                        (lambda (e)
                                          (e 21))
                                      [(x) (go l)])])
                         (go l)))
                  x)))

(assert (equal? '(21 . 22)
                (cfg (let* [(l (finally (x) 21 (halt)))]
                       (let* [(l (finally (x) (cons x 22)
                                     (go l)))]
                         (go l)))
                  x)))

(assert (equal? '(23 24)
                (let ([x 23])
                  (cfg (let* [(l (permute ([l (do (lambda (e)
                                                           (e x))
                                                  [(y) (go l)])])
                                     (finally (x) (list y x) (halt))))]
                         (permute/tail ([l (do (lambda (e)
                                                 (e 24))
                                        [(x) (go l)])])
                           (go l)))
                    x))))

(assert (equal? '(25 . 26)
                (let ([x 25])
                  (cfg (permute ([l (do
                                        (lambda (e)
                                          (e 26))
                                      [(x) (go l)])]
                                 [l (do
                                        (lambda (e)
                                          (e x))
                                      [(y) (go l)])])
                         (finally (x) (cons y x)
                           (halt)))
                    x))))

(define-cfg-syntax simple-indep
  (lambda (stx)
    (syntax-case stx ()
      [(_ [(id init) ...] cfg)
       (for-all identifier? #'(id ...))
       #'(do (lambda (e)
                    (e init ...))
           [(id ...) cfg])])))

(assert (equal? 27
                (let ([x 26])
                  (cfg (simple-indep [(x (fx+ x 1))] (finally (x) x (halt)))
                    x))))

(assert (equal? '30
                (cfg (simple-indep [(x 10)] (finally (y) x (finally (x) (+ x 20) (halt))))
                  y)))

(assert (equal? '(49 (1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   [(l (simple-indep [(x (cons 1 x))] (go l)))]
                                 (simple-indep [(z (cons 3 x))] (go l)))])
                         (simple-indep [(u (cons 5 x))]
                               (finally (x) (list 49 x z u) (halt))))
                    x))))

(assert (equal? '((1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-indep [(x (cons 1 x))] (go l)))]
                                         (simple-indep [(z (cons 3 x))] (go l)))])
                                 (simple-indep [(u (cons 5 x))] (go l)))])
                         (finally (x) (list x z u) (halt)))
                    x))))

(assert (equal? '50
                (cfg (permute
                         ([l (simple-indep [(z 50)] (go l))]
                          [l (go l)])
                       (finally (x) z (halt)))
                  x)))

(assert (equal? '((1 . 28) (2 . 28) (3 . (1 . 28)) (4 . 28) (5 . (1 . 28)) (6 . 28))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-indep [(x (cons 1 x))] (go l)))
                                            (l (simple-indep [(y (cons 2 x))] (go l)))]
                                         (simple-indep [(z (cons 3 x))] (go l)))]
                                    [l (simple-indep [(w (cons 4 x))] (go l))])
                                 (simple-indep [(u (cons 5 x))] (go l)))]
                            [l (simple-indep [(v (cons 6 x))] (go l))])
                         (finally (x) (list x y z w u v) (halt)))
                    x))))

(assert (equal? '(2 30)
          (cfg (simple-indep ([x 1]) (letrec ([l (permute ([p (simple-indep ([y 30]) (go p))]
                                                          [p (do
                                                                 (lambda (e1 e2)
                                                                   (if (eqv? x 1)
                                                                       (e1 2)
                                                                       (e2)))
                                                               [(x) (go l)]
                                                               [() (go p)])])
                                                  (finally (x) (list x y) (halt)))])
                                      (go l)))
            x)))

;;; Permute and finally variables.

(assert (equal? 45
                (let ([x 35])
                  (cfg (permute ([p (finally (x) (+ 10 x) (go p))]
                                 [p (finally (x) (+ 10 x) (go p))])
                         (halt))
                    x))))

;;; Indep

(assert (equal? '((1 2) 4)
                (cfg (simple-indep ([x 1]) (indep ([x (values x 2)] [(y) (+ x 3)])
                                     (finally (x) (list x y) (halt))))
                  x)))

;;; Let*

(assert (equal? 1
                (cfg (let* ([p (finally (x) 1 (halt))]
                              [p (go p)])
                       (go p))
                  x)))

;;; Examples from specification

(define ex1
  (lambda (n*)
    (cfg (letrec
             [(f (do
                     (lambda (e1 e2)
                       (if (null? n*)
                           (e2)
                           (e1 (car n*) (cdr n*))))
                   [(n n*)
                    (do
                        (lambda (e1 e2)
                          (if (odd? n)
                              (e2 (+ o 1))
                              (e1 (+ e 1))))
                      [(e) (go f)]
                      [(o) (go f)])]
                   [()
                    (finally (e o) (values e o) (halt))]))]
           (do (lambda (e) (e n* 0 0)) [(n* e o) (go f)]))
      (values e o))))

(define ex2
  (lambda (n*)
    (cfg (letrec [(f (do
                         (lambda (e1 e2)
                           (if (null? n*)
                               (e2)
                               (e1 (car n*) (cdr n*))))
                       [(n n*)
                        (do
                            (lambda (e1 e2)
                              (if (odd? n)
                                  (e2)
                                  (e1)))
                          [()
                           (finally (e*) (cons n e*) (go f))]
                          [()
                           (finally (o*) (cons n o*) (go f))])]
                       [()
                        (finally (e* o*) (values '() '()) (halt))]))]
           (do (lambda (e) (e n*)) [(n*) (go f)]))
      (values e* o*))))

(assert (equal? '(2 1)
                (call-with-values
                    (lambda ()
                      (ex1 '(2 1 4)))
                  list)))

(assert (equal? '((2 4) (1))
                (call-with-values
                    (lambda ()
                      (ex2 '(2 1 4)))
                  list)))

;;; Label definitions

(define-cfg-label p)

(assert (equal? 40
                (let-syntax ([k (syntax-rules ()
                                  [(k d)
                                   (cfg (letrec ([(p) (finally (x) 40 (halt))])
                                          (go (d)))
                                     x)])])
                  (k p))))

(define-syntax permuting
  (lambda (stx)
    (syntax-case stx ()
      [(_ cfg-term ... result-expr)
       #'(cfg (permute ([(p) cfg-term] ...)
                (finally (res) result-expr (halt)))
           res)])))

(assert (equal? 99
          (permuting (simple-indep ([x 99]) (go (p))) x)))

;;; More examples from spec

(assert (equal? '(2 3)
                (let ([x 1])
                  (cfg (finally (x y) (values (+ x 1) (+ x 2))
                         (halt))
                    (list x y)))))

(assert (equal? 2
                (let ([x 1])
                  (cfg (do (lambda (e)
                                  (e (+ x 1)))
                         [(x) (finally (res) x (halt))])
                    res))))

(assert (equal? 'odd
                (let ([x 1])
                  (cfg (do (lambda (e1 e2)
                                  (if (even? x) (e1) (e2 'odd)))
                         [() (finally (res) 'even (halt))]
                         [(a) (finally (res) a (halt))])
                    res))))

(assert (equal? 'outer
                (let ([a 'outer]
                      [x 1])
                  (cfg (finally (res) a
                         (do (lambda (e1 e2)
                                    (if (even? x) (e1) (e2 'odd)))
                           [() (finally (res) 'even (halt))]
                           [(a) (halt)]))
                    res))))

(assert (equal? 'outer
                (let ([res 'outer]
                      [x 1])
                  (cfg (do (lambda (e1 e2)
                                  (if (even? x) (e1) (e2 'odd)))
                         [() (halt)]
                         [(a) (finally (res) a (halt))])
                    res))))

(assert (equal? 720
                (cfg (letrec ([f (do
                                        (lambda (e1 e2)
                                          (if (> x 6)
                                              (e1)
                                              (e2 (+ x 1) (* a x))))
                                      [() (finally (res) a (halt))]
                                      [(x a) (go f)])])
                          (simple-indep [(x 1) (a 1)] (go f)))
                     res)))

(cfg (letrec ([f (do
                     (lambda (e1 e2)
                       (if (> x 6)
                                              (e1)
                                              (e2 (+ x 1) (* a x))))
                   [() (finally (res) a (halt))]
                   [(x a) (go f)])])
       (simple-indep [(x 1) (a 1)] (go f)))
  res)

(define-cfg-syntax return
  (lambda (stx)
    (syntax-case stx ()
      [(_ return-var ...)
       (for-all identifier? #'(return-var ...))
       #'(finally (return-var ...) (values return-var ...) (halt))])))

(assert (equal? 1
                (cfg (simple-indep ([x 1]) (return x))
                  x)))

(assert (equal? '(outer outer)
                (let ([x 'outer] [y 'outer])
                  (cfg (let* ([c (permute ([p (finally (y) 'inner
                                                  (simple-indep ([a x]) (go p)))])
                                     (finally (a) a (halt)))])
                         (permute/tail [(p (finally (b) y
                                        (simple-indep ([x 'inner]) (go p))))]
                           (go c)))
                    (list a b)))))

(let ([x 'outer] [y 'outer])
  (cfg (let* ([c (permute ([p (finally (y) 'inner
                                  (simple-indep ([a x]) (go p)))])
                     (finally (a) a (halt)))])
         (permute/tail [(p (finally (b) y
                        (simple-indep ([x 'inner]) (go p))))]
           (go c)))
    (list a b)))

;;; Loop example

(define-cfg-syntax loop
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr lp-lbl loop-cfg-term body-cfg-term)
       (identifier? #'lp-lbl)
       #'(indep ([(n) n-expr])
           (letrec ([lp-lbl
                     (do
                         (lambda (loop done)
                           (if (zero? n)
                               (done)
                               (loop (- n 1))))
                       [(n) loop-cfg-term]
                       [() body-cfg-term])])
             (go lp-lbl)))])))

(assert (equal? 20
                (cfg
                    (indep ([(n) 0])
                      (loop 10 next
                            (indep ([(n) (+ n 2)])
                              (go next))
                            (finally (n) n (halt))))
                  n)))

(define-cfg-label next)

(define-cfg-syntax loop2
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr loop-cfg-term body-cfg-term)
       #'(indep ([(n) n-expr])
           (letrec ([(next)
                     (do
                         (lambda (loop done)
                           (if (zero? n)
                               (done)
                               (loop (- n 1))))
                       [(n) loop-cfg-term]
                       [() body-cfg-term])])
             (go (next))))])))

(assert (equal? 20
                (cfg
                    (indep ([(n) 0])
                      (loop2 10
                            (indep ([(n) (+ n 2)])
                              (go (next)))
                            (finally (n) n (halt))))
                  n)))

;;; Examples from the spec

(assert (equal? '(2 5)
                (let ([x 1] [y 2])
                  (cfg
                      (finally (y) (+ x 3)
                        (do (lambda (e)
                              (set! x y)
                              (e))
                          [() (halt)]))
                    (list x y)))))

(assert (equal? '(1 (2 3))
                (cfg
                    (finally (x . y) (values 1 2 3)
                      (halt))
                  (list x y))))

(assert (equal? 4
                (let ([x 1] [y 2])
                  (cfg
                      (do (lambda (e1 e2)
                            (if (odd? y) (e1 5) (e2)))
                        [(x) (finally (y) (+ x 2) (halt))]
                        [() (finally (y) (+ x 3) (halt))])
                    y))))

(assert (equal? '(2 1)
                (let ([x 1])
                  (cfg
                      (permute ([p (indep ([(x) 2])
                                     (go p))]
                                [p (indep ([(y) x])
                                     (go p))])
                        (finally (z) (list x y) (halt)))
                    z))))

;; Some more experiments

(assert
  (equal? 1
    (let ([x 1])
      (cfg
          (letrec ([l (permute ([p (indep ([(y) x])
                                     (go p))])
                        (finally (res) y
                          (halt)))])
            (permute/tail ([p (indep ([(x) 2])
                           (go p))])
              (go l)))
        res))))

(assert
  (equal? '(10 11 11 11)
    (let ([x 1000]
          [stop? #f]
          [zj #f]
          [ze #f]
          [zw #f])
      (cfg
          (letrec ([j (permute ([p
                                  (do (lambda (e1 e2)
                                        (if stop? (e1 x) (begin (set! stop? #t) (set! zj x) (e2 (fx+ x 1)))))
                                      [(x) (finally (zx) x (halt))]
                                    [(x) (go p)])])
                        (do (lambda (next) (set! zw x) (next))
                            [() (go e)]))]
                   [e (permute/tail ([p (do (lambda (next) (set! ze x) (next)) [() (go p)])])
                        (go j))])
            (indep ([(x) 10])
              (go j)))
        (list zj zw ze zx)))))
