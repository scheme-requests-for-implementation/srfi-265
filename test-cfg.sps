#!r6rs

; © 2025 Marc Nieper-Wißkirchen.
;
; SPDX-License-Identifier: MIT

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
  (rnrs eval)
  (srfi :265 cfg))

(define cfg-environment
  (environment '(rnrs) '(srfi :265 cfg)))

(define syntax-violation-raised?
  (lambda (expr)
    (guard (ex
             [(syntax-violation? ex) #t]
             [else #f])
      (eval expr cfg-environment)
      #f)))

(assert (syntax-violation-raised? '(cfg (halt) #f)))
(assert (syntax-violation-raised? '(cfg (return-values (x) 1) #f)))
(assert (syntax-violation-raised? '(cfg (return-values [() (values)] extra) #f)))
(assert (syntax-violation-raised? '(cfg (return-values [() (values)] [bad]) #f)))
(assert (syntax-violation-raised? '(cfg (return-values [(1) 1]) #f)))
(assert (syntax-violation-raised? '(cfg (return [(x) 1]) #f)))
(assert (syntax-violation-raised? '(cfg (return [1 1]) #f)))
(assert (syntax-violation-raised? '(cfg (return [x 1] extra) #f)))
(assert (syntax-violation-raised? '(cfg (let ([(x) 1]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (let ([x 1] [x 2]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (let-values ([(1) 1]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (let-values ([(x) 1] [(x) 2]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (finally (x) 1 (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (finally ([(x) 1]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (finally-values ([(1) 1]) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (finally-values ([(x) 1] [(x) 2]) (return)) #f)))
(assert (syntax-violation-raised? '(cfg (defer () (values) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(1) (return)]) (values) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(target 1) (return)]) (target 1) (return)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(target) (return)]
                  [(target) (return)])
            (target)
            (return))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(selected) (return)]
                   [(unselected) (return-values [() unselected])])
            (if #t
                (selected)
                (unselected))
            (return))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(selected) (return)])
            (selected)
            (return-values [() selected]))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(target x) (return)])
            (target x)
            (return))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(target x) (return)])
            (target 1)
            (return-values [() x]))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (defer ([(left x) (return)]
                   [(right) (return-values [() x])])
            (right)
            (return))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (permute/tail ([label
                           (defer ([(target) (return)])
                             (target)
                             (return))])
            (go label))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (permute/tail ([label
                           (finally ([x 1])
                             (return))])
            (go label))
       #f)))

(assert (equal? 1
          (cfg (return)
            1)))

(assert (equal? 2
                (cfg (return [x 2])
                  x)))

(assert (equal? '(2 1)
                (let ([x 1] [y 2])
                  (cfg (return [x y] [y x])
                    (list x y)))))

(assert (equal? '(1 . 2)
                (let ([x 2])
                  (cfg (return-values [(x) (cons 1 x)])
                    x))))

(assert (equal? '(1 2 3)
                (cfg (return-values [values (values 1 2 3)])
                  values)))

(assert (equal? '(outer . outer)
                (let ([x 'outer])
                  (cfg (return-values [(x) (cons x x)])
                    x))))

;;; Branch

(assert (syntax-violation-raised? '(cfg (branch () (values)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (branch ([(1) (return)]) (values)) #f)))
(assert
  (syntax-violation-raised?
    '(cfg (branch ([(target) (return)]
                   [(target) (return)])
            (target))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (branch ([(target) (return-values [() target])])
            (target))
       #f)))
(assert
  (syntax-violation-raised?
    '(cfg (branch ([(target x) (return)])
            (target x))
       #f)))

(assert (equal? 5
                (cfg (branch ([(next x) (return-values [(y) (+ x 1)])])
                       (next 4))
                  y)))

(assert (equal? '(left 10)
                (cfg (branch ([(left)
                                (return-values [(side) 'left] [(value) 10])]
                               [(right)
                                (return-values [(side) 'right] [(value) 20])])
                       (left))
                  (list side value))))

(assert (equal? '(1 2 3)
                (cfg (branch ([(finish head . tail)
                                (return-values [(result) (cons head tail)])])
                       (finish 1 2 3))
                  result)))

(assert (equal? '(1 2 3)
                (cfg (branch ([(finish . values)
                                (return-values [(result) values])])
                       (apply finish '(1 2 3)))
                  result)))

(assert (equal? 'outer
                (let ([finish (lambda () 'outer)])
                  (cfg (branch ([(finish)
                                  (return-values [(result) (finish)])])
                         (finish))
                    result))))

(assert (equal? '(1 10)
                (let ([x 10])
                  (cfg (return-values [(x) 1] [(y) x])
                    (list x y)))))

(assert (equal? '(9 1)
                (let ([count 0])
                  (cfg (return-values [(x)
                                (begin
                                  (set! count (+ count 1))
                                  9)])
                    (list x count)))))

(assert (equal? 1
                (let ([count 0])
                  (cfg (return-values [()
                                (begin
                                  (set! count (+ count 1))
                                  (values))])
                    count))))

;;; Defer

(assert (equal? 9
                (cfg
                    (defer ([(finish)
                              (return-values [(result) (+ r 1)])])
                      (finish)
                      (return-values [(r) 8]))
                  result)))

;; An ordinary Scheme target formal lexically shadows an inherited CFG
;; binding in its target term without changing the CFG state.
(assert (equal? '(2 3)
                (cfg
                    (defer ([(finish x)
                              (return-values [(seen) x])])
                      (finish 3)
                      (return-values [(x) 2]))
                  (list x seen))))

;; A later backward definition uses and then shadows the inherited binding.
(assert (equal? 3
                (cfg
                    (defer ([(finish)
                              (return-values [(x) (+ x 1)])])
                      (finish)
                      (return-values [(x) 2]))
                  x)))

;; A backward definition is in scope in the corresponding CFG term only
;; when it is bound on every possible return from the last CFG term.
(assert
  (syntax-violation-raised?
    '(cfg
         (defer ([(finish)
                   (return-values [(result) r])])
           (finish)
           (branch ([(with-r) (return-values [(r) 1])]
                    [(without-r) (return)])
             (with-r)))
       result)))

(assert (equal? 5
                (cfg (defer ([(finish x)
                               (return-values [(result) x])])
                       (finish 5)
                       (return))
                  result)))

(assert (equal? '(1 2 3)
                (cfg (defer ([(finish head . tail)
                               (return-values [(result) (cons head tail)])])
                       (finish 1 2 3)
                       (return))
                  result)))

(assert (equal? '(1 2 3)
                (cfg (defer ([(finish . values)
                               (return-values [(result) values])])
                       (apply finish '(1 2 3))
                       (return))
                  result)))

(assert (equal? '(10 10 11)
                (cfg
                    (defer ([(finish x y)
                              (return-values [(x) x] [(y) y])])
                      (let-values ([(x y) (values r (+ r 1))])
                        (finish x y))
                      (return-values [(r) 10]))
                  (list r x y))))

(assert (equal? '(10 20 30)
                (cfg
                    (defer ([(finish x tail)
                              (return-values [(x) x] [(tail) tail])])
                      (let-values ([(x . tail) (values r 20 30)])
                        (finish x tail))
                      (return-values [(r) 10]))
                  (cons x tail))))

(assert (equal? '(10 20 30)
                (cfg
                    (defer ([(finish all)
                              (return-values [(all) all])])
                      (let-values ([all (values r 20 30)])
                        (finish all))
                      (return-values [(r) 10]))
                  all)))

(assert (equal? 'right
                (cfg (defer ([(left)
                               (return-values [(result) 'left])]
                              [(right)
                               (return-values [(result) 'right])])
                       (right)
                       (return))
                  result)))

(assert (equal? '(successor deferred branch)
                (let ([events '()])
                  (cfg
                      (defer ([(finish)
                                (return-values [()
                                          (begin
                                            (set! events
                                              (cons 'branch events))
                                            (values))])])
                        (begin
                          (set! events (cons 'deferred events))
                          (finish))
                        (return-values [()
                                  (begin
                                    (set! events
                                      (cons 'successor events))
                                    (values))]))
                    (reverse events)))))

(assert (equal? '(12 (argument branch))
                (let ([events '()])
                  (cfg
                      (defer ([(finish value)
                                (branch ([(next)
                                          (return-values [(result) value])])
                                  (begin
                                    (set! events
                                      (cons 'branch events))
                                    (next)))])
                        (finish
                          (begin
                            (set! events (cons 'argument events))
                            12))
                        (return))
                    (list result (reverse events))))))

(assert (equal? '(before branch after)
                (let ([events '()])
                  (cfg
                      (defer ([(finish)
                                (return-values [()
                                          (begin
                                            (set! events
                                              (cons 'branch events))
                                            (values))])])
                        (dynamic-wind
                          (lambda ()
                            (set! events (cons 'before events)))
                          (lambda ()
                            (finish))
                          (lambda ()
                            (set! events (cons 'after events))))
                        (return))
                    (reverse events)))))

(define-syntax invoke-defer-target
  (syntax-rules ()
    [(_ target arg ...)
     (target arg ...)]))

(assert (equal? 6
                (cfg (defer ([(finish x)
                               (return-values [(result) x])])
                       (if #t
                           (invoke-defer-target finish 6)
                           (finish 0))
                       (return))
                  result)))

(define-cfg-syntax defer-value
  (lambda (stx)
    (syntax-case stx ()
      [(_ result expr)
       (identifier? #'result)
       #'(defer ([(finish value)
                   (return-values [(result) value])])
           (finish expr)
           (return))])))

(assert (equal? 7
                (cfg (defer-value result 7)
                  result)))

(assert (equal? '(outer outer)
                (let ([finish (lambda () 'outer)])
                  (cfg
                      (defer ([(finish)
                                (return-values [(branch-value) (finish)])])
                        (finish)
                        (return-values [(successor-value) (finish)]))
                    (list successor-value branch-value)))))

(assert (equal? 1
                (cfg
                    (branch ([(enter x)
                       (defer ([(finish)
                                 (return-values [(result) x])])
                         (finish)
                         (branch ([(next x) (return)])
                           (next 2)))])
                      (enter 1))
                  result)))

(assert (equal? 'outer
                (let ([x 'outer])
                  (cfg
                      (defer ([(finish value)
                                (return-values [(result) value])])
                        (finish x)
                        (branch ([(next x) (return)])
                          (next 'successor)))
                    result))))

;; The last CFG term's new binding of `x' enters the corresponding CFG
;; term and shadows the older forward binding there.
(assert (equal? '(2 2)
                (cfg
                    (branch ([(enter x)
                       (defer ([(finish)
                                 (return-values [(result) x])])
                         (finish)
                         (return-values [(x) 2]))])
                      (enter 1))
                  (list x result))))

(assert (equal? '(2 2)
                (cfg
                    (branch ([(enter x)
                       (defer ([(finish returned-x)
                                 (branch ([(next saved-x)
                                    (return-values
                                      [(result)
                                       (list returned-x saved-x)])])
                                   (next x))])
                         (finish x)
                         (return-values [(x) 2]))])
                      (enter 1))
                  result)))

(assert (equal? 2
                (cfg
                    (branch ([(enter x)
                       (defer ([(finish value)
                                 (return-values [(result) value])])
                         (finish x)
                         (return-values [(x) 2]))])
                      (enter 1))
                  result)))

(assert (equal? '(inner inner)
                (let ([r 'outer])
                  (cfg
                      (defer ([(finish)
                                (branch ([(next forward-r)
                                   (return-values
                                     [(result) (list r forward-r)])])
                                  (next r))])
                        (finish)
                        (return-values [(r) 'inner]))
                    result))))

(assert (equal? '(outer outer)
                (let ([x 'outer])
                  (cfg
                      (defer ([(finish x)
                                (return-values
                                  [(branch-value) x])])
                        (finish x)
                        (return-values [(successor-value) x]))
                    (list successor-value branch-value)))))

(assert (equal? 4
                (cfg
                    (defer ([(finish) (return)])
                      (finish)
                      (return-values [(result) 4]))
                  result)))

(assert (equal? '(11 10)
                (cfg
                    (defer ([(finish prior-r)
                              (return-values
                                [(r) (+ prior-r 1)]
                                [(s) prior-r])])
                      (finish r)
                      (return-values [(r) 10]))
                  (list r s))))

(assert (equal? '(10 11)
                (cfg
                    (defer ([(finish prior-r)
                              (defer ([(finish-s s)
                                        (return-values [(s) s])])
                                (finish-s (+ prior-r 1))
                                (return))])
                      (finish r)
                      (return-values [(r) 10]))
                  (list r s))))

(assert (equal? '(outer inner)
                (cfg
                    (defer ([(finish-outer trace)
                              (return-values
                                [(trace) (cons 'outer trace)])])
                      (finish-outer trace)
                      (defer ([(finish-inner trace)
                                (return-values
                                  [(trace) (cons 'inner trace)])])
                        (finish-inner trace)
                        (return-values [(trace) '()])))
                  trace)))

(assert (equal? 'outer
                (let ([r 'outer])
                  (cfg
                      (defer ([(finish)
                                (return-values [(result) r])])
                        (finish)
                        (branch ([(has-r) (return-values [(r) 'inner])]
                                 [(no-r) (return)])
                          (has-r)))
                    result))))

(assert (equal? 'outer
                (let ([result 'outer])
                  (cfg
                      (defer ([(selected)
                                (return-values [(result) 'inner])]
                               [(unselected)
                                (return)])
                        (selected)
                      (return))
                    result))))

(assert (equal? 'outer
                (let ([x 'outer])
                  (cfg
                      (defer ([(finish x) (return)])
                        (finish 'branch)
                        (return))
                    x))))

(assert (equal? 'inner
                (cfg
                    (defer ([(selected)
                              (return-values [(result) 'inner])]
                             [(unselected)
                              (return-values [(result) 'other])])
                      (selected)
                      (return))
                  result)))

;; A backward definition carried into the corresponding CFG term is also
;; carried by `go' to a label outside that term.
(assert (equal? 8
                (cfg
                    (letrec ([label
                               (return-values [(result) r])])
                      (defer ([(finish)
                                (go label)])
                        (finish)
                        (return-values [(r) 8])))
                  result)))

(assert (equal? '(8 1)
                (cfg
                    (letrec ([label
                               (return-values [(result) 1])])
                      (defer ([(finish)
                                (go label)])
                        (finish)
                        (return-values [(r) 8])))
                  (list r result))))

(assert (equal? 8
                (cfg
                    (defer ([(finish)
                              (letrec ([label
                                         (return-values [(result) r])])
                                (go label))])
                      (finish)
                      (return-values [(r) 8]))
                  result)))

(assert (equal? 3
                (cfg
                    (defer ([(finish r)
                              (letrec ([label
                                         (return-values [(result) r])])
                                (go label))])
                      (finish 3)
                      (return-values [(r) 8]))
                  result)))

;; The target formal shadows an inherited same-named CFG binding locally,
;; but `go' carries the CFG binding, rather than the formal, to a label
;; outside the formal's lexical region.
(assert (equal? 8
                (cfg
                    (letrec ([label
                               (return-values [(result) r])])
                      (defer ([(finish r)
                                (go label)])
                        (finish 1)
                        (return-values [(r) 8])))
                  result)))

(assert (equal? 'outer
                (let ([r 'outer])
                  (cfg
                      (letrec ([label
                                 (return-values [(result) r])])
                        (branch ([(via-defer)
                           (defer ([(finish)
                                     (go label)])
                             (finish)
                             (return-values [(r) 'inner]))]
                                 [(directly)
                                  (go label)])
                          (via-defer)))
                    result))))

(assert (equal? 'inner
                (let ([r 'outer])
                  (cfg
                      (letrec ([label
                                 (return-values [(result) 'direct])])
                        (branch ([(via-defer)
                           (defer ([(finish r)
                                     (return-values [(result) r])])
                             (finish r)
                             (defer ([(through-label)
                                       (go label)])
                               (through-label)
                               (return-values [(r) 'inner])))]
                                 [(directly)
                                  (go label)])
                          (via-defer)))
                    result))))

;; A defer target's Scheme formals do not flow through `go' to a label
;; outside their lexical region.
(assert
  (syntax-violation-raised?
    '(cfg
         (letrec ([label
                    (return-values [(result) x])])
           (defer ([(left x) (go label)]
                    [(right x) (go label)])
             (left 1)
             (return)))
       result)))

(assert (equal? 'outer
                (let ([x 'outer])
                  (cfg
                      (letrec ([label
                                 (return-values [(result) x])])
                        (defer ([(left x) (go label)]
                                 [(right) (go label)])
                          (left 1)
                          (return)))
                    result))))

(assert (equal? 5
                (cfg
                    (branch ([(enter x)
                       (letrec ([label
                                  (return-values [(result) x])])
                         (defer ([(left) (go label)]
                                  [(right) (go label)])
                           (left)
                           (return)))])
                      (enter 5))
                  result)))

(assert (equal? 3
                (let ([count 0])
                  (cfg
                      (letrec ([label
                                 (defer ([(again) (go label)]
                                          [(stop) (return)])
                                   (if (< count 2)
                                       (begin
                                         (set! count (+ count 1))
                                         (again))
                                       (stop))
                                   (return-values [(result) (+ count 1)]))])
                        (go label))
                    result))))

;; The selected permutation component returns `b' directly; its label's
;; narrower tail is not on that path, so the outer `defer' sees returned `b'.
(assert (equal? 2
                (let ([b 'outer])
                  (cfg
                      (defer ([(done b)
                                (return-values [(b) b])])
                          (done b)
                        (defer ([(finish a b)
                                  (permute/tail ([label
                                                   (return-values
                                                     [(a) a] [(b) b])])
                                    (permute () (return-values [(a) a])))])
                            (finish 1 2)
                          (return)))
                    b))))

(assert
  (eq? #t
    (cfg
        (branch ([(left x)
           (permute/tail ([unused (return)])
             (permute () (return)))]
                 [(right y) (return)])
          (left 1))
      #t)))

(assert
  (eq? #t
    (cfg
        (branch ([(continue x)
           (permute/tail ([unused (return)])
             (permute () (return)))])
          (continue 1))
      #t)))

(assert (equal? 9
                (cfg
                    (defer ([(finish)
                              (permute/tail ([label (go label)])
                                (permute () (return)))])
                      (finish)
                      (return-values [(result) 9]))
                  result)))

(assert (equal? 9
                (cfg
                    (defer ([(finish)
                              (let* ([label (return)])
                                (go label))])
                      (finish)
                      (return-values [(result) 9]))
                  result)))

(assert (equal? 10
                (cfg
                    (defer ([(finish result)
                              (return-values [(result) result])])
                      (finish result)
                      (permute/tail ([label (go label)])
                        (permute () (return-values [(result) 10]))))
                  result)))

(assert (equal? '(3 . 3)
                (cfg
                    (letrec ([l (return-values [(x) 3])])
                      (defer ([(finish x)
                                (return-values [(x) x])])
                        (finish (cons x x))
                        (go l)))
                  x)))

(assert (equal? '(4 . 5)
                (cfg (branch ([(e x) (return-values [(x) (cons x 5)])])
                       (e 4))
                  x)))

(assert (equal? '(6 . 7)
                (let ([x 6])
                  (cfg (branch ([(e x) (return-values [(x) x])])
                         (e (cons x 7)))
                    x))))

(assert (equal? 7
                (let ([x 7])
                  (cfg (branch ([(e1) (return-values [(x) x])]
                                [(e2 x) (return-values [(x) x])])
                         (e1))
                    x))))

(assert (equal? '(8 . 10)
                (let ([x 8])
                  (cfg (letrec ([l (return-values [(x) (cons x y)])])
                         (branch ([(e1 y) (go l)]
                                  [(e2 x y) (go l)])
                           (e2 9 10)))
                    x))))

(assert (equal? '(12 . 13)
                (cfg (branch ([(e1 x)
                        (defer ([(finish x)
                                  (return-values [(x) x])])
                          (finish (cons x 13))
                          (return-values [(x) (+ x 1)]))])
                       (e1 11))
                  x)))

(assert (equal? 14
                (cfg (permute ([l (go l)])
                       (return-values [(x) 14]))
                  x)))

(assert (equal? '(15 . 15)
                (let ([x 15])
                  (cfg (permute ([l (branch ([(e y) (go l)])
                                        (e x))])
                         (return-values [(x) (cons y x)]))
                    x))))

(assert (equal? '(15 . 16)
                (let ([x 15])
                  (cfg (permute/tail ([l (branch ([(e x) (go l)])
                                        (e 16))])
                         (permute ([l (branch ([(e y) (go l)])
                                          (e x))])
                           (return-values [(x) (cons y x)])))
                    x))))

(assert (equal? '(20 (0 . 17) (17 18 20))
                (let ([x 17]
                      [z 0])
                  (cfg
                      (permute/tail ([l1 (branch ([(e z) (go l1)])
                                               (e 21))])
                        (permute/tail ([l1 (permute/tail ([l2 (branch
                                                                ([(e x) (go l2)])
                                                                (e 18))])
                                             (permute ([l2 (branch
                                                             ([(e y z) (go l2)])
                                                             (e x 20))])
                                               (branch ([(e x) (go l1)])
                                                 (e (list y x z)))))])
                          (permute ([l1 (branch ([(e y) (go l1)])
                                            (e (cons z x)))])
                            (return-values [(x) (list z y x)]))))
                    x))))

(assert (equal? 21
                (cfg (letrec ([l (return-values [(x) x])])
                       (permute ([l (branch ([(e x) (go l)])
                                        (e 21))])
                         (go l)))
                  x)))

(assert (equal? '(21 . 22)
                (cfg (let* [(l (return-values [(x) 21]))]
                       (let* [(l (defer ([(finish x)
                                          (return-values [(x) x])])
                                    (finish (cons x 22))
                                    (go l)))]
                         (go l)))
                  x)))

(assert (equal? '(23 24)
                (let ([x 23])
                  (cfg (let* [(l (permute ([l (branch ([(e y) (go l)])
                                                           (e x))])
                                     (return-values [(x) (list y x)])))]
                         (permute/tail ([l (branch ([(e x) (go l)])
                                                 (e 24))])
                           (go l)))
                    x))))

(assert (equal? '(25 . 26)
                (let ([x 25])
                  (cfg (permute ([l (branch ([(e x) (go l)])
                                        (e 26))]
                                 [l (branch ([(e y) (go l)])
                                        (e x))])
                         (return-values [(x) (cons y x)]))
                    x))))

(define-cfg-syntax simple-let
  (lambda (stx)
    (syntax-case stx ()
      [(_ [(id init) ...] cfg)
       (for-all identifier? #'(id ...))
       #'(branch ([(e id ...) cfg])
           (e init ...))])))

(assert (equal? 27
                (let ([x 26])
                  (cfg (simple-let [(x (fx+ x 1))] (return-values [(x) x]))
                    x))))

(assert (equal? '30
                (cfg (simple-let [(x 10)]
                       (defer ([(finish y)
                                 (return-values [(y) y])])
                         (finish x)
                         (return-values [(x) (+ x 20)])))
                  y)))

(assert (equal? '(49 (1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   [(l (simple-let [(x (cons 1 x))] (go l)))]
                                 (simple-let [(z (cons 3 x))] (go l)))])
                         (simple-let [(u (cons 5 x))]
                               (return-values [(x) (list 49 x z u)])))
                    x))))

(assert (equal? '((1 . 28) (3 . (1 . 28)) (5 . (1 . 28)))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-let [(x (cons 1 x))] (go l)))]
                                         (simple-let [(z (cons 3 x))] (go l)))])
                                 (simple-let [(u (cons 5 x))] (go l)))])
                         (return-values [(x) (list x z u)]))
                    x))))

(assert (equal? '50
                (cfg (permute
                         ([l (simple-let [(z 50)] (go l))]
                          [l (go l)])
                       (return-values [(x) z]))
                  x)))

(assert (equal? '((1 . 28) (2 . 28) (3 . (1 . 28)) (4 . 28) (5 . (1 . 28)) (6 . 28))
                (let ([x 28])
                  (cfg (permute
                           ([l (permute
                                   ([l (permute
                                           [(l (simple-let [(x (cons 1 x))] (go l)))
                                            (l (simple-let [(y (cons 2 x))] (go l)))]
                                         (simple-let [(z (cons 3 x))] (go l)))]
                                    [l (simple-let [(w (cons 4 x))] (go l))])
                                 (simple-let [(u (cons 5 x))] (go l)))]
                            [l (simple-let [(v (cons 6 x))] (go l))])
                         (return-values [(x) (list x y z w u v)]))
                    x))))

(assert (equal? '(2 30)
          (cfg (simple-let ([x 1]) (letrec ([l (permute ([p (simple-let ([y 30]) (go p))]
                                                          [p (branch ([(e1 x) (go l)]
                                                                      [(e2) (go p)])
                                                               (if (eqv? x 1)
                                                                   (e1 2)
                                                                   (e2)))])
                                                  (return-values [(x) (list x y)]))])
                                      (go l)))
            x)))

;;; Permute and defer variables.

(assert (equal? 45
                (let ([x 35])
                  (cfg (permute ([p (defer ([(finish x)
                                             (return-values [(x) x])])
                                       (finish (+ 10 x))
                                       (go p))]
                                 [p (defer ([(finish x)
                                             (return-values [(x) x])])
                                       (finish (+ 10 x))
                                       (go p))])
                         (return))
                    x))))

;;; Let and let-values

(assert (equal? '(2 1)
                (let ([x 1] [y 2])
                  (cfg (let ([x y] [y x])
                         (return-values [(result) (list x y)]))
                    result))))

(assert (equal? '(2 1)
                (let ([x 1] [y 2])
                  (cfg (let-values ([(x y) (values y x)])
                         (return-values [(result) (list x y)]))
                    result))))

(assert (equal? 120
                (cfg
                    (let loop ([n 5] [acc 1])
                      (branch ([(again n acc) (go loop)]
                               [(done) (return-values [(result) acc])])
                        (if (zero? n)
                            (done)
                            (again (- n 1) (* acc n)))))
                  result)))

(assert (equal? '((1 2) 4)
                (cfg (simple-let ([x 1]) (let-values ([x (values x 2)] [(y) (+ x 3)])
                                     (return-values [(x) (list x y)])))
                  x)))

;;; Finally and finally-values

(assert (equal? '(3 4)
                (cfg (finally ([x (+ x 1)] [y (+ x 2)])
                       (return-values [(x) 2]))
                  (list x y))))

(assert (equal? '(2 1)
                (let ([x 1] [y 2])
                  (cfg (finally ([x y] [y x])
                         (return))
                    (list x y)))))

(assert (equal? '(1 2 3 4 5 6)
                (cfg
                    (finally-values ([(x y) (values 1 2)]
                                     [(z . tail) (values 3 4 5)]
                                     [all (values 6)])
                      (return))
                  (append (list x y z) tail all))))

(assert (equal? 12
                (cfg (finally ([x (+ x 1)])
                       (finally ([x (+ x 10)])
                         (return-values [(x) 1])))
                  x)))

(assert (equal? 1
                (cfg (finally-values ([() (values)])
                       (return-values [(x) 1]))
                  x)))

;;; Let*

(assert (equal? 1
                (cfg (let* ([p (return-values [(x) 1])]
                              [p (go p)])
                       (go p))
                  x)))

;;; Examples from specification

(define ex1
  (lambda (n*)
    (cfg (let f ([n* n*] [e 0] [o 0])
           (branch ([(e1 n n*)
                      (branch ([(e1 e) (go f)]
                               [(e2 o) (go f)])
                        (if (odd? n)
                            (e2 (+ o 1))
                            (e1 (+ e 1))))]
                     [(e2)
                      (return-values [(e o) (values e o)])])
             (if (null? n*)
                 (e2)
                 (e1 (car n*) (cdr n*)))))
      (values e o))))

(define ex2
  (lambda (n*)
    (cfg (let f ([n* n*])
           (branch ([(e1 n n*)
                      (branch ([(e1)
                                 (finally ([e* (cons n e*)])
                                   (go f))]
                               [(e2)
                                 (finally ([o* (cons n o*)])
                                   (go f))])
                        (if (odd? n)
                            (e2)
                            (e1)))]
                     [(e2)
                      (return-values [(e* o*) (values '() '())])])
             (if (null? n*)
                 (e2)
                 (e1 (car n*) (cdr n*)))))
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
                                   (cfg (letrec ([(p) (return [x 40])])
                                          (go (d)))
                                     x)])])
                  (k p))))

(define-syntax permuting
  (lambda (stx)
    (syntax-case stx ()
      [(_ cfg-term ... result-expr)
       #'(cfg (permute ([(p) cfg-term] ...)
                (return [res result-expr]))
           res)])))

(assert (equal? 99
          (permuting (simple-let ([x 99]) (go (p))) x)))

;;; More examples from spec

(assert (equal? '(2 3)
                (let ([x 1])
                  (cfg (return-values [(x y) (values (+ x 1) (+ x 2))])
                    (list x y)))))

(assert (equal? 2
                (let ([x 1])
                  (cfg (branch ([(e x) (return [res x])])
                         (e (+ x 1)))
                    res))))

(assert (equal? 'odd
                (let ([x 1])
                  (cfg (branch ([(e1) (return [res 'even])]
                                [(e2 a) (return [res a])])
                         (if (even? x) (e1) (e2 'odd)))
                    res))))

(assert (equal? 'outer
                (let ([a 'outer]
                      [x 1])
                  (cfg (defer ([(finish res)
                                 (return [res res])])
                         (finish a)
                         (branch ([(e1) (return [res 'even])]
                                  [(e2 a) (return)])
                           (if (even? x) (e1) (e2 'odd))))
                    res))))

(assert (equal? 'outer
                (let ([res 'outer]
                      [x 1])
                  (cfg (branch ([(e1) (return)]
                                [(e2 a) (return [res a])])
                         (if (even? x) (e1) (e2 'odd)))
                    res))))

(assert (equal? 720
                (cfg (let f ([x 1] [a 1])
                       (branch ([(e1) (return [res a])]
                                [(e2 x a) (go f)])
                         (if (> x 6)
                             (e1)
                             (e2 (+ x 1) (* a x)))))
                     res)))

(define-cfg-syntax return-variables
  (lambda (stx)
    (syntax-case stx ()
      [(_ return-var ...)
       (for-all identifier? #'(return-var ...))
       #'(return [return-var return-var] ...)])))

(assert (equal? 1
                (cfg (simple-let ([x 1]) (return-variables x))
                  x)))

(assert (equal? '(1 2)
                (cfg (simple-let ([x 1] [y 2]) (return-variables x y))
                  (list x y))))

(assert (equal? '(outer outer)
                (let ([x 'outer] [y 'outer])
                  (cfg (let* ([c (permute ([p (finally ([y 'inner])
                                                 (let ([a x]) (go p)))])
                                     (return [a a]))])
                         (permute/tail [(p (finally ([b y])
                                             (let ([x 'inner]) (go p))))]
                           (go c)))
                    (list a b)))))

;;; Loop example

(define-cfg-syntax loop
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr lp-lbl loop-cfg-term body-cfg-term)
       (identifier? #'lp-lbl)
       #'(let lp-lbl ([n n-expr])
           (branch ([(loop n) loop-cfg-term]
                    [(done) body-cfg-term])
             (if (zero? n)
                 (done)
                 (loop (- n 1)))))])))

(assert (equal? 20
                (cfg
                    (let ([n 0])
                      (loop 10 next
                            (let ([n (+ n 2)])
                              (go next))
                            (return [n n])))
                  n)))

(define-cfg-label next)

(define-cfg-syntax loop2
  (lambda (stx)
    (syntax-case stx ()
      [(_ n-expr loop-cfg-term body-cfg-term)
       #'(let ([n n-expr])
           (letrec ([(next)
                     (branch ([(loop n) loop-cfg-term]
                              [(done) body-cfg-term])
                       (if (zero? n)
                           (done)
                           (loop (- n 1))))])
             (go (next))))])))

(assert (equal? 20
                (cfg
                    (let ([n 0])
                      (loop2 10
                            (let ([n (+ n 2)])
                              (go (next)))
                            (return [n n])))
                  n)))

;;; Examples from the spec

(assert (equal? '(2 4)
                (let ([x 1])
                  (cfg
                      (finally ([y (+ x 2)])
                        (return [x (+ x 1)]))
                    (list x y)))))

(assert (equal? '(2 5)
                (let ([x 1] [y 2])
                  (cfg
                      (defer ([(finish y)
                                (return [y y])])
                          (finish (+ x 3))
                        (branch ([(e) (return)])
                          (begin
                            (set! x y)
                            (e))))
                    (list x y)))))

(assert (equal? 13
                (let ([x 1] [y 2])
                  (cfg
                      (finally ([x 3])
                        (branch ([(e1) (return)]
                                 [(e2) (return)])
                          (if (odd? y) (e1) (e2))))
                    (+ x 10)))))

(assert (equal? 11
                (let ([x 1])
                  (cfg
                      (branch ([(e x)
                         (defer ([(finish x)
                                   (return [x x])])
                             (finish (+ x 2))
                           (return [x (+ x 3)]))])
                        (e (+ x 1)))
                    (+ x 4)))))

(assert (equal? 42
                (let ([retry? #t])
                  (cfg
                      (letrec ([l
                                 (defer ([(again) (go l)]
                                          [(done) (return)])
                                   (if retry?
                                       (begin
                                         (set! retry? #f)
                                         (again))
                                       (done))
                                   (return [result 42]))])
                        (go l))
                    result))))

(assert (equal? '(1 (2 3))
                (cfg
                    (return-values [(x . y) (values 1 2 3)])
                  (list x y))))

(assert (equal? 4
                (let ([x 1] [y 2])
                  (cfg
                      (branch ([(e1 x) (return [y (+ x 2)])]
                               [(e2) (return [y (+ x 3)])])
                        (if (odd? y) (e1 5) (e2)))
                    y))))

(assert (equal? '(2 1)
                (let ([x 1])
                  (cfg
                      (permute ([p (let ([x 2])
                                     (go p))]
                                [p (let ([y x])
                                     (go p))])
                        (return [z (list x y)]))
                    z))))

(assert (equal? '(2 1)
                (let ([x 1])
                  (cfg
                      (permute ([p (finally ([y x])
                                      (go p))]
                                [p (finally ([x 2])
                                      (go p))])
                        (return))
                    (list x y)))))

;; Some more experiments

(assert
  (equal? 1
    (let ([x 1])
      (cfg
          (letrec ([l (permute ([p (let ([y x])
                                     (go p))])
                        (return [res y]))])
            (permute/tail ([p (let ([x 2])
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
                                  (branch ([(e1 x) (return [zx x])]
                                           [(e2 x) (go p)])
                                    (if stop? (e1 x) (begin (set! stop? #t) (set! zj x) (e2 (fx+ x 1)))))])
                        (branch ([(next) (go e)])
                          (begin (set! zw x) (next))))]
                   [e (permute/tail ([p (branch ([(next) (go p)])
                                          (begin (set! ze x) (next)))])
                        (go j))])
            (let ([x 10])
              (go j)))
        (list zj zw ze zx)))))
