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

(library (srfi :265 cfg expand)
  (export
    expand
    go
    branch
    return-values
    defer
    let
    let-values
    let*
    letrec
    permute
    permute/tail
    cfg-syntax-type
    cfg-transform)
  (import
    (rnrs)
    (srfi :265 cfg helpers)
    (srfi :265 cfg identifiers)
    (srfi :265 cfg syntax-types))

  (define-syntax cfg-transform
    (lambda (stx)
      (syntax-case stx ()
        [(_ k ... cfg-term)
         (syntax-violation #f "invalid cfg syntax" #'cfg-term)]
        [_ (syntax-violation #f "invalid cfg syntax" '(cfg ...))])))

  (define-syntax define-auxiliary-cfg-syntax
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define-syntax name
             (lambda (stx)
               (syntax-violation #f "invalid use of cfg syntax" stx)))])))

  (define-auxiliary-cfg-syntax go)
  (define-auxiliary-cfg-syntax branch)
  (define-auxiliary-cfg-syntax return-values)
  (define-auxiliary-cfg-syntax defer)
  (define-auxiliary-cfg-syntax permute)
  (define-auxiliary-cfg-syntax permute/tail)

  (define-syntax cfg-syntax-type
    (lambda (stx)
      (syntax-violation #f "invalid use of syntax" stx)))

  (define-syntax expand
    (make-transformer/type
      (lambda (stx type-guard)
        (define do-expand
          (lambda (k* kwd stx)
            (with-syntax ([(k ...) k*]
                          [cfg-stx stx])
              (syntax-case kwd
                  (go branch return-values defer let let-values let* letrec
                    permute permute/tail)
                [go (expand-go k* stx)]
                [branch (expand-branch k* stx)]
                [return-values (expand-return-values k* stx)]
                [defer (expand-defer k* stx)]
                [let (expand-let k* stx)]
                [let-values (expand-let-values k* stx)]
                [letrec (expand-letrec k* stx)]
                [let* (expand-let* k* stx)]
                [permute (expand-permute k* stx)]
                [permute/tail (expand-permute/tail k* stx)]
                [_
                  (type-guard #'cfg-syntax-type kwd)
                  (expand-macro k* kwd stx)]
                [_ (syntax-violation #f "invalid cfg syntax keyword" stx kwd)]))))
        (define expand-go
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ lbl)
               (label-expression? #'lbl)
               (with-syntax ([(k ...) k*])
                 #'(k ... (go lbl)))]
              [_ (syntax-violation 'go "invalid cfg syntax" stx)])))
        (define expand-branch
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([(target1 . formals1) cfg1]
                    [(target . formals) cfg] ...)
                  expr)
               (and (identifier? #'target1)
                    (for-all identifier? #'(target ...))
                    (formals? #'formals1)
                    (for-all formals? #'(formals ...)))
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-branch-step k ...
                     expr
                     (target1 target ...)
                     (formals1 formals ...)
                     (cfg1 cfg ...)
                     ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-return-values
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ [formals expr] ...)
               (for-all formals? #'(formals ...))
               (with-syntax ([(k ...) k*])
                 #'(k ... (return-values [formals expr] ...)))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-defer
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([(target1 . formals1) cfg1]
                    [(target . formals) cfg] ...)
                  expr successor)
               (and (identifier? #'target1)
                    (for-all identifier? #'(target ...))
                    (formals? #'formals1)
                    (for-all formals? #'(formals ...)))
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-defer-step k ...
                     expr
                     (target1 target ...)
                     (formals1 formals ...)
                     (cfg1 cfg ... successor)
                     ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-let
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ lbl ([var expr] ...) cfg)
               (and (identifier? #'lbl)
                    (for-all identifier? #'(var ...)))
               (with-syntax ([(k ...) k*])
                 #'(expand k ...
                     (letrec ([lbl cfg])
                       (let ([var expr] ...)
                         (go lbl)))))]
              [(_ ([var expr] ...) cfg)
               (for-all identifier? #'(var ...))
               (with-syntax ([(k ...) k*])
                 #'(expand k ...
                     (let-values ([(var) expr] ...) cfg)))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-let-values
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([formals expr] ...) cfg)
               (for-all formals? #'(formals ...))
               (with-syntax ([(k ...) k*]
                             [((var ...) ...)
                              (map formals->list #'(formals ...))])
                 #'(expand k ...
                     (branch ([(target var ... ...) cfg])
                       (let-values ([formals expr] ...)
                         (target var ... ...)))))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-let*
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([lbl cfg1] ...) cfg2)
               (for-all label-expression? #'(lbl ...))
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-let*-step k ... (lbl ...) (cfg1 ... cfg2) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-letrec
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([lbl cfg1] ...) cfg2)
               (for-all label-expression? #'(lbl ...))
               ;; XXX: The letrec should be distinct?
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-letrec-step k ... (lbl ...) (cfg1 ... cfg2) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-permute
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ () cfg)
               (label-expression? #'lbl)
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-permute-step k ... (cfg) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-permute/tail
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ ([lbl cfg1]) cfg2)
               (label-expression? #'lbl)
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-permute/tail-step k ... cfg2 lbl (cfg1 cfg2) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-macro
          (lambda (k* kwd stx)
            (with-syntax ([(k ...) k*]
                          [macro-keyword kwd]
                          [macro-use stx])
              #'(macro-keyword (cfg-transform expand k ... macro-use)))))
        (syntax-case stx ()
          [(_ k ... cfg-stx)
           (syntax-case #'cfg-stx ()
             [kwd
               (identifier? #'kwd)
               (do-expand #'(k ...) #'kwd #'cfg-stx)]
             [(kwd . args)
              (identifier? #'kwd)
              (do-expand #'(k ...) #'kwd #'cfg-stx)]
             [_ (syntax-violation #f "invalid cfg syntax" #'cfg-stx)])]))))

  (define-syntax expand-step
    (syntax-rules ()
      [(expand-step k ... () (cfg ...))
       (k ... (cfg ...))]
      [(expand-step k ... (cfg1 cfg2 ...) cfg3)
       (expand expand-cont k ... (cfg2 ...) cfg3 cfg1)]))

  (define-syntax expand-cont
    (syntax-rules ()
      [(expand-cont k ... cfg2 (cfg3 ...) cfg1)
       (expand-step k ... cfg2 (cfg3 ... cfg1))]))

  (define-syntax expand-branch-step
    (syntax-rules ()
      [(expand-branch-step k ... expr (target ...) (formals ...) (cfg ...))
       (k ... (branch ([(target . formals) cfg] ...) expr))]))

  (define-syntax expand-defer-step
    (syntax-rules ()
      [(expand-defer-step k ...
         expr
         (target ...)
         (formals ...)
         (branch ... successor))
       (k ... (defer ([(target . formals) branch] ...)
                expr
              successor))]))

  (define-syntax expand-let*-step
    (lambda (stx)
      (syntax-case stx ()
        [(_ k ... (lbl ...) (cfg1 ... cfg2))
         #`(k ...
             #,(fold-right
                 (lambda (lbl cfg tail)
                   #`(let* ([#,lbl #,cfg]) #,tail))
                 #'cfg2 #'(lbl ...) #'(cfg1 ...)))])))

  (define-syntax expand-letrec-step
    (syntax-rules ()
      [(expand-letrec-step k ... (lbl ...) (cfg1 ... cfg2))
       (k ... (letrec ([lbl cfg1] ...) cfg2))]))

  (define-syntax expand-permute-step
    (syntax-rules ()
      [(expand-permute-step k ... (cfg))
       (k ... (permute () cfg))]))

  (define-syntax expand-permute/tail-step
    (syntax-rules ()
      [(expand-permute/tail-step k ... stx lbl (cfg1 cfg2))
       (k ... (permute/tail stx ([lbl cfg1]) cfg2))])))
