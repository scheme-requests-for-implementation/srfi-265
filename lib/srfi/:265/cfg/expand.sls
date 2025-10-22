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

(library (srfi :265 cfg expand)
  (export
    expand
    go
    do
    finally
    halt
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
  (define-auxiliary-cfg-syntax finally)
  (define-auxiliary-cfg-syntax halt)
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
                  (go do finally halt let* letrec permute permute/tail)
                [go (expand-go k* stx)]
                [do (expand-do k* stx)]
                [finally (expand-finally k* stx)]
                [halt (expand-halt k* stx)]
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
        (define expand-do
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ proc-expr [formals cfg] ...)
               (for-all formals? #'(formals ...))
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-do-step k ...
                     proc-expr [formals ...] (cfg ...) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-finally
          (lambda (k* stx)
            (syntax-case stx ()
              [(_ formals expr cfg)
               (formals? #'formals)
               (with-syntax ([(k ...) k*])
                 #'(expand-step expand-finally-step k ... formals expr (cfg) ()))]
              [_ (syntax-violation #f "invalid cfg syntax" stx)])))
        (define expand-halt
          (lambda (k* stx)
            (syntax-case stx ()
              [(_)
               (with-syntax ([(k ...) k*])
                 #'(k ... (halt)))]
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

  (define-syntax expand-do-step
    (syntax-rules ()
      [(expand-do-step k ... proc-expr [formals ...] (cfg ...))
       (k ... (do proc-expr [formals cfg] ...))]))

  (define-syntax expand-finally-step
    (syntax-rules ()
      [(expand-finally-step k ... formals expr (cfg))
       (k ... (finally formals expr cfg))]))

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
