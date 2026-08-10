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

(library (srfi :265 cfg derived)
  (export
    return
    finally
    finally-values
    permute
    permute/tail)
  (import
    (rnrs)
    (srfi :265 cfg helpers)
    (srfi :265 cfg identifiers)
    (rename (srfi :265 cfg primitive)
      (permute primitive:permute)
      (permute/tail primitive:permute/tail)))

  (define-cfg-syntax return
    (lambda (stx)
      (syntax-case stx ()
        [(_ [var expr] ...)
         (for-all identifier? #'(var ...))
         #'(return-values [(var) expr] ...)]
        [_ (syntax-violation #f "invalid cfg syntax" stx)])))

  (define-cfg-syntax finally-values
    (let ()
      (define formals->values-expression
        (lambda (formals)
          (syntax-case formals ()
            [var
             (identifier? #'var)
             #'(apply values var)]
            [(var ...)
             (for-all identifier? #'(var ...))
             #'(values var ...)]
            [(var ... . rest)
             (and (for-all identifier? #'(var ...))
                  (identifier? #'rest))
             #'(apply values var ... rest)]
            [_ (assert #f)])))
      (lambda (stx)
        (syntax-case stx ()
          [(_ ([formals expr] ...) cfg)
           (for-all formals? #'(formals ...))
           (with-syntax ([((var ...) ...)
                          (map formals->list #'(formals ...))]
                         [(values-expr ...)
                          (map formals->values-expression
                            #'(formals ...))])
             #'(defer ([(finish var ... ...)
                         (return-values [formals values-expr] ...)])
                 (let-values ([formals expr] ...)
                   (finish var ... ...))
                 cfg))]
          [_ (syntax-violation #f "invalid cfg syntax" stx)]))))

  (define-cfg-syntax finally
    (lambda (stx)
      (syntax-case stx ()
        [(_ ([var expr] ...) cfg)
         (for-all identifier? #'(var ...))
         #'(finally-values ([(var) expr] ...) cfg)]
        [_ (syntax-violation #f "invalid cfg syntax" stx)])))

  (define-cfg-syntax permute
    (lambda (stx)
      (syntax-case stx ()
        [(_ [(lbl cfg1) ...] cfg2)
         (for-all label-expression? #'(lbl ...))
         (fold-right
           (lambda (lbl head tail)
             (with-syntax ([lbl lbl] [head head] [tail tail])
               #'(primitive:permute/tail ([lbl head]) tail)))
           #'(primitive:permute () cfg2) #'(lbl ...) #'(cfg1 ...))])))

  (define-cfg-syntax permute/tail
    (lambda (stx)
      (syntax-case stx ()
        [(_ [(lbl cfg1) ...] cfg2)
         (for-all label-expression? #'(lbl ...))
         (begin
           (fold-right
             (lambda (lbl head tail)
               (with-syntax ([lbl lbl] [head head] [tail tail])
                 #'(primitive:permute/tail ([lbl head]) tail)))
             #'cfg2 #'(lbl ...) #'(cfg1 ...)))]))))
