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

(library (srfi :265 cfg primitive)
  (export
    cfg
    go
    do
    finally
    halt
    let*
    letrec
    permute
    permute/tail
    define-cfg-syntax
    define-cfg-label)
  (import
    (rnrs)
    (srfi :265 cfg compile)
    (srfi :265 cfg expand)
    (srfi :265 cfg parse)
    (srfi :265 cfg syntax-types))

  (define-syntax cfg
    (lambda (stx)
      (syntax-case stx ()
        [(_ cfg-term result-expr)
         #'(expand cfg-step result-expr cfg-term)])))

  (define-syntax cfg-step
    (lambda (stx)
      (syntax-case stx ()
        [(_ result-expr cfg-term)
         (let ([ast (parse #'cfg-term)])
           ;; TODO: Add optimizer (e.g. for determining SCC).
           (compile! #'result-expr ast))]
        [_ (assert #f)])))

  (define-syntax define-cfg-syntax
    (lambda (stx)
      (syntax-case stx ()
        [(_ name transformer-expr)
         (identifier? #'name)
         #'(...
             (define-syntax/type name cfg-syntax-type
               (let ([transformer transformer-expr])
                 (unless (procedure? transformer)
                   (assertion-violation 'define-cfg-syntax
                     "invalid cfg syntax transformer" transformer))
                 (lambda (stx)
                   (syntax-case stx (cfg-transform)
                     [(_ (cfg-transform k ... cfg-term))
                      (with-syntax ([transformed-stx (transformer #'cfg-term)])
                        #'(k ... transformed-stx))]
                     [_ (syntax-violation #f "invalid use of cfg syntax" stx)])))))])))

  (define-syntax define-cfg-label
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define-syntax/type name cfg-label-type
             (lambda (stx)
               (syntax-violation #f "invalid use of cfg label" stx)))])))

  )
