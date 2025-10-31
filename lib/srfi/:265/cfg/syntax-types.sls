#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg syntax-types)
  (export
    define-syntax/type
    make-transformer/type)
  (import
    (rnrs))

  (define-syntax unknown-syntax-type
    (lambda (stx)
      (syntax-violation #f "invalid use of type identifier" stx)))

  (define-syntax define-syntax/type
    (lambda (stx)
      (syntax-case stx ()
        [(_ id type-id transformer-expr)
         (and (identifier? #'id) (identifier? #'type-id))
         #'(begin
             (define-syntax id transformer-expr))])))

  (define make-transformer/type
    (lambda (transformer)
      (lambda (stx)
        (define type-guard
          (lambda (type id)
            (assert (identifier? type))
            (assert (identifier? id))
            #t))
        (transformer stx type-guard)))))
