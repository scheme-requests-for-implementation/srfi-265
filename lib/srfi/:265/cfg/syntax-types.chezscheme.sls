#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg syntax-types)
  (export
    define-syntax/type
    make-transformer/type)
  (import
    (rnrs)
    (only (chezscheme) define-property))

  (define-syntax syntax-type
    (lambda (stx)
      (syntax-violation #f "invalid use of syntax" stx)))

  (define-syntax define-syntax/type
    (lambda (stx)
      (syntax-case stx ()
        [(_ id type-id transformer-expr)
         (and (identifier? #'id) (identifier? #'type-id))
         #'(begin
             (define-syntax id transformer-expr)
             (define-property id syntax-type #'type-id))])))

  (define make-transformer/type
    (lambda (transformer)
      (lambda (stx)
        (lambda (lookup)
          (define type-guard
            (lambda (type id)
              (assert (identifier? type))
              (assert (identifier? id))
              (guard (c [(syntax-violation? c) #f])
                (let ([type-id (lookup id #'syntax-type)])
                  (and (identifier? type-id)
                       (free-identifier=? type type-id))))))
          (transformer stx type-guard))))))
