#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg helpers)
  (export
    label-expression?
    label-expression=?
    label-expression-hash)
  (import
    (rnrs)
    (srfi :265 cfg identifiers))

  (define label-expression?
    (lambda (stx)
      (syntax-case stx ()
        [id (identifier? #'id) #t]
        [(id) (identifier? #'id) #t]
        [_ #f])))

  (define label-expression=?
    (lambda (e1 e2)
      (syntax-case (list e1 e2) ()
        [(id1 id2)
         (and (identifier? #'id1) (identifier? #'id2))
         (bound-identifier=? #'id1 #'id2)]
        [((id1) (id2))
         (and (identifier? #'id1) (identifier? #'id2))
         (free-identifier=? #'id1 #'id2)]
        [_ #f])))

  (define label-expression-hash
    (lambda (e)
      (syntax-case e ()
        [id (identifier? #'id) (bound-identifier-hash #'id)]
        [(id) (identifier? #'id) (free-identifier-hash #'id)]
        [_ (assert #f)]))))
