#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg list-case)
  (export
    list-case)
  (import
    (rnrs))

  (define-syntax list-case
    (lambda (x)
      (syntax-case x ()
        [(_ e
            [(a . b) e1 ... e2]
            [() e3 ... e4])
	 (for-all identifier? #'(a b))
         #'(let ([tmp e])
             (cond
              [(pair? tmp)
               (let ([a (car tmp)] [b (cdr tmp)])
                 e1 ... e2)]
              [(null? tmp) e3 ... e4]
              [else
               (assertion-violation 'list-case "invalid list" tmp)]))]))))
