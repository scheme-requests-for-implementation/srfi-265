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

(library (srfi :265 cfg ast)
  (export
    ast?
    ast-permute/tail-body?
    ast-permute/tail-body?-set!
    make-label
    label?
    label-id
    label-target
    label-target-set!
    binding?
    binding-label
    binding-label-id
    binding-init
    binding-delta
    binding-rho
    binding-sigma
    binding-psi
    binding-phi
    binding-epsilon
    exit-edge?
    exit-edge-formals
    exit-edge-next
    make-let*-ast
    let*-ast?
    let*-ast-binding
    let*-ast-body
    make-letrec-ast
    letrec-ast?
    letrec-ast-bindings
    letrec-ast-body
    make-do-ast
    do-ast?
    do-ast-sigma
    do-ast-proc-expr
    do-ast-exit-edges
    make-finally-ast
    finally-ast?
    finally-ast-formals
    finally-ast-expr
    finally-ast-body
    finally-ast-sigma
    finally-ast-psi-input
    finally-ast-psi-output
    finally-ast-epsilon-input
    make-go-ast
    go-ast?
    go-ast-target
    go-ast-target-id
    make-halt-ast
    halt-ast?
    make-permute-ast
    permute-ast?
    permute-ast-body
    make-permute/tail-ast
    permute/tail-ast?
    permute/tail-ast-pending
    permute/tail-ast-binding
    permute/tail-ast-body-syntax)
  (import
    (rnrs)
    (srfi :265 cfg box)
    (srfi :265 cfg identifiers))

  (define label-list?
    (lambda (obj)
      (and (list? obj) (for-all label? obj))))

  (define ast-list?
    (lambda (obj)
      (and (list? obj) (for-all ast? obj))))

  (define formals-list?
    (lambda (obj)
      (and (list? obj) (for-all formals? obj))))

  (define-record-type label
    (nongenerative label-4193dc41-8dbe-468b-8b63-d1edf5afd5db)
    (fields id (mutable target))
    (sealed #t)
    (protocol
      (lambda (p)
        (lambda (id)
          (assert (identifier? id))
          (p id #f)))))

  (define-record-type binding
    (nongenerative binding-66ff974d-298a-4680-a3e3-bb00e451f05d)
    (sealed #t)
    (fields label delta rho sigma psi phi epsilon init)
    (protocol
      (lambda (p)
        (lambda (lbl init)
          (assert (label? lbl))
          (assert (ast? init))
          (p lbl (box 0) (box -1) (box -1) (box -1) (box -1) (box -1) init)))))

  (define binding-label-id
    (lambda (bdg)
      (assert (binding? bdg))
      (label-id (binding-label bdg))))

  (define-record-type ast
    (nongenerative ast-4c1a1489-35f8-4467-a0b0-4960dbf8bfd7)
    (fields (mutable permute/tail-body?))
    (protocol
      (lambda (p)
        (lambda (permute/tail-body?)
          (p permute/tail-body?)))))

  (define-record-type letrec-ast
    (nongenerative letrec-ast-c5fb5fc1-d6dc-4270-b3e8-3ee6c040bd05)
    (sealed #t)
    (parent ast)
    (fields bindings body)
    (protocol
      (lambda (n)
        (lambda (lbl* init* body)
          (assert (label-list? lbl*))
          (assert (ast-list? init*))
          (assert (ast? body))
          ((n (ast-permute/tail-body? body)) (map make-binding lbl* init*) body)))))

  (define-record-type exit-edge
    (nongenerative exit-edge-f76f1d32-ce2e-4c41-9306-d8159dd9a3ad)
    (sealed #t)
    (fields formals next)
    (protocol
      (lambda (p)
        (lambda (formals next)
          (assert (formals? formals))
          (assert (ast? next))
          (p formals next)))))

  (define-record-type do-ast
    (nongenerative do-ast-cc8be75e-a436-4018-a7e7-07b21f41d891)
    (sealed #t)
    (parent ast)
    (fields sigma proc-expr exit-edges)
    (protocol
      (lambda (n)
        (lambda (proc-expr formals* next*)
          (assert (formals-list? formals*))
          (assert (ast-list? next*))
          ((n #f) (box -1) proc-expr (map make-exit-edge formals* next*))))))

  (define-record-type finally-ast
    (nongenerative finally-ast-9117b61f-5e3e-4d99-a5fd-6f38d88b4b18)
    (sealed #t)
    (parent ast)
    (fields sigma psi-input psi-output epsilon-input formals expr body)
    (protocol
      (lambda (n)
        (lambda (formals expr body)
          (assert (formals? formals))
          (assert (ast? body))
          ((n #f) (box -1) (box -1) (box -1) (box -1) formals expr body)))))

  (define-record-type go-ast
    (nongenerative go-ast-3000f675-33f2-44ec-bd18-a95bba38de17)
    (sealed #t)
    (parent ast)
    (fields target)
    (protocol
      (lambda (n)
        (lambda (tgt)
          (assert (label? tgt))
          ((n #t) tgt)))))

  (define go-ast-target-id
    (lambda (ast)
      (assert (go-ast? ast))
      (label-id (go-ast-target ast))))

  (define-record-type halt-ast
    (nongenerative halt-ast-fd4029ec-8abd-4289-afde-33ea2fb57ccb)
    (sealed #t)
    (parent ast)
    (protocol
      (lambda (n)
        (lambda ()
          ((n #f))))))

  (define-record-type permute-ast
    (nongenerative permute-ast-0ad23656-fda6-4907-9a80-49738ff80017)
    (sealed #t)
    (parent ast)
    (fields body)
    (protocol
      (lambda (n)
        (lambda (body)
	  (assert (ast? body))
	  ((n #t) body)))))

  (define-record-type permute/tail-ast
    (nongenerative permute/tail-06effd79-3106-4523-bca4-b41bfb2826ae)
    (sealed #t)
    (parent ast)
    (fields body-syntax pending binding)
    (protocol
      (lambda (n)
        (lambda (stx label pending tail)
	  (assert (label? label))
	  (assert (ast? pending))
	  (assert (ast? tail))
	  ((n #t) stx pending (make-binding label tail))))))

  (define-record-type let*-ast
    (nongenerative let*-ast-7d6f871f-a103-4fdf-aa30-45bf9fd64c7e)
    (sealed #t)
    (parent ast)
    (fields binding body)
    (protocol
      (lambda (n)
        (lambda (lbl init body)
	  (assert (label? lbl))
	  (assert (ast? init))
	  (assert (ast? body))
	  ((n (ast-permute/tail-body? body)) (make-binding lbl init) body))))))
