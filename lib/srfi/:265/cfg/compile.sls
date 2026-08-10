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

(library (srfi :265 cfg compile)
  (export compile!
	  $go
	  $branch
	  $return-values
	  $defer
	  $let*
	  $letrec
          $permute/tail)
  (import (rnrs)
          (srfi :265 cfg box)
	  (srfi :265 cfg ast)
          (srfi :265 cfg identifiers)
          (srfi :265 cfg infer-types))

  (define-syntax $go
    (syntax-rules ()
      [($go id arg ...)
       (id arg ...)]))

  (define-syntax $branch
    (syntax-rules ()
      [($branch (([var tmp] ...) expr)
	 [(target formals) next-expr] ...)
       (let ([target (lambda formals next-expr)] ...)
         (let ([var tmp] ...)
           expr))]))

  (define-syntax $let*
    (syntax-rules ()
      [($let* ([id (var ...) init-expr]) body-expr)
       (let ([id (lambda (var ...)
                   init-expr)])
         body-expr)]))

  (define-syntax $letrec
    (lambda (stx)
      (syntax-case stx ($go)
        ;; Chez recognizes a recursive procedure returned by `letrec' as
        ;; a loop and propagates its result information through the call.
        ;; Keep the procedure directly in operator position when a
        ;; single-label body immediately enters that label.
        [($letrec ([id (var ...) init-expr])
           ($go body-id arg ...))
         (free-identifier=? #'id #'body-id)
         #'((letrec ([id (lambda (var ...)
                           init-expr)])
              id)
            arg ...)]
        [($letrec ([id (var ...) init-expr] ...) body-expr)
         #'(letrec ([id (lambda (var ...)
                          init-expr)] ...)
             body-expr)])))

  ;; A one-variable `let-values' is an ordinary single-value context.
  ;; Saying so with `let' lets Chez propagate single-valuedness without
  ;; changing the zero- or multiple-value cases.  The first rule also
  ;; handles several independent one-variable bindings while retaining
  ;; their unspecified evaluation order.
  (define-syntax $let-values
    (syntax-rules ()
      [($let-values ([(var) expr] ...) body-expr)
       (let ([var expr] ...)
         body-expr)]
      [($let-values ([formals expr] ...) body-expr)
       (let-values ([formals expr] ...)
         body-expr)]))

  (define-syntax $return-values
    (syntax-rules ()
      [($return-values ([(var tmp) ...]
                        [[formals expr] ...])
         outvar ...)
       (let ([var tmp] ...)
         ($let-values ([formals expr] ...)
           (values outvar ...)))]))

  (define-syntax $defer
    (syntax-rules ()
      [($defer ([(intmp ...) successor]
                [(var tmp) ...]
                [(invar orig) ...]
                [(target formals
                   (outtmp ...) (outvar ...) branch) ...]
                deferred-expr))
       ($let-values ([(intmp ...) successor]
                     [(var) tmp] ...)
         ;; Target formals are ordinary Scheme variables.  The
         ;; corresponding CFG term occurs directly in their lexical
         ;; scope and also captures the CFG-variable bindings returned
         ;; by the last CFG term.
         (let ([target
                (lambda formals
                  ($let-values ([(outtmp ...) branch])
                    (values outvar ...)))] ...)
           (let ([invar orig] ...)
             deferred-expr)))]))

  (define-syntax $permute/tail
    (syntax-rules ()
      [($permute/tail (id head) (var ...) tail)
       (let ([id (lambda (var ...) tail)])
	 head)]))

  (define compile!
    (lambda (result-expr ast)
      (define label->identifier (renamer))
      (define variable->identifier (renamer))
      (define label-arguments-table (make-identifier-hashtable))
      (define label-arguments
        (lambda (lbl)
          (assert (identifier? lbl))
          (assert (hashtable-ref label-arguments-table lbl #f))))
      (define binding-referenced?
        (lambda (bdg)
          (assert (binding? bdg))
          (unbox (binding-rho bdg))))
      (define identifier-member?
        (lambda (var var*)
          (exists
            (lambda (other)
              (bound-identifier=? var other))
            var*)))
      (define visible-cfg-variables
        (lambda (var* scheme-bound)
          (filter
            (lambda (var)
              (not (identifier-member? var scheme-bound)))
            var*)))
      (define result-vars (infer-types! ast))
      (define loop-expr
        (let f ([ast ast] [scheme-bound '()])
          (cond
            [(go-ast? ast)
             (let ([tgt (go-ast-target-id ast)])
               (with-syntax ([id (label->identifier tgt)]
                             [(arg ...) (label-arguments tgt)])
                 #'($go id arg ...)))]
            [(branch-ast? ast)
             (let ([sigma-set
                     (visible-cfg-variables
                       (unbox (branch-ast-sigma ast)) scheme-bound)])
               (with-syntax
                   ([(var ...) sigma-set]
                    [(tmp ...) (map variable->identifier sigma-set)]
                    [expr (branch-ast-expr ast)]
                    [(((target formals) next-expr) ...)
                     (map
                       (lambda (target edge)
                         (with-syntax ([target target]
                                       [formals
                                         (map-formals variable->identifier (exit-edge-formals edge))]
                                       [next-expr
                                        (f (exit-edge-next edge)
                                          scheme-bound)])
                           #'((target formals) next-expr)))
                       (branch-ast-targets ast)
                       (branch-ast-exit-edges ast))])
		 #'($branch ([(var tmp) ...] expr)
		     [(target formals) next-expr] ...)))]
            [(return-values-ast? ast)
             (let ([sigma
                     (visible-cfg-variables
                       (unbox (return-values-ast-sigma ast)) scheme-bound)])
               (with-syntax
                   ([(var ...) sigma]
                    [(tmp ...) (map variable->identifier sigma)]
                    [(outvar ...) (map variable->identifier (unbox (return-values-ast-psi-output ast)))]
                    [([formals expr] ...)
                     (map
                       (lambda (formals expr)
                         (with-syntax ([formals (map-formals variable->identifier formals)]
                                       [expr expr])
                           #'[formals expr]))
                       (return-values-ast-formals* ast)
                       (return-values-ast-expr* ast))])
                 #'($return-values ([(var tmp) ...]
                                    [[formals expr] ...])
                     outvar ...)))]
            [(defer-ast? ast)
             (let ([sigma
                    (visible-cfg-variables
                      (unbox (defer-ast-sigma ast)) scheme-bound)]
                   [input-psi (unbox (defer-ast-psi-input ast))]
                   [input-epsilon
                    (unbox (defer-ast-epsilon-input ast))]
                   [output-psi (unbox (defer-ast-psi-output ast))])
               (let ([visible-input-psi (visible-cfg-variables input-psi scheme-bound)])
                 (with-syntax
                     ([(var ...) sigma]
                      [(tmp ...) (map variable->identifier sigma)]
                      [(intmp ...)
                       (map variable->identifier input-epsilon)]
                      [(invar ...) visible-input-psi]
                      [(orig ...)
                       (map variable->identifier visible-input-psi)]
                      [((target formals
                          (outtmp ...) (outvar ...) branch) ...)
                       (map
                         (lambda (target edge epsilon-branch)
                           (define target-formals
                             (exit-edge-formals edge))
                           (with-syntax
                               ([target target]
                                [formals target-formals]
                                [(outtmp ...)
                                 (map variable->identifier
                                   (unbox epsilon-branch))]
                                [(outvar ...)
                                 (map variable->identifier output-psi)]
                                [branch
                                  (f (exit-edge-next edge)
                                    (append
                                      (formals->list target-formals)
                                      scheme-bound))])
                             #'(target formals
                                 (outtmp ...) (outvar ...) branch)))
                         (defer-ast-targets ast)
                         (defer-ast-exit-edges ast)
                         (defer-ast-epsilon-branches ast))]
                      [successor
                        (f (defer-ast-successor ast) scheme-bound)]
                      [deferred-expr (defer-ast-expr ast)])
                   #'($defer ([(intmp ...) successor]
                              [(var tmp) ...]
                              [(invar orig) ...]
                              [(target formals
                                 (outtmp ...) (outvar ...) branch) ...]
                              deferred-expr)))))]
            [(and (let*-ast? ast)
                  (not (binding-referenced? (let*-ast-binding ast))))
             (f (let*-ast-body ast) scheme-bound)]
	    [(let*-ast? ast)
	     (let ([bdg (let*-ast-binding ast)])
	       (define lbl (binding-label-id bdg))
	       (hashtable-set! label-arguments-table lbl
		 (map variable->identifier (unbox (binding-delta bdg))))
	       (with-syntax ([id (label->identifier lbl)]
                             [(var ...) (label-arguments lbl)]
                             [init-expr
                              (f (binding-init bdg) scheme-bound)]
                             [body-expr
                              (f (let*-ast-body ast) scheme-bound)])
		 #'($let* ([id (var ...) init-expr])
		     body-expr)))]
            [(letrec-ast? ast)
             (let ([bdg* (filter binding-referenced?
                           (letrec-ast-bindings ast))])
               (define lbl* (map binding-label-id bdg*))
               (for-each
                 (lambda (bdg lbl)
                   (hashtable-set! label-arguments-table lbl
                     (map variable->identifier (unbox (binding-delta bdg)))))
                 bdg* lbl*)
               (with-syntax
                   ([(id ...) (map label->identifier lbl*)]
                    [((var ...) ...) (map label-arguments lbl*)]
                    [(init-expr ...)
                     (map
                       (lambda (bdg)
                         (f (binding-init bdg) scheme-bound))
                       bdg*)]
                    [body-expr
                     (f (letrec-ast-body ast) scheme-bound)])
		 #'($letrec ([id (var ...) init-expr] ...)
		     body-expr)))]
	    [(permute-ast? ast)
             (f (permute-ast-body ast) scheme-bound)]
            [(and (permute/tail-ast? ast)
                  (not (binding-referenced?
                         (permute/tail-ast-binding ast))))
             (f (permute/tail-ast-pending ast) scheme-bound)]
	    [(permute/tail-ast? ast)
	     (let ([bdg (permute/tail-ast-binding ast)])
	       (define lbl (binding-label-id bdg))
	       (hashtable-set! label-arguments-table lbl
		 (map variable->identifier (unbox (binding-delta bdg))))
	       (with-syntax
                   ([id (label->identifier lbl)]
		    [(var ...) (label-arguments lbl)]
		    [head
                     (f (permute/tail-ast-pending ast) scheme-bound)]
		    [tail (f (binding-init bdg) scheme-bound)])
		 #'($permute/tail (id head)
		     (var ...) tail)))]
            [else (assert #f)])))
      (with-syntax
          ([result result-expr]
           [loop loop-expr]
           [(var ...) result-vars])
        #'($let-values ([(var ...) loop])
            result))))

  )
