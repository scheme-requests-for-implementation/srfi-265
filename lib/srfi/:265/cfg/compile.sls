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

(library (srfi :265 cfg compile)
  (export compile!
	  $go
	  $do
	  $finally
	  $halt
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

  (define-syntax $do
    (syntax-rules ()
      [($do (([var tmp] ...)
		  proc-expr)
	 [formals next-expr] ...)
       ((let ([var tmp] ...) proc-expr)
	(lambda formals next-expr) ...)]))

  (define-syntax $finally
    (syntax-rules ()
      [($finally ([(intmp ...) body]
		  [(var tmp) ...]
		  [(invar orig)  ...]
		  [formals expr])
	 outvar ...)
       (let-values ([(intmp ...) body]
                    [(var) tmp] ...)
         (let ([invar orig] ...)
           (let-values ([formals expr])
             (values outvar ...))))]))

  (define-syntax $let*
    (syntax-rules ()
      [($let* ([id (var ...) init-expr]) body-expr)
       (let ([id (lambda (var ...)
                   init-expr)])
         body-expr)]))

  (define-syntax $letrec
    (syntax-rules ()
      [($letrec ([id (var ...) init-expr] ...) body-expr)
       (letrec ([id (lambda (var ...)
                      init-expr)] ...)
         body-expr)]))

  (define-syntax $halt
    (syntax-rules ()
      [($halt)
       (values)]))

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
      (define result-vars (infer-types! ast))
      (define loop-expr
        (let f ([ast ast])
          (cond
            [(go-ast? ast)
             (let ([tgt (go-ast-target-id ast)])
               (with-syntax ([id (label->identifier tgt)]
                             [(arg ...) (label-arguments tgt)])
                 #'($go id arg ...)))]
            [(do-ast? ast)
             (let ([sigma-set (unbox (do-ast-sigma ast))])
               (with-syntax
                   ([(var ...) sigma-set]
                    [(tmp ...) (map variable->identifier sigma-set)]
                    [proc-expr (do-ast-proc-expr ast)]
                    [((formals next-expr) ...)
                     (map
                       (lambda (edge)
                         (with-syntax ([formals
                                         (map-formals variable->identifier (exit-edge-formals edge))]
                                       [next-expr (f (exit-edge-next edge))])
                           #'(formals next-expr)))
                       (do-ast-exit-edges ast))])
		 #'($do ([(var tmp) ...] proc-expr)
		     [formals next-expr] ...)))]
            [(finally-ast? ast)
             (let ([sigma (unbox (finally-ast-sigma ast))])
               (define input-psi (unbox (finally-ast-psi-input ast)))
	       (define input-epsilon (unbox (finally-ast-epsilon-input ast)))
               (with-syntax
                   ([(var ...) sigma]
                    [(tmp ...) (map variable->identifier sigma)]
                    [body (f (finally-ast-body ast))]
                    [(invar ...) input-psi]
                    [(intmp ...) (map variable->identifier input-epsilon)]
		    [(orig ...) (map variable->identifier input-psi)]
                    [(outvar ...) (map variable->identifier (unbox (finally-ast-psi-output ast)))]
                    [formals (map-formals variable->identifier (finally-ast-formals ast))]
                    [expr (finally-ast-expr ast)])
		 #'($finally ([(intmp ...) body]
			      [(var tmp) ...]
			      [(invar orig) ...]
			      [formals expr])
		     outvar ...)))]
            [(halt-ast? ast)
             #'($halt)]
	    [(let*-ast? ast)
	     (let ([bdg (let*-ast-binding ast)])
	       (define lbl (binding-label-id bdg))
	       (hashtable-set! label-arguments-table lbl
		 (map variable->identifier (unbox (binding-delta bdg))))
	       (with-syntax ([id (label->identifier lbl)]
                             [(var ...) (label-arguments lbl)]
                             [init-expr (f (binding-init bdg))]
                             [body-expr (f (let*-ast-body ast))])
		 #'($let* ([id (var ...) init-expr])
		     body-expr)))]
            [(letrec-ast? ast)
             (let ([bdg* (letrec-ast-bindings ast)])
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
                         (f (binding-init bdg)))
                       bdg*)]
                    [body-expr (f (letrec-ast-body ast))])
		 #'($letrec ([id (var ...) init-expr] ...)
		     body-expr)))]
	    [(permute-ast? ast)
             (f (permute-ast-body ast))]
	    [(permute/tail-ast? ast)
	     (let ([bdg (permute/tail-ast-binding ast)])
	       (define lbl (binding-label-id bdg))
	       (hashtable-set! label-arguments-table lbl
		 (map variable->identifier (unbox (binding-delta bdg))))
	       (with-syntax
                   ([id (label->identifier lbl)]
		    [(var ...) (label-arguments lbl)]
		    [head (f (permute/tail-ast-pending ast))]
		    [tail (f (binding-init bdg))])
		 #'($permute/tail (id head)
		     (var ...) tail)))]
            [else (assert #f)])))
      (with-syntax
          ([result result-expr]
           [loop loop-expr]
           [(var ...) result-vars])
        #'(let-values ([(var ...) loop])
            result))))

  )
