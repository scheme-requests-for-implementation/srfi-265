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

(library (srfi :265 cfg infer-types)
  (export
    infer-types!)
  (import
    (rnrs)
    (srfi :265 cfg box)
    (srfi :265 cfg ast)
    (srfi :265 cfg numbers)
    (srfi :265 cfg list-case)
    (srfi :265 cfg identifiers))

  (define make-constraint
    (lambda (box updater)
      (assert (box? box))
      (assert (procedure? updater))
      (lambda ()
	(define old (unbox box))
	(define new (updater old))
	(cond
	  [(eqv? old new) #f]
	  [else
	    (set-box! box new)
	    #t]))))

  (define apply-constraints!
    (lambda (constraints)
      (let f! ()
	(define dirty?
	  (fold-left
	   (lambda (dirty? constraint!)
             (or (constraint!) dirty?))
	   #f constraints))
	(when dirty? (f!)))))

  (define infer-types!
    (lambda (ast)
      (define variable-table (make-identifier-hashtable))
      (define variable->index
	(lambda (var)
	  (assert (identifier? var))
	  (or (hashtable-ref variable-table var #f)
	      (let ([idx (hashtable-size variable-table)])
		(hashtable-set! variable-table var idx)
		idx))))
      (define variable->mask
	(lambda (var)
	  (assert (identifier? var))
	  (bitwise-arithmetic-shift 1 (variable->index var))))
      (define variable-list->bitwise
	(lambda (var*)
	  (assert (list? var*))
	  (fold-left
	   (lambda (set var)
	     (assert (identifier? var))
	     (bitwise-ior set (variable->mask var)))
	   0 var*)))
      ;; The binding environment plays the role of Sigma in Olin
      ;; Shiver's paper.
      (define binding-environment (make-identifier-hashtable))
      (define add-binding!
	(lambda (bdg)
	  (assert (binding? bdg))
	  (hashtable-set! binding-environment (binding-label-id bdg) bdg)))
      (define lookup-binding
	(lambda (lbl)
	  (assert (identifier? lbl))
	  (assert (hashtable-ref binding-environment lbl #f))))
      (define constraints '())
      (define add-constraint!
	(lambda (box updater)
	  (assert (box? box))
	  (assert (procedure? updater))
	  (set! constraints
		(cons (make-constraint box updater) constraints))))
      (define add-constraint<=!
	(lambda (box1 box2)
	  (assert (box? box1))
	  (assert (box? box2))
	  (add-constraint! box1 (lambda (x) (bitwise-and x (unbox box2))))))
      (define add-constraint=!
	(lambda (box1 box2)
	  (assert (box? box1))
	  (assert (box? box2))
	  (add-constraint<=! box1 box2)
	  (add-constraint<=! box2 box1)))
      (define add-constraint-union!
	(lambda (box1 box2 box3)
	  (assert (box? box1))
	  (assert (box? box2))
	  (assert (box? box3))
	  (add-constraint! box1 (lambda (x) (bitwise-and x (bitwise-ior (unbox box2) (unbox box3)))))))
      (define psi-output (box -1))
      (let f! ([ast ast]
               ;; ENV plays the role of Delta in Olin Shiver's paper.
	       [env '()]
	       [rho-type (box 0)]
	       [sigma-type (box 0)]
	       [psi psi-output]
	       [phi (box -1)]
	       [epsilon (box -1)])
	(define label-local-delta
	  (lambda (lbl)
	    (assert (identifier? lbl))
	    (let f ([env env])
	      (list-case env
		[(bdg . env)
		 (cond
		   [(identifier? bdg) (if (bound-identifier=? bdg lbl) '() (f env))]
		   [(or (box? bdg) (exact-integer? bdg)) (cons bdg (f env))]
		   [else (assert #f)])]
		[() '()]))))
	(define cover
	  (lambda (covering)
	    (fold-left
	     (lambda (set part)
	       (cond
		[(box? part)
		 (bitwise-ior set (unbox part))]
		[(exact-integer? part)
		 (bitwise-ior set part)]
		[else
                  (assert #f)]))
	     0 covering)))
	(define add-constraint>=!
	  (lambda (box covering)
	    (add-constraint! box (lambda (x) (bitwise-ior x (cover covering))))))
	(cond
	 [(do-ast? ast)
	  (add-constraint<=! (do-ast-sigma ast) rho-type)
	  (for-each
	   (lambda (exit)
	     (define var* (formals->list (exit-edge-formals exit)))
	     (define ast (exit-edge-next exit))
	     (define set (variable-list->bitwise var*))
	     (define inner-rho (box -1))
	     (add-constraint-union! inner-rho (box set) rho-type)
	     (f! ast
		 (cons set env)
		 inner-rho
		 inner-rho
		 psi
		 psi
		 epsilon)
	     (add-constraint=! phi psi))
	   (do-ast-exit-edges ast))]
	 [(go-ast? ast)
	  (let ([lbl (go-ast-target-id ast)])
	    (define bdg (lookup-binding lbl))
	    (add-constraint<=! (binding-rho bdg) rho-type)
	    (add-constraint<=! (binding-sigma bdg) sigma-type)
	    (add-constraint>=! (binding-delta bdg) (label-local-delta lbl))
            (add-constraint=! psi (binding-psi bdg))
	    (add-constraint=! phi (binding-phi bdg))
	    (add-constraint=! epsilon (binding-epsilon bdg)))]
	 [(finally-ast? ast)
          (let ([set (variable-list->bitwise (formals->list (finally-ast-formals ast)))])
            (define sigma (finally-ast-sigma ast))
            (define psi-input (finally-ast-psi-input ast))
	    (define epsilon-input (finally-ast-epsilon-input ast))
	    (f! (finally-ast-body ast)
	        env
	        rho-type
	        rho-type
                psi-input
		;; PHI is ignored as `finally' realizes a
		;; pending sequence.
		(box -1)
		epsilon-input)
            (add-constraint<=! sigma rho-type)
	    (add-constraint-union! psi (box set) psi-input)
	    (add-constraint-union! epsilon (box set) epsilon-input)
	    (add-constraint=! phi psi)
	    (add-constraint<=! (finally-ast-psi-output ast) epsilon))]
	 [(halt-ast? ast)
          (set-box! psi 0)
	  (set-box! phi 0)
	  (set-box! epsilon 0)]
	 [(let*-ast? ast)
	  (let ([bdg (let*-ast-binding ast)])
	    (define delta (binding-delta bdg))
	    (add-binding! bdg)
	    (f! (binding-init bdg)
		(cons delta env)
		(binding-rho bdg)
		(binding-sigma bdg)
		(binding-psi bdg)
		(binding-phi bdg)
		(binding-epsilon bdg))
	    (f! (let*-ast-body ast)
		(cons (binding-label-id bdg) env)
		rho-type
		sigma-type
		psi
		phi
		epsilon))]
	 [(letrec-ast? ast)
	  (let ([bdg* (letrec-ast-bindings ast)])
	    (for-each add-binding! bdg*)
	    (for-each
	     (lambda (bdg)
	       (define delta (binding-delta bdg))
	       (f! (binding-init bdg)
		   (cons delta env)
		   (binding-rho bdg)
		   (binding-sigma bdg)
                   (binding-psi bdg)
		   (binding-phi bdg)
		   (binding-epsilon bdg)))
	     bdg*)
	    (f! (letrec-ast-body ast)
		(append (map binding-label-id bdg*) env)
		rho-type
		sigma-type
                psi
		phi
		epsilon))]
	 [(permute-ast? ast)
          (f! (permute-ast-body ast)
            env
            rho-type
            ;; As a permute realizes a pending permutation sequence,
            ;; we ignore SIGMA here.
            rho-type
            psi
            psi
            epsilon)
          (add-constraint=! phi psi)]
	 [(permute/tail-ast? ast)
	  (let ([bdg (permute/tail-ast-binding ast)])
	    (define lbl (binding-label-id bdg))
	    (define psi-tail (box -1))
	    (define phi-head (box -1))
	    (define psi-head (box -1))
	    (define tail-rho (box -1))
	    (add-binding! bdg)
	    (add-constraint-union! tail-rho (binding-rho bdg) rho-type)
	    (add-constraint-union! psi psi-tail psi-head)
	    (add-constraint=! (binding-psi bdg) (binding-phi bdg))
	    (add-constraint=! (binding-phi bdg) phi)
	    ;; Tail
	    (f! (binding-init bdg)
		(cons (binding-delta bdg) env)
		tail-rho
                sigma-type
		psi-tail
		(binding-phi bdg)
		(binding-epsilon bdg))
	    ;; Head
	    (f! (permute/tail-ast-pending ast)
		(cons lbl env)
		sigma-type
		sigma-type
		psi-head
		phi-head
		epsilon))]
	 [else (assert #f)]))
      (apply-constraints! constraints)

      (let ([update
	     (let-values ([(vars idxs) (hashtable-entries variable-table)])
	       (lambda (set)
		 (let f ([i 0] [res '()])
		   (cond [(fx=? i (vector-length vars)) res]
			 [(bitwise-bit-set? set (vector-ref idxs i))
			  (f (fx+ i 1) (cons (vector-ref vars i) res))]
			 [else (f (fx+ i 1) res)]))))])
	(update-sets! ast update)
        (update (unbox psi-output)))))

   (define update-sets!
     (lambda (ast update)
       (define update-box!
	 (case-lambda
	  [(box)
	   (set-box! box (update (unbox box)))]
	  [(box limit)
	   (set-box! box (update (bitwise-and (unbox box) limit)))]))
      (let visit! ([ast ast])
	(define visit-binding!
	  (lambda (bdg)
	    (update-box! (binding-delta bdg) (unbox (binding-rho bdg)))
	    (update-box! (binding-rho bdg))
	    (update-box! (binding-sigma bdg))
            (update-box! (binding-psi bdg))
	    (visit! (binding-init bdg))))
	(cond
	 [(halt-ast? ast)
	  (values)]
	 [(go-ast? ast)
	  (values)]
	 [(do-ast? ast)
	  (update-box! (do-ast-sigma ast))
	  (for-each
	   (lambda (edge)
	     (visit! (exit-edge-next edge)))
	   (do-ast-exit-edges ast))]
	 [(let*-ast? ast)
	  (visit-binding! (let*-ast-binding ast))
	  (visit! (let*-ast-body ast))]
	 [(letrec-ast? ast)
	  (for-each visit-binding! (letrec-ast-bindings ast))
	  (visit! (letrec-ast-body ast))]
	 [(finally-ast? ast)
          (update-box! (finally-ast-sigma ast))
          (update-box! (finally-ast-psi-input ast))
          (update-box! (finally-ast-psi-output ast))
	  (update-box! (finally-ast-epsilon-input ast))
	  (visit! (finally-ast-body ast))]
         [(permute-ast? ast)
	  (visit! (permute-ast-body ast))]
	 [(permute/tail-ast? ast)
	  (visit-binding! (permute/tail-ast-binding ast))
	  (visit! (permute/tail-ast-pending ast))]
	 [else (assert #f)]))))

   )
