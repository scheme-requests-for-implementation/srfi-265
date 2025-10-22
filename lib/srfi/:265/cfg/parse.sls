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

(library (srfi :265 cfg parse)
  (export
    parse)
  (import
    (rnrs)
    (srfi :265 cfg ast)
    (srfi :265 cfg expand)
    (srfi :265 cfg identifiers)
    (srfi :265 cfg helpers))

  ;; Environments

  (define empty-environment
    (lambda ()
      '()))

  (define add-frame
    (lambda (env lbl-expr* lbl*)
      (let ([frame (make-hashtable label-expression-hash label-expression=?)])
        (for-each
          (lambda (lbl-expr lbl)
	    (when (hashtable-ref frame lbl-expr #f)
	      (syntax-violation #f "multiple label" lbl-expr))
	    (hashtable-set! frame lbl-expr lbl))
          lbl-expr* lbl*)
        (cons frame env))))

  (define environment-lookup
    (lambda (env id)
      (let f ([env env])
        (unless (pair? env)
          (syntax-violation #f "undefined label" id))
        (or (hashtable-ref (car env) id #f)
            (f (cdr env))))))

  ;; Parser

  (define parse
    (lambda (cfg-term)
      (let ([ast (do-parse cfg-term)])
        (propagate-permute/tail-body?! ast)
        (check-permute/tail-bodies ast)
        ast)))

  (define make-constraint
    (lambda (ast tail)
      (assert (ast? ast))
      (assert (ast? tail))
      (lambda ()
        (and (not (ast-permute/tail-body? tail))
             (ast-permute/tail-body? ast)
             (begin
               (ast-permute/tail-body?-set! ast #f)
               #t)))))

  (define constraint?
    (lambda (obj)
      (procedure? obj)))

  (define apply-constraint!
    (lambda (constraint)
      (assert (constraint? constraint))
      (constraint)))

  (define apply-constraints!
    (lambda (constraints)
      (fold-left
        (lambda (changed? constraint)
          (or (apply-constraint! constraint) changed?))
        #f constraints)))

  (define gather-constraints
    (lambda (ast)
      (let f ([ast ast] [constraints '()])
        (cond
          [(do-ast? ast)
           (fold-left
             (lambda (constraints edge)
               (f (exit-edge-next edge) constraints))
             constraints (do-ast-exit-edges ast))]
          [(let*-ast? ast)
           (let ([body (let*-ast-body ast)])
             (let* ([constraints (cons (make-constraint ast body) constraints)]
                    [constraints (f body constraints)])
               (f (binding-init (let*-ast-binding ast)) constraints)))]
          [(letrec-ast? ast)
           (let ([body (letrec-ast-body ast)])
             (let* ([constraints (cons (make-constraint ast body) constraints)]
                    [constraints (f body constraints)])
               (fold-left
                 (lambda (constraints bdg)
                   (f (binding-init bdg) constraints))
                 constraints (letrec-ast-bindings ast))))]
          [(finally-ast? ast)
           (f (finally-ast-body ast) constraints)]
          [(permute-ast? ast)
           (f (permute-ast-body ast) constraints)]
          [(permute/tail-ast? ast)
           (let ([constraints (f (binding-init (permute/tail-ast-binding ast)) constraints)])
             (f (permute/tail-ast-pending ast) constraints))]
          [(go-ast? ast)
           (cons (make-constraint ast (label-target (go-ast-target ast))) constraints)]
          [else constraints]))))

  (define propagate-permute/tail-body?!
    (lambda (ast)
      (let ([constraints (gather-constraints ast)])
        (let loop! ()
          (when (apply-constraints! constraints)
            (loop!))))))

  (define check-permute/tail-bodies
    (lambda (ast)
      (cond
        [(do-ast? ast)
         (for-each
           (lambda (edge)
             (check-permute/tail-bodies (exit-edge-next edge)))
           (do-ast-exit-edges ast))]
        [(let*-ast? ast)
         (check-permute/tail-bodies (binding-init (let*-ast-binding ast)))
         (check-permute/tail-bodies (let*-ast-body ast))]
        [(letrec-ast? ast)
         (for-each
           (lambda (bdg)
             (check-permute/tail-bodies (binding-init bdg)))
           (letrec-ast-bindings ast))
         (check-permute/tail-bodies (letrec-ast-body ast))]
        [(finally-ast? ast)
         (check-permute/tail-bodies (finally-ast-body ast))]
        [(permute-ast? ast)
         (check-permute/tail-bodies (permute-ast-body ast))]
        [(permute/tail-ast? ast)
         (let ([body (binding-init (permute/tail-ast-binding ast))])
           (unless (ast-permute/tail-body? body)
             (syntax-violation #f "invalid permute/tail body" (permute/tail-ast-body-syntax ast)))
           (check-permute/tail-bodies body)
           (check-permute/tail-bodies (permute/tail-ast-pending ast)))])))

  (define do-parse
    (lambda (cfg-term)
      (let f ([cfg-frag cfg-term] [env (empty-environment)])
        (define g (lambda (cfg-frag) (f cfg-frag env)))
        (syntax-case cfg-frag (go do finally halt let* letrec permute permute/tail)
          [(go tgt)
           (label-expression? #'tgt)
           (make-go-ast (environment-lookup env #'tgt))]
          [(do proc-expr [formals next-cfg-frag] ...)
           (for-all formals? #'(formals ...))
           (make-do-ast #'proc-expr #'(formals ...) (map g #'(next-cfg-frag ...)))]
	  [(finally formals expr body-cfg-frag)
	   (formals? #'formals)
	   (make-finally-ast #'formals #'expr (g #'body-cfg-frag))]
	  [(halt)
	   (make-halt-ast)]
	  [(let* [(lbl-expr init-cfg-frag)] body-cfg-frag)
	   (label-expression? #'lbl-expr)
	   (let ([lbl (make-label (car (generate-temporaries #'(lbl-expr))))])
	     (let ([extended-env (add-frame env (list #'lbl-expr) (list lbl))])
               (let ([init (f #'init-cfg-frag env)])
                 (label-target-set! lbl init)
	         (make-let*-ast lbl init
                   (f #'body-cfg-frag extended-env)))))]
	  [(letrec [(lbl-expr init-cfg-frag) ...] body-cfg-frag)
	   (for-all label-expression? #'(lbl-expr ...))
	   (let ([lbl* (map make-label (generate-temporaries #'(lbl-expr ...)))])
	     (let ([extended-env (add-frame env #'(lbl-expr ...) lbl*)])
               (let ([init* (map (lambda (cfg-frag)
			           (f cfg-frag extended-env))
		                 #'(init-cfg-frag ...))])
                 (for-each label-target-set! lbl* init*)
	         (make-letrec-ast lbl* init* (f #'body-cfg-frag extended-env)))))]
	  [(permute () cfg)
	   (make-permute-ast (f #'cfg env))]
	  [(permute/tail stx ([lbl-expr cfg1]) cfg2)
	   (label-expression? #'lbl-expr)
           (let ([lbl (make-label (car (generate-temporaries #'(lbl-expr))))])
	     (let ([extended-env (add-frame env (list #'lbl-expr) (list lbl))])
               (let ([body (f #'cfg2 env)])
                 (label-target-set! lbl body)
	         (make-permute/tail-ast #'stx lbl (f #'cfg1 extended-env) body))))]
          [_ (assert #f)])))))
