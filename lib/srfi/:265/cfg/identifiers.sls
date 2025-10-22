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

(library (srfi :265 cfg identifiers)
  (export
    formals?
    formals->list
    map-formals
    make-identifier-hashtable
    bound-identifier-hash
    free-identifier-hash
    renamer)
  (import
    (rnrs))

  (define formals?
    ;; XXX: Check whether the vars are distinct?
    (lambda (stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 #t]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 #t])))

  (define formals->list
    (lambda (stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 #'(var ...)]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 #'(var1 ... var2)]
	[_ (assert #f)])))

  (define map-formals
    (lambda (proc stx)
      (syntax-case stx ()
	[(var ... )
	 (for-all identifier? #'(var ...))
	 (map proc #'(var ...))]
	[(var1 ... . var2)
	 (for-all identifier? #'(var1 ... var2))
	 (append (map proc #'(var1 ...)) (proc #'var2))]
	[_ (assert #f)])))

  (define bound-identifier-hash
    (lambda (id)
      (assert (identifier? id))
      (symbol-hash (syntax->datum id))))

  (define free-identifier-hash
    (lambda (id)
      (assert (identifier? id))
      0))

  (define make-identifier-hashtable
    (lambda ()
      (make-hashtable bound-identifier-hash bound-identifier=?)))

  (define renamer
    (lambda ()
      (let ([table (make-identifier-hashtable)])
        (lambda (id)
          (assert (identifier? id))
          (or (hashtable-ref table id #f)
              (with-syntax ([(tmp) (generate-temporaries (list id))])
                (hashtable-set! table id #'tmp)
                #'tmp)))))))
