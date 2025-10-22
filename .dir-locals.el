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

;; TODO: Update the set of keywords to the latest version.

((scheme-mode
  . ((eval
      . (progn
	  (put 'cfg 'scheme-indent-function 1)
	  (put 'execute 'scheme-indent-function 1)
	  (put 'finally 'scheme-indent-function 2)
	  (put 'indep 'scheme-indent-function 1)
	  (put 'label* 'scheme-indent-function 1)
	  (put 'labels 'scheme-indent-function 1)
	  (put 'loop 'scheme-indent-function 0)
          (put 'list-case 'scheme-indent-function 1)
          (put 'permute 'scheme-indent-function 1)
          (put 'permute/tail 'scheme-indent-function 1)
          (put 'with-implicit 'scheme-indent-function 1)
          (put 'with-syntax 'scheme-indent-function 1)
          (font-lock-add-keywords
           nil
           '(("(\\(define/who\\|define-syntax/who\\|define-cfg-syntax\\|define-cfg-syntax\\*\\|define-property\\|define-loop-syntax\\*?\\)\\>[ \t]*(*\\(\\sw+\\)?"
              (1 font-lock-keyword-face)
              (2 font-lock-function-name-face nil t))
             ("(\\(call\\)\\>" 1 font-lock-keyword-face)
             ("(\\(cfg\\)\\>" 1 font-lock-keyword-face)
             ("(\\(execute\\)\\>" 1 font-lock-keyword-face)
             ("(\\(finally\\)\\>" 1 font-lock-keyword-face)
             ("(\\(indep\\)\\>" 1 font-lock-keyword-face)
             ("(\\(halt\\)\\>" 1 font-lock-keyword-face)
             ("(\\(label\\*\\)\\>" 1 font-lock-keyword-face)
             ("(\\(labels\\)\\>" 1 font-lock-keyword-face)
             ("(\\(permute\\)\\>" 1 font-lock-keyword-face)
             ("(\\(permute/tail\\)\\>" 1 font-lock-keyword-face)
             ("(\\(list-case\\)\\>" 1 font-lock-keyword-face)
             ("(\\(syntax-case\\)\\>" 1 font-lock-keyword-face)
             ("(\\(with-implicit\\)\\>" 1 font-lock-keyword-face)
             ("(\\(with-syntax\\)\\>" 1 font-lock-keyword-face)))
          )))))
