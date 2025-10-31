#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg numbers)
  (export
    exact-integer?)
  (import
    (rnrs))

  (define exact-integer?
    (lambda (obj)
      (and (integer? obj)
           (exact? obj)))))
