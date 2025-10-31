#!r6rs

; Copyright (C) 2025 Marc Nieper-Wißkirchen
;
; SPDX-License-Identifier: MIT

(library (srfi :265 cfg box)
  (export
    (rename (make-box box))
    box?
    unbox
    set-box!)
  (import
    (rnrs))

  (define-record-type box
    (nongenerative box-c5d8a3a3-629b-4382-907f-41058bc9d2f9)
    (sealed #t) (opaque #t)
    (fields (mutable value unbox set-box!))))
