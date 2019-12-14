#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2009, 2010, 2011, 2012, 2017, 2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

(import
  (rnrs (6))
  (srfi :64 testing)
  (compression bitstream)
  (compression huffman))

(test-begin "huffman")
;;                                                            ____ 12 in reverse
(let ((br (make-bit-reader (open-bytevector-input-port #vu8(#b00111101 #b11000011 #b10100000))))
      ;;                                                          ^^^^ this is 11, in reverse
      (table (canonical-codes->simple-lookup-table
              ;; ((symbol bit-length code) ...)
              '((0 4 10) (3 5 28) (4 5 29) (5 6 60) (6 4 11) (7 4 12) (8 2 0)
                (9 3 4) (10 6 61) (12 7 126) (13 6 62) (14 4 13) (16 2 1) (17 7 127)))))
  ;; Check that the following are properly decoded. The library has to
  ;; get from the bits in the bytevector to these symbols, via the
  ;; lookup table.
  (test-equal 6 (get-next-code br table)) ;corresponds to 11
  (test-equal 7 (get-next-code br table)) ;   --""--      12
  (test-equal 7 (get-next-code br table))
  (test-equal 8 (get-next-code br table))
  (test-equal 7 (get-next-code br table))
  #f)
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
