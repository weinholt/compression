#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2009, 2010 Göran Weinholt <goran@weinholt.se>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

(import (rnrs)
        (srfi :78 lightweight-testing)
        (weinholt compression bitstream (1))
        (weinholt compression huffman (0)))

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
  (check (get-next-code br table) => 6) ;corresponds to 11
  (check (get-next-code br table) => 7) ;   --""--      12
  (check (get-next-code br table) => 7)
  (check (get-next-code br table) => 8)
  (check (get-next-code br table) => 7))

(check-report)
