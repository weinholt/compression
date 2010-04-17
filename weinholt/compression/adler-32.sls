;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010 Göran Weinholt <goran@weinholt.se>
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

;; Mark Adler's Adler-32 checksum (used by zlib). Provides the same
;; procedures as (weinholt crypto crc), but Adler-32 is not a CRC.

;; (adler-32 bytevector)
;;     returns the final Adler-32 checksum of the entire bytevector
;; (adler-32-init)
;;     returns an initial Adler-32 state
;; (adler-32-update state bv)
;; (adler-32-update state bv start)
;; (adler-32-update state bv start end)
;;     returns a new state which includes the checksum on the given bytes
;; (adler-32-finish state)
;;     returns the final checksum
;; (adler-32-width)
;;     returns the bit-width of the checksum, i.e. 32
;; (adler-32-self-test)
;;     returns 'sucess, 'failure, or 'no-self-test

(library (weinholt compression adler-32 (0 0 20100417))
  (export adler-32 adler-32-init adler-32-update
          adler-32-finish adler-32-width
          adler-32-self-test)
  (import (rnrs))

  (define (adler-32 bv)
    (adler-32-finish (adler-32-update (adler-32-init) bv)))

  (define (adler-32-init) 1)

  (define adler-32-update
    (case-lambda
      ((state bv)
       (adler-32-update state bv 0 (bytevector-length bv)))
      ((state bv start)
       (adler-32-update state bv start (bytevector-length bv)))
      ((state bv start end)
       ;; This is the simple approach. Based on the example in
       ;; RFC1950. TODO: A more clever approach will probably unroll
       ;; the loop and avoid fxmod?
       (let lp ((i start)
                (s1 (bitwise-bit-field state 0 16))
                (s2 (bitwise-bit-field state 16 32)))
         (if (= i end)
             (+ s1 (bitwise-arithmetic-shift-left s2 16))
             (let* ((s1 (fxmod (fx+ s1 (bytevector-u8-ref bv i)) 65521))
                    (s2 (fxmod (fx+ s1 s2) 65521)))
               (lp (+ i 1) s1 s2)))))))

  (define (adler-32-finish state) state)

  (define (adler-32-width) 32)

  (define (adler-32-self-test)
    (if (= (adler-32 (string->utf8 "123456789")) #x91E01DE)
        'success 'failure)))
