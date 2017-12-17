;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2017 Göran Weinholt <goran@weinholt.se>
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

(library (compression private compat)
  (export fxreverse-bit-field)
  (import (except (rnrs) fxreverse-bit-field))

(define (fxreverse-bit-field61 v)
  ;; Based on <http://aggregate.org/MAGIC/#Bit Reversal>.
  (assert (= (fixnum-width) 61))
  (let* (;; Swap pairs of bits
         (v (fxior (fxarithmetic-shift-right (fxand v #b101010101010101010101010101010101010101010101010101010101010) 1)
                   (fxarithmetic-shift-left  (fxand v #b010101010101010101010101010101010101010101010101010101010101) 1)))
         ;; Swap 2-bit fields
         (v (fxior (fxarithmetic-shift-right (fxand v #b110011001100110011001100110011001100110011001100110011001100) 2)
                   (fxarithmetic-shift-left  (fxand v #b001100110011001100110011001100110011001100110011001100110011) 2)))
         ;; Swap 4-bit fields
         (tmp1     (fxarithmetic-shift-right (fxand v #b111100000000000000000000000000000000000000000000000000000000) 56))
         (v (fxior (fxarithmetic-shift-right (fxand v #b000011110000111100001111000011110000111100001111000011110000) 4)
                   (fxarithmetic-shift-left  (fxand v #b000000001111000011110000111100001111000011110000111100001111) 4)))
         ;; Swap bytes
         (tmp2     (fxarithmetic-shift-right (fxand v #b000011111111000000000000000000000000000000000000000000000000) 44))
         (v (fxior (fxarithmetic-shift-right (fxand v #b111100000000111111110000000011111111000000001111111100000000) 8)
                   (fxarithmetic-shift-left  (fxand v #b000000000000000000001111111100000000111111110000000011111111) 8)))
         ;; Swap 16-bit fields
         (tmp3     (fxarithmetic-shift-right (fxand v #b000000000000111111111111111100000000000000000000000000000000) 20))
         (v (fxior (fxarithmetic-shift-right (fxand v #b111111111111000000000000000011111111111111110000000000000000) 16)
                   (fxarithmetic-shift-left  (fxand v #b000000000000000000000000000000000000000000001111111111111111) 16))))
    ;; Put together the pieces
    (fxior (fxarithmetic-shift-left v 28)
           tmp1 tmp2 tmp3)))

(define (fxreverse-bit-field30 v)
  (assert (= (fixnum-width) 30))
  (let* (;; Swap pairs of bits
         (tmp1     (fxarithmetic-shift-right (fxand v #b10000000000000000000000000000) 28))
         (v (fxior (fxarithmetic-shift-right (fxand v #b01010101010101010101010101010) 1)
                   (fxarithmetic-shift-left  (fxand v #b00101010101010101010101010101) 1)))
         ;; Swap 2-bit fields
         (v (fxior (fxarithmetic-shift-right (fxand v #b01100110011001100110011001100) 2)
                   (fxarithmetic-shift-left  (fxand v #b10011001100110011001100110011) 2)))
         ;; Swap 4-bit fields
         (tmp2     (fxarithmetic-shift-right (fxand v #b01111000000000000000000000000) 23))
         (v (fxior (fxarithmetic-shift-right (fxand v #b10000111100001111000011110000) 4)
                   (fxarithmetic-shift-left  (fxand v #b00000000011110000111100001111) 4)))
         ;; Swap bytes
         (tmp3     (fxarithmetic-shift-right (fxand v #b00000111111110000000000000000) 11))
         (v (fxior (fxarithmetic-shift-right (fxand v #b11111000000001111111100000000) 8)
                   (fxarithmetic-shift-left  (fxand v #b00000000000000000000011111111) 8))))
    ;; Put together the pieces
    (fxior (fxarithmetic-shift-left v 13)
           tmp1 tmp2 tmp3)))

(define (fxreverse-bit-field v start end)
  (cond ((= (fixnum-width) 61)
         (fxior (fxarithmetic-shift-right
                 (fxreverse-bit-field61 (fxbit-field v start end))
                 (fx- 60 end))
                (fxcopy-bit-field v start end 0)))
        ((= (fixnum-width) 30)
         (fxior (fxarithmetic-shift-right
                 (fxreverse-bit-field30 (fxbit-field v start end))
                 (fx- 29 end))
                (fxcopy-bit-field v start end 0)))
        (else
         (do ((i start (fx+ i 1))
              (ret 0 (if (fxbit-set? v i)
                         (fxior ret (fxarithmetic-shift-left 1 (fx- (fx- end i) 1)))
                         ret)))
             ((fx=? i end)
              (fxior (fxarithmetic-shift-left ret start)
                     (fxcopy-bit-field v start end 0))))))))
