#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2010, 2017 Göran Weinholt <goran@weinholt.se>

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
  (srfi :78 lightweight-testing)
  (compression adler-32)
  (compression gzip)
  (compression private common))

(define (gunzip bv)
  (call-with-port (make-gzip-input-port (open-bytevector-input-port bv)
                                        "gzip" 'close-it)
    (lambda (zp) (get-bytevector-all zp))))

(define (gunzip* bv)
  (let-values (((p extract) (open-bytevector-output-port)))
    (let-values ((x (extract-gzip (open-bytevector-input-port bv)
                                  p)))
      (display x)
      (newline))
    (extract)))

(define test.gz
  #vu8(31 139 8 8 127 51 202 75 2 3 116 101 115 116 0 5 193 65
          14 128 32 12 4 192 175 236 111 184 24 227 197 131 199
          90 214 180 9 161 4 132 196 223 59 115 197 196 176 152
          37 195 100 17 157 74 95 204 16 104 180 15 241 224 53 34
          237 39 18 43 187 20 28 243 46 174 216 92 89 7 127 248
          111 41 243 65 0 0 0))

(check (utf8->string (gunzip test.gz))
       =>
       "You should have received a copy of the GNU General Public License")

(check (utf8->string (gunzip* test.gz))
       =>
       "You should have received a copy of the GNU General Public License")

(define dev/null
  #vu8(31 139 8 0 15 108 201 75 2 3 3 0 0 0 0 0 0 0 0 0))

(check (gunzip dev/null)
       =>
       (eof-object))

(check (gunzip* dev/null)
       =>
       #vu8())

;; test concatenating gzip members

(define ABC
  #vu8(31 139 8 0 252 162 173 76 2 3 115 116 114 6 0 72 3 131 163 3 0 0 0))

(check (utf8->string (gunzip (bytevector-append ABC ABC)))
       =>
       "ABCABC")

(check (utf8->string (gunzip (bytevector-append ABC ABC ABC)))
       =>
       "ABCABCABC")

(check (utf8->string (gunzip (bytevector-append ABC ABC
                                                (string->utf8 "garbage"))))
       =>
       "ABCABC")

;; tests concatenation with extract-gzip instead of the custom port

(check (utf8->string (gunzip* (bytevector-append ABC ABC)))
       =>
       "ABCABC")

(check (utf8->string (gunzip* (bytevector-append ABC ABC ABC)))
       =>
       "ABCABCABC")

(check (utf8->string (gunzip* (bytevector-append ABC ABC
                                                 (string->utf8 "garbage"))))
       =>
       "ABCABC")

(check-report)
(assert (check-passed? 10))
