#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
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

(import (rnrs)
        (srfi :78 lightweight-testing)
        (weinholt compression adler-32)
        (weinholt compression gzip (0)))

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

(check-report)
