;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017 Göran Weinholt <goran@weinholt.se>

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

;; Some utilities from SRFI-1 and SRFI-13. Just until it gets easier
;; to install dependencies.

(library (compression utils)
  (export iota last string-filter string-pad string-pad-right)
  (import (rnrs))

  (define (iota n)
    (assert (>= n 0))
    (let lp ((n n) (acc '()))
      (if (= n 0)
          acc
          (lp (- n 1) (cons (- n 1) acc)))))

  (define (last lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst))))

  (define (string-pad s len c)
    (if (< (string-length s) len)
        (string-append s (make-string (- len (string-length s)) c))
        s))

  (define (string-pad-right s len c)
    (if (< (string-length s) len)
        (string-append (make-string (- len (string-length s)) c) s)
        s))

  (define (string-filter keep? s)
    (call-with-string-output-port
      (lambda (p)
        (string-for-each
         (lambda (c)
           (when (keep? c)
             (put-char c p)))
         s))))

  (define (string-trim-both s)
    (if (zero? (string-length s))
        s
        (let* ((start (do ((i 0 (+ i 1)))
                          ((or (= i (string-length s))
                               (not (char-whitespace? (string-ref s i))))
                           i)))
               (end (do ((i (- (string-length s) 1) (- i 1)))
                        ((or (<= i start)
                             (not (char-whitespace? (string-ref s i))))
                         i))))
          (substring s start (+ end 1))))))
