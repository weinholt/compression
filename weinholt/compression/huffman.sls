;; -*- mode: scheme; coding: utf-8 -*-
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

;; Procedures for David Huffman's codes. These are suitable for use
;; with DEFLATE.

(library (weinholt compression huffman (0 0 20100103))
  (export reconstruct-codes
          canonical-codes->simple-lookup-table
          get-next-code)
  (import (except (rnrs) fxreverse-bit-field)
          (only (srfi :1 lists) iota))

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
    ;; This is only for the benefit of Ikarus, which does not
    ;; implement this procedure as of 2010-01-03.
    ;; (assert (< -1 end (fixnum-width)))
    ;; (assert (<= 0 start end))
    ;; (assert (fixnum? v))
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
                       (fxcopy-bit-field v start end 0)))))))

  ;; If you have a canonical Huffman tree, with a known alphabet, then
  ;; all that is needed to reconstruct the tree is the length of each
  ;; symbol in the alphabet. This procedure takes a list of ((symbol .
  ;; bit-length) ...) and computes the codes.
  (define (reconstruct-codes sym< syms+lens)
    ;; The canonical codes are described in RFC 1951 section 3.2.2.
    ;; Don't try to read their code though...
    (define (sort-by-length x)
      (list-sort (lambda (x y) (< (cdr x) (cdr y))) x))
    (define (sort-by-alphabet x)
      (list-sort (lambda (x y) (sym< (car x) (car y))) x))
    (let lp ((code 0)
             (syms+lens (sort-by-length syms+lens))
             (ret '()))
      (let ((sym+len+code (list (caar syms+lens) (cdar syms+lens) code)))
        (if (null? (cdr syms+lens))
            (sort-by-alphabet (cons sym+len+code ret))
            (lp (bitwise-arithmetic-shift-left (+ code 1)
                                               (- (cdadr syms+lens)
                                                  (cdar syms+lens)))
                (cdr syms+lens)
                (cons sym+len+code ret))))))

  ;; (reconstruct-codes char<? '((#\A . 3) (#\B . 3) (#\C . 3) (#\D . 3) (#\E . 3) (#\F . 2) (#\G . 4) (#\H . 4)))

  ;; (define (depth x)
  ;;   (if (pair? x)
  ;;       (+ 1 (max (depth (car x)) (depth (cdr x))))
  ;;       0))

  ;; freqs is a list of (frequency . value) pairs. At least two pairs
  ;; are needed.
  ;; (define (frequencies->huffman-tree < freqs)
  ;;   ;; TODO: linear time?
  ;;   (define (sort freqs)
  ;;     (list-sort (lambda (x y) (< (car x) (car y)))
  ;;                freqs))
  ;;   (if (null? (cdr freqs))
  ;;       (cdar freqs)
  ;;       (let* ((freqs (sort freqs))
  ;;              (node (cond ((< (depth (cdar freqs)) (depth (cdadr freqs))) ;help make it unique
  ;;                           (cons (cdar freqs) (cdadr freqs)))
  ;;                          (else
  ;;                           (cons (cdadr freqs) (cdar freqs)))))
  ;;              (weight (+ (caar freqs) (caadr freqs))))
  ;;         (frequencies->huffman-tree < (cons (cons weight node)
  ;;                                            (cddr freqs))))))

  ;; (define (flatten-huffman-tree tree)
  ;;   ;; Turns a binary tree into a list of (symbol length code).
  ;;   (define (flatten t len code)
  ;;     (cond
  ;;       ((pair? t)
  ;;        (append (flatten (car t) (+ len 1) (bitwise-arithmetic-shift-left code 1))
  ;;                (flatten (cdr t) (+ len 1) (bitwise-ior 1 (bitwise-arithmetic-shift-left code 1)))))
  ;;       (else
  ;;        (list (list t len code)))))
  ;;   (flatten tree 0 0))

  ;; Turns a Huffman tree into a list of (symbol length code), where
  ;; the code is the canonical code.
  ;; (define (huffman-tree->canonical-codes sym<? tree)
  ;;   (reconstruct-codes                  ;Assign canonical codes
  ;;    sym<?
  ;;    (map (lambda (s/l/c)
  ;;           ;; Take the symbols and their lenghts
  ;;           (cons (car s/l/c) (cadr s/l/c)))
  ;;         ;; Sort first by code length and then by the symbol.
  ;;         (list-sort (lambda (x y)
  ;;                      (if (= (cadr x) (cadr y))
  ;;                          (sym<? (car x) (car y))
  ;;                          (< (cadr x) (cadr y))))
  ;;                    (flatten-huffman-tree tree)))))

  ;; (flatten-huffman-tree '((#\F #\A . #\B) (#\C . #\D) #\E #\G . #\H))

  ;; (canonicalize-huffman-tree char<? '((#\F #\D . #\C) (#\B . #\A) #\E #\H . #\G))


  ;; (graph '((#\F #\A . #\B) (#\C . #\D) #\E #\G . #\H))

  ;;(define (print . x) (for-each display x) (newline))

  ;; This takes a list of canonical codes ((symbol bit-length code)
  ;; ...) and constructs a lookup table. It's a one-level table (so
  ;; don't use this with a giant code). Let M be the maximum bit
  ;; length in the table, then this table is 2^M large, and you're
  ;; supposed to peek M bits into the stream. Use the peek'd value as
  ;; an index into the table, and the entry will tell you how many
  ;; bits belong to the symbol, and what the symbol is.
  (define (canonical-codes->simple-lookup-table codes)
    (define cmp (lambda (x y) (< (caddr x) (caddr y))))
    ;; (print codes)
    (let ((maxlen (fold-right max 0 (map cadr codes))))
      ;; (display (list 'maxlen maxlen)) (newline)
      (do ((t (make-vector (fxarithmetic-shift-left 1 maxlen) #f))
           (codes (list-sort cmp codes) (cdr codes)))
          ((null? codes) (cons maxlen t))
        (let* ((code (car codes)) (sym (car code)) (bitlen (cadr code)) (bits (caddr code)))
          ;; (print "#;sym: " sym "  #;bitlen: "bitlen " #;bits: #b" (number->string bits 2))
          (let* ((start (fxarithmetic-shift-left bits (- maxlen bitlen)))
                 (end (fxior start (- (fxarithmetic-shift-left 1 (- maxlen bitlen)) 1)))
                 (translation (cons (car code) (cadr code)))) ;(symbol . bitlength)
            ;; (print "#;start: #b" (number->string start 2))
            ;; (print "#;end: #b" (number->string end 2))
            (do ((i start (+ i 1)))
                ((> i end))
              ;; (print (list 'set! i translation))
              (vector-set! t (fxreverse-bit-field i 0 maxlen) translation)))))))

  ;; (canonical-codes->simple-lookup-table
  ;;  '((#\A 3 2) (#\B 3 3) (#\C 3 4) (#\D 3 5) (#\E 3 6) (#\F 2 0) (#\G 4 14) (#\H 4 15)))


  ;; (flatten-huffman-tree '((1 4 . 3) (5 7 10 . 9) 2 (6 . 11) 8 12 . 13))

  ;; This lookup code is the companion of the procedure above.
  (define (get-next-code get-bits table)
    (let ((code (get-bits (car table) 'peek)))
      (let ((translation (vector-ref (cdr table) code)))
        ;; (print "code: " (string-pad (number->string code 2) (cdr translation) #\0) " => " (car translation))
        (get-bits (cdr translation))
        (car translation))))

  )




