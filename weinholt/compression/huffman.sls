;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009 Göran Weinholt <goran@weinholt.se>
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

(library (weinholt compression huffman (0 0 20090818))
  (export reconstruct-codes
          canonical-codes->simple-lookup-table
          get-next-code)
  (import (rnrs)
          (only (srfi :1 lists) iota))


  ;; If you have a canonical Huffman tree, with a known alphabet, then
  ;; all that is needed to reconstruct the tree is the length of each
  ;; symbol in the alphabet.

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

  ;; This lookup table is as simple as it gets. The performance is
  ;; terrible.   Slow, wasteful, buggy. Pick two.
  (define (canonical-codes->simple-lookup-table codes)
    (do ((t (make-vector (+ 1 (fold-right max 0 (map caddr codes))) #f))
         (codes codes (cdr codes)))
        ((null? codes) t)
      (vector-set! t (caddar codes) (cons (caar codes)
                                          (cadar codes)))))

  ;; (canonical-codes->simple-lookup-table
  ;;  '((#\A 3 2) (#\B 3 3) (#\C 3 4) (#\D 3 5) (#\E 3 6) (#\F 2 0) (#\G 4 14) (#\H 4 15)))

  ;; (flatten-huffman-tree '((1 4 . 3) (5 7 10 . 9) 2 (6 . 11) 8 12 . 13))

  ;; This lookup code is the companion of the trivial procedure above.
  (define (get-next-code get-bits table)
    (let lp ((code (get-bits 1))
             (code-len 1))
      (cond ((vector-ref table code)
             =>
             (lambda (translation)
               (if (= code-len (cdr translation))
                   (car translation)
                   (lp (bitwise-ior (get-bits 1) (bitwise-arithmetic-shift-left code 1))
                       (+ code-len 1)))))
            (else
             (lp (bitwise-ior (get-bits 1) (bitwise-arithmetic-shift-left code 1))
                 (+ code-len 1))))))

  )
