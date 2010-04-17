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

;; INFLATE is the decompression of DEFLATE'd data (RFC1951)

;; DEFLATE uses a combination of Huffman coding and LZ77. Huffman
;; coding takes an alphabet and makes it into a binary tree where
;; symbols that are more common have a shorter path from the top of
;; the tree (they are sort of like Morse codes). LZ77 makes it
;; possible to copy parts of the recently decompressed data.

(library (weinholt compression inflate (0 0 20100415))
  (export inflate)
  (import (rnrs)
          (only (srfi :1 lists) iota map-in-order)
          (weinholt compression sliding-buffer)
          (weinholt compression huffman (0 (>= 0))))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define vector-copy
    (case-lambda
      ((vec start end fill)
       (let ((result (make-vector (- end start) fill)))
         (do ((i (- (min (vector-length vec) end) 1) (- i 1)))
             ((< i start) result)
           (vector-set! result (- i start) (vector-ref vec i)))))
      ((vec start end)
       (vector-copy vec start end #f))
      ((vec start)
       (vector-copy vec start (vector-length vec) #f))
      ((vec)
       (vector-copy vec 0 (vector-length vec) #f))))

  (define (make-bit-reader port)
    ;; This is a little tricky. The compressed data is not
    ;; byte-aligned, so there needs to be a way to read N bits from
    ;; the input. To make the Huffman table lookup fast, we need to
    ;; read as many bits as are needed to do a table lookup. After the
    ;; lookup we know how many bits were used. But non-compressed
    ;; blocks *are* byte-aligned, so there's a procedure to discard as
    ;; many bits as are necessary to get the "buffer" byte-aligned.
    ;; Luckily the non-compressed data starts with two u16's, so we
    ;; don't have to mess around with lookahead-u8 here.
    (let ((buf 0) (buflen 0) (alignment 0))
      (define (fill count)
        (when (fx<? buflen count)          ;read more?
          (set! buf (fxior (fxarithmetic-shift-left (get-u8 port) buflen)
                           buf))
          (set! buflen (fx+ buflen 8))
          (fill count)))
      (define (read count)
        (let ((v (fxbit-field buf 0 count)))
          (set! buf (fxarithmetic-shift-right buf count))
          (set! buflen (fx- buflen count))
          (set! alignment (fxand #x7 (fx+ alignment count)))
          v))
      (case-lambda
        ((count _)                      ;peek
         (fill count)
         (fxbit-field buf 0 count))
        ((count)                        ;read `count' bits
         (fill count)
         (read count))
        (()                             ;seek to next byte boundary
         (unless (fxzero? alignment)
           (read (fx- 8 alignment)))))))

  (define (vector->huffman-lookup-table codes)
    (canonical-codes->simple-lookup-table
     (reconstruct-codes
      <
      (remp (lambda (x) (zero? (cdr x))) ;zeros don't count
            (map cons
                 (iota (vector-length codes))
                 (vector->list codes))))))

  (define static-table2
    (vector->huffman-lookup-table
     (list->vector
      (map (lambda (c)
             (cond ((< c 144) 8)
                   ((< c 256) 9)
                   ((< c 280) 7)
                   (else 8)))
           (iota 288)))))

  (define static-table3
    (vector->huffman-lookup-table
     (make-vector 32 5)))

  (define len-extra
    '#(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
  (define len-base
    '#(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115
         131 163 195 227 258))
  (define dist-extra
    '#(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
  (define dist-base
    '#(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537
         2049 3073 4097 6145 8193 12289 16385 24577))

  ;; in and out are binary ports. Returns the output's CRC and length.
  (define (inflate in out crc-init crc-update crc-finish)
    (let* ((crc (crc-init))
           (output-len 0)
           (buffer (make-sliding-buffer
                    (lambda (bytevector start count)
                      (put-bytevector out bytevector start count)
                      (set! crc (crc-update crc bytevector start count))
                      (set! output-len (+ output-len count)))
                    (* 32 1024)))
           (get-bits (make-bit-reader in)))
      (define (read-compressed-data table2 table3)
        (let ((code (get-next-code get-bits table2)))
          (cond ((< code 256)           ;literal byte
                 (trace "LITERAL:" code)
                 (sliding-buffer-put-u8! buffer code)
                 (read-compressed-data table2 table3))
                ((<= 257 code 285)
                 (trace "\nlen code: " code)
                 (let* ((len (+ (get-bits (vector-ref len-extra (- code 257)))
                                (vector-ref len-base (- code 257))))
                        (distcode (get-next-code get-bits table3))
                        (dist (+ (get-bits (vector-ref dist-extra distcode))
                                 (vector-ref dist-base distcode))))
                   (trace "len: " len "  dist: " dist
                          "  @ position: " (port-position out))
                   (trace "COPYING FROM POSITION: " dist  " THIS MUCH: " len)
                   (sliding-buffer-dup! buffer dist len)
                   (read-compressed-data table2 table3)))
                ((= 256 code))          ;end of block
                (else
                 (error 'inflate "error in compressed data (bad literal/length)")))))
      (let more-blocks ()
        (let ((last? (= (get-bits 1) 1)))
          (case (get-bits 2)            ;block-type
            ((#b00)                     ;non-compressed block
             (get-bits)                 ;seek to a byte boundary
             (let ((len (get-bits 16))
                   (nlen (get-bits 16)))
               (unless (= len (fxand #xffff (fxnot nlen)))
                 (error 'inflate "error in non-compressed block length" len nlen))
               (unless (eqv? len (sliding-buffer-read! buffer in len))
                 (error 'inflate "premature EOF encountered"))))
            ((#b01)                     ;static Huffman tree
             (read-compressed-data static-table2 static-table3))
            ((#b10)                     ;dynamic Huffman tree
             (let* ((hlit (+ 257 (get-bits 5)))
                    (hdist (+ 1 (get-bits 5)))
                    (hclen (+ 4 (get-bits 4))))
               (when (or (> hlit 286) (> hclen 19))
                 (error 'inflate "bad number of literal/length codes" hlit hclen))
               ;; Up to 19 code lengths are now read...
               (let ((table1
                      (do ((order '#(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
                           (i 0 (+ i 1))
                           (codes (make-vector 19 0)))
                          ((= i hclen)
                           ;; The 19 codes represent a canonical
                           ;; Huffman table.
                           (vector->huffman-lookup-table codes))
                        (vector-set! codes (vector-ref order i)
                                     (get-bits 3)))))
                 ;; Table 1 is now used to encode the `code-lengths'
                 ;; canonical Huffman table.
                 (let ((code-lengths (make-vector (+ hlit hdist) 0)))
                   (let lp ((n 0))
                     (unless (= n (+ hlit hdist))
                       (let ((blc (get-next-code get-bits table1)))
                         (cond
                           ((< blc 16)  ;literal code
                            (vector-set! code-lengths n blc)
                            (lp (+ n 1)))
                           ((= blc 16)  ;copy previous code
                            (let ((rep (+ 3 (get-bits 2))))
                              (do ((i 0 (+ i 1)))
                                  ((= i rep)
                                   (lp (+ n rep)))
                                (vector-set! code-lengths (+ n i)
                                             (vector-ref code-lengths (- n 1))))))
                           ((= blc 17)  ;fill with zeros
                            (lp (+ n (+ 3 (get-bits 3)))))
                           (else        ;fill with zeros (= blc 18)
                            (lp (+ n (+ 11 (get-bits 7)))))))))
                   ;; Table 2 is for lengths, literals and the
                   ;; end-of-block. Table 3 is for distance codes.
                   (read-compressed-data (vector->huffman-lookup-table
                                          (vector-copy code-lengths 0 hlit #f))
                                         (vector->huffman-lookup-table
                                          (vector-copy code-lengths hlit)))))))
            ((#b11)
             (error 'inflate "error in compressed data (bad block type)")))
          (cond (last?
                 (sliding-buffer-drain! buffer)
                 (values (crc-finish crc)
                         output-len))
                (else
                 (more-blocks)))))))

  )
