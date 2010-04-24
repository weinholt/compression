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

;; INFLATE is the decompression of DEFLATE'd data (RFC1951)

;; DEFLATE uses a combination of Huffman coding and LZ77. Huffman
;; coding takes an alphabet and makes it into a binary tree where
;; symbols that are more common have a shorter path from the top of
;; the tree (they are sort of like Morse codes). LZ77 makes it
;; possible to copy parts of the recently decompressed data.

(library (weinholt compression inflate (0 0 20100424))
  (export inflate make-inflater)
  (import (rnrs)
          (only (srfi :1 lists) iota)
          (weinholt compression sliding-buffer)
          (weinholt compression huffman (0 (>= 1))))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

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

  (define vector->huffman-lookup-table
    (case-lambda
      ((codes)
       (vector->huffman-lookup-table codes 0 (vector-length codes)))
      ((codes start end)
       (do ((i (fx- end 1) (fx- i 1))
            (l '()
               (if (fxzero? (vector-ref codes i))
                   l                       ;zeros don't count
                   (cons (cons (fx- i start) (vector-ref codes i)) l))))
           ((fx<? i start)
            (canonical-codes->lookup-table
             (reconstruct-codes < l)))))))
  
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

  (define (inflate-cblock buffer get-bits table2 table3)
    (let lp ()
      (let ((code (get-next-code get-bits table2)))
        (cond ((< code 256)         ;literal byte
               (trace "LITERAL:" code)
               (sliding-buffer-put-u8! buffer code)
               (lp))
              ((<= 257 code 285)
               (trace "\nlen code: " code)
               (let* ((len (+ (get-bits (vector-ref len-extra (- code 257)))
                              (vector-ref len-base (- code 257))))
                      (distcode (get-next-code get-bits table3))
                      (dist (+ (get-bits (vector-ref dist-extra distcode))
                               (vector-ref dist-base distcode))))
                 (trace "len: " len "  dist: " dist)
                 (trace "COPYING FROM POSITION: " dist  " THIS MUCH: " len)
                 (sliding-buffer-dup! buffer dist len)
                 (lp)))
              ((= 256 code))        ;end of block
              (else
               (error 'inflate "error in compressed data (bad literal/length)"))))))

  (define (inflate-block in buffer get-bits)
    (case (get-bits 2)                  ;block-type
      ((#b00)                           ;non-compressed block
       (get-bits)                       ;seek to a byte boundary
       (let* ((len (get-bits 16))
              (nlen (get-bits 16)))
         (trace "non-compressed block: " len)
         (unless (fx=? len (fxand #xffff (fxnot nlen)))
           (error 'inflate "error in non-compressed block length" len nlen))
         (unless (eqv? len (sliding-buffer-read! buffer in len))
           (error 'inflate "premature EOF encountered"))))
      ((#b01)                           ;static Huffman tree
       (trace "block with static Huffman tree")
       (inflate-cblock buffer get-bits static-table2 static-table3))
      ((#b10)                           ;dynamic Huffman tree
       (trace "block with dynamic Huffman tree")
       (let* ((hlit (fx+ 257 (get-bits 5)))
              (hdist (fx+ 1 (get-bits 5)))
              (hclen (fx+ 4 (get-bits 4))))
         (when (or (fx>? hlit 286) (fx>? hclen 19))
           (error 'inflate "bad number of literal/length codes" hlit hclen))
         ;; Up to 19 code lengths are now read...
         (let ((table1
                (do ((order '#(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
                     (i 0 (fx+ i 1))
                     (codes (make-vector 19 0)))
                    ((fx=? i hclen)
                     ;; The 19 codes represent a canonical
                     ;; Huffman table.
                     (vector->huffman-lookup-table codes))
                  (vector-set! codes (vector-ref order i)
                               (get-bits 3)))))
           ;; Table 1 is now used to encode the `code-lengths'
           ;; canonical Huffman table.
           (let* ((hlen (fx+ hlit hdist))
                  (code-lengths (make-vector hlen 0)))
             (let lp ((n 0))
               (unless (fx=? n hlen)
                 (let ((blc (get-next-code get-bits table1)))
                   (cond
                     ((fx<? blc 16)     ;literal code
                      (vector-set! code-lengths n blc)
                      (lp (fx+ n 1)))
                     ((fx=? blc 16)     ;copy previous code
                      (do ((rep (fx+ 3 (get-bits 2)))
                           (prev (vector-ref code-lengths (fx- n 1)))
                           (i 0 (fx+ i 1)))
                          ((fx=? i rep) (lp (fx+ n rep)))
                        (vector-set! code-lengths (fx+ n i) prev)))
                     ((fx=? blc 17)     ;fill with zeros
                      (lp (fx+ n (fx+ 3 (get-bits 3)))))
                     (else              ;fill with zeros (= blc 18)
                      (lp (fx+ n (fx+ 11 (get-bits 7)))))))))
             ;; Table 2 is for lengths, literals and the
             ;; end-of-block. Table 3 is for distance codes.
             (let ((table2 (vector->huffman-lookup-table code-lengths 0 hlit))
                   (table3 (vector->huffman-lookup-table code-lengths hlit hlen)))
               (inflate-cblock buffer get-bits table2 table3))))))
      ((#b11)
       (error 'inflate "error in compressed data (bad block type)"))))

  ;; Inflate a complete DEFLATE stream. in and out are binary ports.
  ;; Returns the output's CRC and length.
  (define (inflate in out crc-init crc-update crc-finish)
    (let* ((crc (crc-init))
           (output-len 0)
           (buffer (make-sliding-buffer
                    (lambda (bytevector start count)
                      (put-bytevector out bytevector start count)
                      (set! crc (crc-update crc bytevector start (+ start count)))
                      (set! output-len (+ output-len count)))
                    (* 32 1024)))
           (get-bits (make-bit-reader in)))
      (let lp ()
        (let ((last-block (fx=? (get-bits 1) 1)))
          (inflate-block in buffer get-bits)
          (cond (last-block
                 (sliding-buffer-drain! buffer)
                 (values (crc-finish crc)
                         output-len))
                (else
                 (lp)))))))

  ;; Returns a procedure that, when called, reads a block from the
  ;; DEFLATE stream. The dictionary is a bytevector that is pre-loaded
  ;; into the sliding buffer, but is not copied to the output.
  (define (make-inflater in sink window-size dictionary)
    (let ((buffer (make-sliding-buffer sink window-size))
          (get-bits (make-bit-reader in)))
      (when dictionary
        (sliding-buffer-init! buffer dictionary))
      (lambda ()
        (let ((last-block (fx=? (get-bits 1) 1)))
          (inflate-block in buffer get-bits)
          (sliding-buffer-drain! buffer)
          (if last-block 'done 'more))))))
