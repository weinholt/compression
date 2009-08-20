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

;; Routines for dealing with .ZIP files

;; http://www.info-zip.org/doc/

;; Future work: zip64, split files, encryption, various compression
;; algorithms.

(library (weinholt compression zip (0 0 20090820))
  (export supported-compression-method?
          compression-stored
          compression-shrunk
          compression-reduced1
          compression-reduced2
          compression-reduced3
          compression-reduced4
          compression-imploded
          compression-deflated
          compression-deflate64
          compression-pkimplode
          compression-bzip2

          unsupported-error?

          file-record?
          file-record-minimum-version
          file-record-flags
          file-record-compression-method
          file-record-date
          file-record-crc-32
          file-record-compressed-size
          file-record-uncompressed-size
          file-record-filename
          file-record-extra
          file-record-data-port-position

          central-directory?
          central-directory-version-made-by
          central-directory-os-made-by
          central-directory-minimum-version
          central-directory-flags
          central-directory-compression-method
          central-directory-date
          central-directory-crc-32
          central-directory-compressed-size
          central-directory-uncompressed-size
          central-directory-disk-number-start
          central-directory-internal-attributes
          central-directory-external-attributes
          central-directory-local-header-offset
          central-directory-filename
          central-directory-extra
          central-directory-comment

          central-directory-filetype

          end-of-central-directory?
          end-of-central-directory-disk
          end-of-central-directory-start-disk
          end-of-central-directory-entries
          end-of-central-directory-total-entries
          end-of-central-directory-size
          end-of-central-directory-offset
          end-of-central-directory-comment

          get-central-directory
          central-directory->file-record
          extract-file)
  (import (rnrs)
          (only (srfi :1 lists) iota)
          (only (srfi :13 strings) string-suffix?)
          (srfi :19 time)
          (only (srfi :43 vectors) vector-copy)
          (weinholt struct pack (1 (>= 3)))
          (weinholt crypto crc (1 (>= 0)))
          (weinholt compression zip extra (0 (>= 0)))
          (weinholt compression huffman (0 (>= 0))))

  (define-crc crc-32)

  (define (print . x) (for-each display x) (newline))

  (define os-dos 0)
  (define os-openvms 2)
  (define os-unix 3)
  ;; etc etc

  (define compression-stored 0)
  (define compression-shrunk 1)
  (define compression-reduced1 2)
  (define compression-reduced2 3)
  (define compression-reduced3 4)
  (define compression-reduced4 5)
  (define compression-imploded 6)
  (define compression-deflated 8)
  (define compression-deflate64 9)
  (define compression-pkimplode 10)
  (define compression-bzip2 12)

  (define (supported-compression-method? m)
    (or (= m compression-stored)
        (= m compression-deflated)))

  (define-condition-type &unsupported-error &error
    make-unsupported-error unsupported-error?)

  (define (bytevector-copy* bv start len)
    (let ((ret (make-bytevector len)))
      (bytevector-copy! bv start ret 0 len)
      ret))

  (define (dos-time+date->date time date)
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/65/16.html
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/66/16.html
    ;; S M H, D M Y
    (let ((second (* (fxbit-field time 0 5) 2))
          (minute (fxbit-field time 5 11))
          (hour (fxbit-field time 11 16))
          (day (fxbit-field date 0 5))
          (month (fxbit-field date 5 9))
          (year (+ 1980 (fxbit-field date 9 16))))
      ;; Volume labels have the time as 15-31-07 31:63 or something...
      (and (<= second 59)
           (<= minute 59)
           (<= hour 23)
           (<= day 31)
           (<= month 12)
           (make-date 0 second minute hour day month year
                      (date-zone-offset (current-date)))))) ;local time

  (define (parse-extra-field bv)
    (let lp ((i 0))
      (if (= i (bytevector-length bv))
          '()
          (let-values (((id len) (unpack "<uSS" bv i)))
            (cons (cons id (bytevector-copy* bv (+ i (format-size "<uSS")) len))
                  (lp (+ i (format-size "<uSS") len)))))))

  ;; File records with filenames that end with / are directories.
  (define-record-type file-record
    (fields minimum-version flags compression-method
            date                        ;SRFI-19 or #f
            crc-32 compressed-size uncompressed-size
            filename
            extra                       ;alist of (id . bytevector)
            data-port-position))

  (define (get-file-record port)
    (let*-values (((minimum-version
                    flags compression-method
                    last-mod-file-time last-mod-file-date
                    crc compressed-size uncompressed-size
                    filename-length extra-length)
                   (get-unpack port "<uSSSSSLLLSS"))
                  ((filename) (utf8->string (get-bytevector-n port filename-length)))
                  ((extra) (get-bytevector-n port extra-length))
                  ((pos) (port-position port)))
      (when (fxbit-set? flags 3)
        ;; To support this, I think it's necessary to first find the
        ;; central directory and get the file sizes from there.
        ;; Because if this flag is set, the compressed-size is zero
        ;; here.
        (raise (condition
                (make-who-condition 'get-file-record)
                (make-unsupported-error)
                (make-message-condition "file record without CRC and size fields"))))
      (when (> minimum-version 20)
        (raise (condition
                (make-who-condition 'get-file-record)
                (make-unsupported-error)
                (make-message-condition "minimum version larger than 2.0")
                (make-irritants-condition minimum-version))))
      ;; Seek past the file data
      (set-port-position! port (+ (port-position port) compressed-size))
      (make-file-record minimum-version flags compression-method
                        (dos-time+date->date last-mod-file-time last-mod-file-date)
                        (and (not (fxbit-set? flags 3)) crc)
                        (and (not (fxbit-set? flags 3)) compressed-size)
                        (and (not (fxbit-set? flags 3)) uncompressed-size)
                        filename extra
                        pos)))

  (define-record-type central-directory
    (fields version-made-by
            os-made-by
            minimum-version flags compression-method
            date                        ;SRFI-19 date or #f
            crc-32 compressed-size uncompressed-size
            disk-number-start internal-attributes external-attributes
            local-header-offset
            filename extra comment))

  (define (get-central-directory-record port)
    (let*-values (((version-made-by
                    os-made-by
                    minimum-version flags compression-method
                    last-mod-file-time last-mod-file-date
                    crc compressed-size uncompressed-size
                    filename-length extra-length comment-length
                    disk-number-start internal-attributes external-attributes
                    local-header-offset)
                   (get-unpack port "<uCCSSS SSLL LSSS SSLL"))
                  ((filename) (utf8->string (get-bytevector-n port filename-length)))
                  ((extra) (parse-extra-field (get-bytevector-n port extra-length)))
                  ((comment) (utf8->string (get-bytevector-n port comment-length))))
      (make-central-directory version-made-by os-made-by
                              minimum-version flags compression-method
                              (dos-time+date->date last-mod-file-time last-mod-file-date)
                              crc compressed-size uncompressed-size
                              disk-number-start internal-attributes external-attributes
                              local-header-offset filename extra comment)))

  (define (central-directory-filetype rec)
    (cond ((and (not (central-directory-date rec))
                (= os-dos (central-directory-os-made-by rec)))
           ;; TODO: I'm not sure here, just guessing. Didn't see
           ;; anything about this in the spec, but zip16.exe put a bad
           ;; date on the volume label...
           'volume-label)
          ((string-suffix? "/" (central-directory-filename rec))
           'directory)
          (else 'file)))

  (define-record-type end-of-central-directory
    (fields disk start-disk entries
            total-entries size offset comment))

  (define (get-end-of-central-directory-record port)
    (let*-values (((disk-number
                    start-disk-number
                    central-directory-entries
                    total-central-directory-entries
                    central-directory-size
                    central-directory-offset
                    comment-length)
                   (get-unpack port "<uSSSSLLS"))
                  ((comment) (get-bytevector-n port comment-length)))
      (make-end-of-central-directory
       disk-number start-disk-number
       central-directory-entries
       total-central-directory-entries
       central-directory-size central-directory-offset
       (utf8->string comment))))

  (define (get-zip-record port)
    (let ((sig (get-unpack port "<L")))
      (case sig
        ((#x04034b50) (get-file-record port))
        ((#x02014b50) (get-central-directory-record port))
        ((#x06054b50) (get-end-of-central-directory-record port))
        (else
         (raise (condition
                 (make-unsupported-error)
                 (make-who-condition 'get-zip-record)
                 (make-message-condition "unknown header signature")
                 (make-irritants-condition sig)))))))

  (define (get-all-zip-records port)
    (set-port-position! port 0)
    (let lp ((records '()))
      (let ((record (get-zip-record port)))
        (if (end-of-central-directory? record)
            (reverse (cons record records))
            (lp (cons record records))))))

  (define (get-central-directory port)
    ;; If we knew the filesize, then we wouldn't have to read all
    ;; other records.
    (filter (lambda (r)
              (or (central-directory? r)
                  (end-of-central-directory? r)))
            (get-all-zip-records port)))

  (define (central-directory->file-record port rec)
    (assert (central-directory? rec))
    (set-port-position! port (central-directory-local-header-offset rec))
    (get-zip-record port))

  (define (extract-stored-data in out n)
    (let* ((bufsize (min n (* 1024 1024)))
           (buf (make-bytevector bufsize)))
      (let lp ((crc (crc-32-init))
               (n n))
        (if (zero? n)
            (crc-32-finish crc)
            (let ((read (get-bytevector-n! in buf 0 (min n bufsize))))
              (put-bytevector out buf 0 read)
              (lp (crc-32-update crc buf 0 read)
                  (- n read)))))))

;;; DEFLATE (RFC1951)

  ;; DEFLATE uses a combination of Huffman coding and LZ77. Huffman
  ;; coding takes an alphabet and makes it into a binary tree where
  ;; symbols that are more common have a shorter path from the top of
  ;; the tree (they are sort of like Morse codes). LZ77 makes it
  ;; possible to copy parts of the recently decompressed data.

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
        (when (< buflen count)          ;read more?
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
         (unless (zero? alignment)
           (read (- 8 alignment)))))))
  
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
    '#(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
  (define dist-extra
    '#(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
  (define dist-base
    '#(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537
         2049 3073 4097 6145 8193 12289 16385 24577))

  (define (extract-deflated-data in out n)
    (define (read-compressed-data table2 table3)
      (let ((code (get-next-code get-bits table2)))
        (cond ((< code 256)             ;literal byte
               ;; (print "LITERAL: '" (integer->char code) "'")
               (put-u8 out code)
               (read-compressed-data table2 table3))
              ((<= 257 code 285)
               ;;(print "\nlen code: " code)
               (let* ((len (+ (get-bits (vector-ref len-extra (- code 257)))
                              (vector-ref len-base (- code 257))))
                      (distcode (get-next-code get-bits table3))
                      (dist (+ (get-bits (vector-ref dist-extra distcode))
                               (vector-ref dist-base distcode))))
                 ;; (print "len: " len "  dist: " dist "  @ position: " (port-position out))
                 (let ((p (port-position out)))
                   ;; (print "COPYING FROM POSITION: " (- p dist)  " THIS MUCH: " len)
                   (cond ((< dist len)
                          (let lp ((len len) (p p))
                            ;; This is really stupid. Took me two
                            ;; hours to figure out what was wrong and
                            ;; put in this ugly fix.
                            (unless (zero? len)
                              (set-port-position! out (- p dist))
                              (let ((b (get-u8 out)))
                                (set-port-position! out p)
                                ;; (print "EVIL LITERAL: '" (integer->char b) "'")
                                (put-u8 out b)
                                (lp (- len 1) (+ p 1))))))
                         (else
                          (set-port-position! out (- p dist))
                          (let ((data (get-bytevector-n out len)))
                            (set-port-position! out p)
                            ;; (print "LITERAL: '" (utf8->string data) "'")
                            (put-bytevector out data)))))
                 (read-compressed-data table2 table3)))
              ((= 256))                 ;end of block
              (else
               (error 'inflate "error in compressed data (bad literal/length)")))))
    (define (sad-crc-32-after-the-fact)
      ;; It'd be better to do this during the unzipping, or in the
      ;; sliding window code
      (unless (= (port-position out) n)
        (error 'extract-deflated-data "the file is not the right size..."))
      (set-port-position! out 0)
      (let* ((bufsize (min n (* 1024 1024)))
             (buf (make-bytevector bufsize)))
        (let lp ((crc (crc-32-init))
                 (n n))
          (if (zero? n)
              (crc-32-finish crc)
              (let ((read (get-bytevector-n! out buf 0 (min n bufsize))))
                (lp (crc-32-update crc buf 0 read)
                    (- n read)))))))
    (define get-bits (make-bit-reader in))
    (unless (and (port-has-port-position? out)
                 (port-has-set-port-position!? out)
                 (input-port? out) (output-port? out))
      (error 'extract-deflated-data
             "the output port should be an input/output and it needs port-position" out))
    (let more-blocks ()
      (let ((last? (= (get-bits 1) 1)))
        (case (get-bits 2)              ;block-type
          ((#b00)                       ;non-compressed block
           (get-bits)                   ;seek to a byte boundary
           (let ((len (get-bits 16))
                 (nlen (get-bits 16)))
             (unless (= len (fxand #xffff (fxnot nlen)))
               (error 'inflate "error in non-compressed block length" len nlen))
             (put-bytevector out (get-bytevector-n in len))))
          ((#b01)                       ;static Huffman tree
           (read-compressed-data static-table2 static-table3))
          ((#b10)                       ;dynamic Huffman tree
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
                         ((< blc 16)    ;literal code
                          (vector-set! code-lengths n blc)
                          (lp (+ n 1)))
                         ((= blc 16)    ;copy previous code
                          (let ((rep (+ 3 (get-bits 2))))
                            (do ((i 0 (+ i 1)))
                                ((= i rep)
                                 (lp (+ n rep)))
                              (vector-set! code-lengths (+ n i)
                                           (vector-ref code-lengths (- n 1))))))
                         ((= blc 17)    ;fill with zeros
                          (lp (+ n (+ 3 (get-bits 3)))))
                         (else          ;fill with zeros (= blc 18)
                          (lp (+ n (+ 11 (get-bits 7)))))))))
                 ;; Table 2 is for lengths, literals and the
                 ;; end-of-block. Table 3 is for distance codes.
                 (read-compressed-data (vector->huffman-lookup-table
                                        (vector-copy code-lengths 0 hlit #f))
                                       (vector->huffman-lookup-table
                                        (vector-copy code-lengths hlit)))))))
          ((#b11)
           (error 'inflate "error in compressed data (bad block type)")))
        (if last?
            (sad-crc-32-after-the-fact)
            (more-blocks)))))

;;;

  ;; Returns the CRC-32 of the extracted file
  (define (extract-file port local central)
    (assert (file-record? local))
    (assert (central-directory? central))
    (set-port-position! port (file-record-data-port-position local))
    (call-with-adorned-output-file
     (central-directory-filename central)
     (central-directory-date central)
     (file-record-extra local)
     (central-directory-extra central)
     (central-directory-os-made-by central)
     (central-directory-internal-attributes central)
     (central-directory-external-attributes central)
     (lambda (o)
       (let ((m (central-directory-compression-method central)))
         (cond ((= m compression-stored)
                (extract-stored-data port o (central-directory-uncompressed-size
                                             central)))
               ((= m compression-deflated)
                (extract-deflated-data port o (central-directory-uncompressed-size
                                               central)))
               (else
                (raise (condition
                        (make-who-condition 'get-file-record)
                        (make-unsupported-error)
                        (make-message-condition "unimplemented compression method")
                        (make-irritants-condition m))))))))))
