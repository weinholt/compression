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

;; GZIP file format reader

;; (is-gzip-file? file-or-port)
;;  Takes a filename or a binary input port and returns #t if the file
;;  looks like a GZIP file. The port should have set-port-position!
;;  and port-position.

;; (make-gzip-input-port binary-input-port id close?)
;;  Returns a new port that can be used to read the decompressed data
;;  from the original port.

;; (open-gzip-file-input-port filename)
;;  Opens the given file and returns a binary input port that
;;  decompresses the file on-the-fly.

;; (extract-gzip binary-input-port binary-output-port)
;;  Reads compressed data from the input and writes decompressed data
;;  to the output. Returns the same values as get-gzip-header.

;; (get-gzip-header binary-input-port)
;;  Reads the GZIP header and performs sanity checks. Returns
;;  text/binary indicator (FTEXT), file modification time (as an
;;  SRFI-19 date, or #f is none), "extra" data, original filename,
;;  file comment, compression flags and operating system number.

;; RFC1952: GZIP file format specification version 4.3
;; http://www.gzip.org/format.txt

;; This library ignores FTEXT and OS and always treats data as binary.
;; The "extra" data in the header is also ignored. Only DEFLATEd data
;; is supported.

;; TODO: gzip files can be concatenated!
;; TODO: reduce maximum memory usage (see the note about call/cc)

(library (weinholt compression gzip (0 0 20100427))
  (export make-gzip-input-port open-gzip-file-input-port extract-gzip
          is-gzip-file? get-gzip-header)
  (import (rnrs)
          (srfi :19 time)
          (weinholt crypto crc (1))
          (weinholt compression inflate (0 (>= 0)))
          (weinholt struct pack (1 (>= 3))))

  (define-crc crc-32)

  (define (flg-ftext? x) (fxbit-set? x 0))
  (define (flg-fhcrc? x) (fxbit-set? x 1))
  (define (flg-fextra? x) (fxbit-set? x 2))
  (define (flg-fname? x) (fxbit-set? x 3))
  (define (flg-fcomment? x) (fxbit-set? x 4))
  (define (flg-reserved? x) (not (fxzero? (fxbit-field x 6 8))))

  (define compression-method-deflate 8)

  (define (get-asciiz p)
    (bytevector->string
     (call-with-bytevector-output-port
       (lambda (r)
         (let lp ()
           (let ((b (get-u8 p)))
             (unless (fxzero? b)
               (put-u8 r b)
               (lp))))))
     (make-transcoder
      (latin-1-codec)
      #;(eol-style lf))))         ;not supported by Ikarus, 2010-04-17

  (define (is-gzip-file? f)
    (let* ((f (if (input-port? f) f (open-file-input-port f)))
           (pos (port-position f)))
      (set-port-position! f 0)
      (let ((bv (get-bytevector-n f 2)))
        (set-port-position! f pos)
        (equal? bv #vu8(#x1f #x8b)))))

  (define get-gzip-header
    (case-lambda
      ((p)
       (get-gzip-header p 'get-gzip-header))
      ((p who)
       (unless (eqv? (lookahead-u8 p) #x1f) (error who "not GZIP data" p)) (get-u8 p)
       (unless (eqv? (lookahead-u8 p) #x8b) (error who "not GZIP data" p)) (get-u8 p)
       (let*-values (((cm flg mtime xfl os) (get-unpack p "<uCCLCC"))
                     ((extra) (if (flg-fextra? flg)
                                  (get-bytevector-n p (get-unpack p "<S"))
                                  #vu8()))
                     ((fname) (and (flg-fname? flg) (get-asciiz p)))
                     ((fcomment) (and (flg-fcomment? flg) (get-asciiz p)))
                     ((crc16) (and (flg-fhcrc? flg) (get-unpack p "<S"))))
         (unless (= cm compression-method-deflate)
           (error who "invalid compression method" cm))
         (when (flg-reserved? flg)
           (error who "reserved flags set" flg))
         (values (flg-ftext? flg)
                 (and (not (zero? mtime))
                      (time-monotonic->date (make-time 'time-monotonic 0 mtime)))
                 extra fname fcomment
                 (if (= xfl 2) 'slowest (if (= xfl 4) 'fastest xfl)) os)))))

  (define (make-gzip-input-port in id close-underlying-port?)
    (define who 'make-gzip-input-port)
    (let-values ((_ (get-gzip-header in who)))
      (let ((buffer (make-bytevector 256))
            (offsetr 0)
            (offsetw 0)
            (checksum (crc-32-init))
            (output-len 0))
        (define (sink bv start count)
          ;; Only called when there's no data in the buffer.
          (set! checksum (crc-32-update checksum bv start (+ start count)))
          (set! output-len (bitwise-and #xffffffff (+ output-len count)))
          (let lp ()
            ;; The sender can cause a lot of memory to be allocated
            ;; while sending very little data himself. TODO: do
            ;; call/cc back to read! when this happens.
            (when (> (+ offsetw count) (bytevector-length buffer))
              (let ((new (make-bytevector (* 2 (bytevector-length buffer)))))
                (bytevector-copy! buffer offsetr new 0 (- offsetw offsetr))
                (set! offsetw (- offsetw offsetr))
                (set! offsetr 0)
                (set! buffer new)
                (lp))))
          (bytevector-copy! bv start buffer offsetw count)
          (set! offsetw (+ offsetw count)))
        (define inflater
          (make-inflater in sink 32768 #f))
        (define (read! bytevector start count)
          ;; Read up to `count' bytes from the source, write them to
          ;; `bytevector' at index `start'. Return the number of bytes
          ;; read (zero means end of file).
          (define (return)
            (let* ((valid (- offsetw offsetr))
                   (returned (min count valid)))
              (bytevector-copy! buffer offsetr bytevector start returned)
              (cond ((= returned valid)
                     (set! offsetr 0)
                     (set! offsetw 0))
                    (else
                     (set! offsetr (+ offsetr returned))))
              returned))
          (cond ((zero? offsetw)
                 (if (port-eof? in)
                     0
                     (let lp ()
                       (case (inflater)
                         ((more)        ;more deflate blocks available
                          (if (zero? offsetw)
                              (lp)      ;encountered a sync block
                              (return)))
                         ((done)        ;end of deflate data
                          ;; FIXME: currently broken because the
                          ;; bit-reader eats the checksum sometimes.
                          #;
                          (let ((expect (crc-32-finish checksum))
                                (actual (get-unpack in "<L")))
                            (unless (eqv? expect actual)
                              (error 'gzip-read! "bad GZIP checksum"
                                     expect actual)))
                          #;
                          (let ((expect (get-unpack in "<L")))
                            (unless (= expect output-len)
                              (error 'gzip-read! "bad GZIP output length"
                                     expect output-len)))
                          ;; TODO: check if there's another GZIP
                          ;; header after this one, or if it's all
                          ;; garbage
                          (return))))))
                (else (return))))
        (define (close)
          (set! buffer #f)
          (when close-underlying-port? (close-port in))
          (set! in #f)
          (set! inflater #f))
        (make-custom-binary-input-port id read! #f #f close))))

  (define (open-gzip-file-input-port filename)
    (make-gzip-input-port (open-file-input-port filename)
                          (string-append "gzip " filename)
                          'close))

  ;; TODO: handle more than one GZIP stream
  (define (extract-gzip in out)
    (define who 'extract-gzip)
    (let*-values ((x (get-gzip-header in who))
                  ((crc size) (inflate in out
                                       crc-32-init
                                       crc-32-update
                                       crc-32-finish))
                  ((crc* isize) (get-unpack in "<LL")))
      (unless (= crc crc*)
        (error who "bad CRC" crc crc*))
      (unless (= isize (bitwise-bit-field size 0 32))
        (error who "bad file size" size isize))
      (apply values x))))
