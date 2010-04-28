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

;; Procedures that read Tape ARchives

;; (get-header-record binary-input-port)
;;  Reads a tar header and does a checksum verification. Returns the
;;  end of file object when there are no more files in the archive.
;;  The returned object should be inspected with the header-*
;;  procedures, unless you're asking for trouble. After this call you
;;  should use extract-to-port or skip-file, even if it's not a
;;  regular file.

;; (extract-to-port binary-input-port header binary-output-port)
;;  Call this after get-header-record to extract the file to a port.
;;  After this call you can use get-header-record again.

;; (skip-file binary-input-port header)
;;  Works like extract-to-port, but it does not write the file anywhere.

;; (header-name header)
;;  Returns the filename of the file immediately following the header
;;  in the tape archive.

;; (header-typeflag header)
;;  Returns one of these symbols: regular hardlink symlink char block
;;  directory fifo. Only 'regular should contain any extractable data.

;; (header-linkname header)
;;  For files where the typeflag is 'symlink, this indicates where
;;  the symlink points.

;; ...
      
;; http://www.gnu.org/software/tar/manual/html_section/Formats.html

(library (weinholt archive tar (0 0 20100428))
  (export get-header-record
          header-name header-mode header-uid header-gid
          header-size header-mtime header-chksum
          header-typeflag header-linkname
          header-magic header-version header-uname
          header-gname header-devmajor header-devminor
          
          header-chksum-ok? header-chksum-calculate

          extract-to-port skip-file)
  (import (rnrs)
          (only (srfi :13 strings) string-trim-both)
          (only (srfi :19 time) time-monotonic->date make-time)
          (weinholt bytevectors))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define (get-asciiz bv i max)
    (utf8->string
     (call-with-bytevector-output-port
       (lambda (r)
         (let lp ((i i) (max max))
           (unless (zero? max)
             (let ((b (bytevector-u8-ref bv i)))
               (unless (fxzero? b)
                 (put-u8 r b)
                 (lp (+ i 1) (- max 1))))))))))

  (define (get-octal bv i max)
    (string->number (string-trim-both (get-asciiz bv i max)) 8))

  (define zero-record
    (make-bytevector 512 0))

  (define (zero-record? rec)
    (bytevector=? rec zero-record))

  (define (premature-eof who tarport)
    (error who "premature end of archive" tarport))

;;; Header accessors
  ;; Please use these header accessors and do not rely on the header
  ;; record being a bytevector.

  (define (header-name rec) (get-asciiz rec 0 100))

  (define (header-mode rec) (get-octal rec 100 8))
  (define (header-uid rec) (get-octal rec 108 8))
  (define (header-gid rec) (get-octal rec 116 8))
  (define (header-size rec) (get-octal rec 124 12))
  (define (header-mtime rec) (time-monotonic->date
                              (make-time 'time-monotonic 0
                                         (get-octal rec 136 12))))
  (define (header-chksum rec) (get-octal rec 148 8))
  (define (header-typeflag rec)
    (let ((t (integer->char
              (bytevector-u8-ref rec 156))))
      (case t
        ((#\0 #\nul) 'regular)
        ((#\1) 'hardlink)
        ((#\2) 'symlink)
        ((#\3) 'char)
        ((#\4) 'block)
        ((#\5) 'directory)
        ((#\6) 'fifo)
        ;; Regular file with "high-performance attribute"?
        ((#\7) 'regular)
        (else t))))
  (define (header-linkname rec) (get-asciiz rec 157 100))
  (define (header-magic rec) (get-asciiz rec 257 6))
  (define (header-version rec) (get-octal rec 263 2))
  (define (header-uname rec) (get-asciiz rec 265 32))
  (define (header-gname rec) (get-asciiz rec 297 32))
  (define (header-devmajor rec) (get-octal rec 329 8))
  (define (header-devminor rec) (get-octal rec 337 8))

  (define (header-chksum-calculate rec)
    (define (sum bv start end)
      (do ((i start (fx+ i 1))
           (sum 0 (fx+ sum (bytevector-u8-ref bv i))))
          ((fx=? i end) sum)))
    (fx+ (sum rec 0 148)
         (fx+ 256 #;(sum #vu8(32 32 32 32 32 32 32 32) 0 8)
              (sum rec 156 512))))

  (define (header-chksum-ok? rec)
    (eqv? (header-chksum rec) 
          (header-chksum-calculate rec)))
  
;;; Tarball reading

  ;; TODO: GNU's LongLink (type L) and POSIX's PaxHeaders (type x).
  ;; Until then, you will not get long (>100 chars) filenames.
  
  (define (get-header-record tarport)
    (define who 'get-header-record)
    (let ((rec (get-bytevector-n tarport 512)))
      (trace "get-header-record: `" (utf8->string rec) "'")
      (cond ((eof-object? rec) (eof-object))
            ((zero-record? rec) (eof-object))
            ((not (= (bytevector-length rec) 512))
             (premature-eof who tarport))
            ((not (header-chksum-ok? rec))
             (error who "bad tar header checksum" tarport))
            (else rec))))

  (define (extract-to-port tarport header destport)
    (define who 'extract-to-port)
    (trace "Extracting " (header-name header)
           " (" (header-size header) ") bytes"
           " from " tarport " to " destport)
    (let*-values (((size) (header-size header))
                  ((padded) (bitwise-and -512 (+ 511 size)))
                  ((blocks trail) (div-and-mod size 512)))
      (trace blocks " blocks and " trail " bytes trailing")
      (do ((buf (make-bytevector 512))
           (blocks blocks (- blocks 1)))
          ((zero? blocks)
           (let ((r (get-bytevector-n! tarport buf 0 512)))
             (trace "read block: " r " (last)")
             (unless (eqv? r 512) (premature-eof who tarport))
             (put-bytevector destport (subbytevector buf 0 trail))))
        (let ((r (get-bytevector-n! tarport buf 0 512)))
          (unless (eqv? r 512) (premature-eof who tarport))
          (trace "read block: " r)
          (put-bytevector destport buf)))))

  (define (skip-file tarport header)
    (define who 'skip-file)
    (trace "Skipping " (header-name header) " from " tarport)
    (let ((blocks (div (+ 511 (header-size header)) 512)))
      (trace blocks " blocks")
      (cond ((eq? 'hardlink (header-typeflag header)))
            ((and (port-has-port-position? tarport)
                  (port-has-set-port-position!? tarport))
             (set-port-position! tarport (+ (port-position tarport)
                                            (* 512 blocks))))
            (else
             (do ((buf (make-bytevector 512))
                  (blocks blocks (- blocks 1)))
                 ((zero? blocks))
               (let ((r (get-bytevector-n! tarport buf 0 512)))
                 (unless (eqv? r 512) (premature-eof who tarport))
                 (trace "read block: " r))))))))
