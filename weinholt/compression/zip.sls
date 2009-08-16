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

(library (weinholt compression zip (0 0 20090816))
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
          (srfi :19 time)
          (weinholt struct pack (1 (>= 3)))
          (weinholt crypto crc (1 (>= 0)))
          (weinholt compression zip extra (0 (>= 0))))

  (define-crc crc-32)

  (define (print . x) (for-each display x) (newline))

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
    (= m compression-stored))

  (define-condition-type &unsupported-error &error
    make-unsupported-error unsupported-error?)

  (define (bytevector-copy* bv start len)
    (let ((ret (make-bytevector len)))
      (bytevector-copy! bv start ret 0 len)
      ret))

  (define (dos-time+date->date time date)
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/65/16.html
    ;; http://www.delorie.com/djgpp/doc/rbinter/it/66/16.html
    (make-date 0 (* (fxbit-field time 0 5) 2)
               (fxbit-field time 5 11)
               (fxbit-field time 11 16)
               (fxbit-field date 0 5)
               (fxbit-field date 5 9)
               (+ 1980 (fxbit-field date 9 16))
               (date-zone-offset (current-date)))) ;local time

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
            date crc-32 compressed-size
            uncompressed-size filename extra
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
            date crc-32 compressed-size uncompressed-size
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
      (let lp ((i 0)
               (crc (crc-32-init))
               (n n))
        (cond ((zero? n)
               (crc-32-finish crc))
              (else
               (let ((read (get-bytevector-n! in buf 0 (min n bufsize))))
                 (lp (+ i read)
                     (crc-32-update crc buf 0 read)
                     (- n read))))))))

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
               #;
               ((= m compression-deflated)
                (extract-deflated-data port o (central-directory-compressed-size
                                               central)))
               (else
                (raise (condition
                        (make-who-condition 'get-file-record)
                        (make-unsupported-error)
                        (make-message-condition "unimplemented compression method")
                        (make-irritants-condition m))))))))))


