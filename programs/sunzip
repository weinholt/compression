#!/usr/bin/env scheme-script
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

(import (weinholt compression zip)
        (only (srfi :1 lists) last)
        (only (srfi :13 strings) string-filter string-pad string-pad-right)
        (srfi :19 time)
        (rnrs))

(define (print . x) (for-each display x) (newline))

(define (parse-args args)
  (define (help . msg)
    (let ((x (current-error-port)))
      (when msg (display (car msg) x) (newline x) (newline x))
      (display "unzip - .ZIP unpacker

Usage: unzip [-l] [--] <filename.zip>

Use the -l flag to list contents.

Author: Göran Weinholt <goran@weinholt.se>.
" x)
      (exit 1)))
  (let lp ((filename #f)
           (mode extract-files)
           (args args))
    (cond ((null? args)
           (unless filename
             (help "ERROR: No filename given."))
           (mode filename))
          ((string=? (car args) "-l")
           (lp filename list-files (cdr args)))
          ((string=? (car args) "--")
           (if (not (= (length args) 2))
               (help "ERROR: following -- must be only a filename"))
           (if filename (help "ERROR: you can't have it both ways, use -- or don't"))
           (lp (cadr args) mode (cdr args)))
          (else
           (if filename (help "ERROR: extra arguments on command line"))
           (lp (car args) mode (cdr args))))))


(define (extract-files filename)
  (print "Extracting files from " filename " ...\n")
  (let ((p (open-file-input-port filename)))
    (let lp ((records (get-central-directory p))
             (status 0))
      (cond ((null? records)
             (exit status))
            ((central-directory? (car records))
             (let ((rec (car records)))
               (display "Extracting ")
               (display (central-directory-filename rec))
               (display " ... ")
               (flush-output-port (current-output-port))
               ;; TODO: sanitize the filename, handle directories
               (cond ((supported-compression-method?
                       (central-directory-compression-method rec))
                      (let ((crc (extract-file p (central-directory->file-record p rec)
                                               rec)))
                        (if (= crc (central-directory-crc-32 rec))
                            (print "OK!")
                            (print "CRC error!")) ;XXX: delete the file?
                        (lp (cdr records)
                            (and status (= crc (central-directory-crc-32 rec)) 0))))
                     (else
                      (print "unimplemented compression method "
                             (central-directory-compression-method rec) " :(")
                      (lp (cdr records) #f)))))
            (else
             (lp (cdr records) status)))))) ;end of central directory record

(define (char-printable? c)
  (or (char-whitespace? c)
      (memq (char-general-category c)
            '(Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
                 Nd Mc Me))))

(define (pad s len c)
  (if (< (string-length s) len)
      (string-append s (make-string (- len (string-length s)) c))
      s))

(define (list-files filename)
  (print "Listing files in " filename " (from the central directory) ...\n")
  (print "CRC-32    Last modified            Size           Name\n")

  (let* ((p (open-file-input-port filename))
         (records (get-central-directory p))
         (end (last records)))
    (for-each (lambda (rec)
                (let ((date (central-directory-date rec)))
                  (print
                   (string-pad (number->string (central-directory-crc-32 rec) 16)
                               9 #\0)
                   (if date (date->string date " ~1 ~2 ") " ????-??-?? ??:??:??+???? ")
                   
                   (pad (string-append (number->string (central-directory-uncompressed-size rec))
                                       "/"
                                       (number->string (central-directory-compressed-size rec)))
                        13 #\space)
                   "  " (string-filter char-printable? (central-directory-filename rec))))

                (unless (supported-compression-method?
                         (central-directory-compression-method rec))
                  (print "File uses unimplemented compression method "
                         (central-directory-compression-method rec)))
                (for-each
                 (lambda (e)
                   (print "Extra field: #x" (string-pad (number->string (car e) 16) 4 #\0)
                          " #vu8" (map (lambda (b)
                                         (string-append
                                          "#x"
                                          (string-pad
                                           (number->string b 16)
                                           2 #\0)))
                                   
                                       (bytevector->u8-list (cdr e)))))
                 (central-directory-extra rec))
                (let ((comment (central-directory-comment rec)))
                  (unless (equal? "" comment)
                    (print "Comment: " (string-filter char-printable? comment)))))
              (filter central-directory? records))

    (print "----\n")

    (let ((comment (end-of-central-directory-comment end)))
      (unless (equal? "" comment)
        (print "\n.ZIP file comment:\n"
               (string-filter char-printable? comment))))))




(parse-args (cdr (command-line)))
