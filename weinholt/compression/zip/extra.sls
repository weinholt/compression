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

;; Template for implementation hooks for (weinholt compression zip).

;; The R6RS does not specify many file system operations and does not
;; specify how filenames are handled. It does not have operations for
;; creating directories or looking up file attributes. So you can
;; override this library and translate between zip's idea of file
;; names and attributes, and your operating system's idea of the same.

;; This default library only does as much as is possible with R6RS.

(library (weinholt compression zip extra (0 0 20090824))
  (export call-with-adorned-output-file get-file-attributes)
  (import (rnrs)
          (only (srfi :13 strings) string-suffix? string-trim)
          (srfi :19 time))

  (define os-dos 0)
  (define os-openvms 2)
  (define os-unix 3)
  ;; etc etc

  ;; This procedure functions like call-with-output-file, except it
  ;; can optionally set the file's timestamps and other attributes.
  ;; None of this can be done portably (especially the OS-dependent
  ;; stuff) so, if you like, you can make an extra.IMPL.sls override
  ;; which uses the functions in your Scheme to set any attributes you
  ;; like. The date is an SRFI-19 date object and extra-arguments are
  ;; alists with (id . bytevector) entries. The bytevector does not
  ;; include the tag. See the .ZIP file format specification for more
  ;; information on the contents of these lists:

  ;; http://www.info-zip.org/doc/

  ;; The port should be in binary mode.

  ;; call-with-adorned-output-file will be called for all file records
  ;; in a .zip file, even directories. Only call `proc' for files!
  ;; Return zero if you don't call proc, because zero is the CRC-32 of
  ;; directories etc.

  ;; It's your responsibility to create any directories that are
  ;; needed to create the file.

  ;; It's also your responsibility to make sure that you only create
  ;; files under the current directory. The inzip-filename is
  ;; supposed to be relative, but it *could* be an absolute filename,
  ;; and someone might create such a zip file to make you create a
  ;; file in some unexpected place. The caller checks that filenames
  ;; don't begin with a #\/, but checking for things like C:\ and
  ;; SYS$LOGIN:LOGIN.COM is your responsibility, because only you know
  ;; your operating system's filename specification.
  
  (define (call-with-adorned-output-file inzip-filename date local-extra
                                         central-extra
                                         os-made-by
                                         internal-attributes
                                         external-attributes
                                         uncompressed-size
                                         proc)
    (cond ((and (string-suffix? "/" inzip-filename) (zero? uncompressed-size))
           ;; Directory. Optimally you would create the directory
           ;; here, and not print anything.
           (display "directories not implemented. ")
           -1)
          ((and (not date) (= os-dos os-made-by) (zero? uncompressed-size))
           ;; Volume label. Should be ignored, methinks.
           0)
          (else
           (call-with-port (open-file-input/output-port inzip-filename)
             proc))))

  ;; This procedure will be used when creating .ZIP files. The data
  ;; types are the same as for the previous procedure, except the
  ;; filename is from the implementation's perspective. The *returned*
  ;; filename should be suitable for inclusion in the .zip file. This
  ;; means that the path separator becomes #\/ and directories have a
  ;; #\/ appended.
  (define (get-file-attributes implementation-filename)
    (values
      ;; Remove leading /
      (string-trim implementation-filename #\/) ;filename in .zip file
      (current-date)                    ;date
      '()                               ;local-extra
      '()                               ;central-extra
      0                                 ;os-made-by
      0                                 ;internal-attributes
      0))                               ;external-attributes

  )
