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

(library (weinholt compression zip extra (0 0 20090816))
  (export call-with-adorned-output-file get-file-attributes)
  (import (rnrs)
          (srfi :19 time))

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

  (define (call-with-adorned-output-file filename date local-extra
                                         central-extra
                                         os-made-by
                                         internal-attributes
                                         external-attributes
                                         proc)
    (call-with-port (open-file-output-port filename)
      proc))

  ;; This procedure will be used when creating .ZIP files. The data
  ;; types are the same as for the previous procedure.
  (define (get-file-attributes filename)
    (values (current-date)              ;date
            '()                         ;local-extra
            '()                         ;central-extra
            0                           ;os-made-by
            0                           ;internal-attributes
            0)))                        ;external-attributes
