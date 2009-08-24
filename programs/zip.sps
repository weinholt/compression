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
        (rnrs))

(define (print . x) (for-each display x) (newline))

(define (parse-args args)
  (define (help . msg)
    (let ((x (current-error-port)))
      (when msg (display (car msg) x) (newline x) (newline x))
      (display "zip - .ZIP archiver

Usage: zip <filename.zip> <files> ...

This program creates a .ZIP file and adds the given files to it.

Author: Göran Weinholt <goran@weinholt.se>.
" x)
      (exit 1)))
  (let lp ((filename #f)
           (args args))
    (cond ((string=? (car args) "--")
           (lp filename (cdr args)))
          ((null? args)
           (help "ERROR: No filename given."))
          (else
           (call-with-port (open-file-input/output-port (car args))
             (lambda (p)
               ;; TODO: output. :)
               (create-file p (cdr args))))))))

(parse-args (cdr (command-line)))
