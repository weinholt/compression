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

;; RFC1950: ZLIB Compressed Data Format Specification version 3.3

;; On flushing:
;; http://www.bolet.org/~pornin/deflate-flush.html

;; Please understand that the zlib input port is blocking. If the
;; sender does not flush properly, you might be stuck waiting for more
;; data, even though there is data in the buffers.

(library (weinholt compression zlib (0 0 20100427))
  (export make-zlib-input-port)
  (import (rnrs)
          (weinholt compression adler-32 (0 (>= 0)))
          (weinholt compression inflate (0 (>= 0)))
          (weinholt struct pack (1 (>= 3))))

  (define (flg-fdict? x) (fxbit-set? x 5))
  (define (cmf-cm x) (fxbit-field x 0 4))
  (define (cmf-cinfo x) (fxbit-field x 4 8))

  (define compression-method-deflate 8)

  (define (get-zlib-header p)
    (define who 'get-zlib-header)
    (let*-values (((cmf flg) (get-unpack p "CC"))
                  ((dictid) (and (flg-fdict? flg) (get-unpack p "!L"))))
      (unless (fxzero? (fxmod (fxior (fxarithmetic-shift-left cmf 8) flg) 31))
        (error who "bad ZLIB header"))
      (unless (= (cmf-cm cmf) compression-method-deflate)
        (error who "bad compression algorithm" cmf))
      (when (> (cmf-cinfo cmf) 7)
        (error who "reserved window size" cmf))
      (let ((window-size (fxarithmetic-shift-left 1 (+ 8 (cmf-cinfo cmf)))))
        (values window-size dictid))))

  ;; Makes a binary input port that reads compressed data from the
  ;; given binary input port. If max-buffer-size is #f, then the
  ;; buffer can grow forever (might be a bad idea). Protocols using
  ;; ZLIB will normally specify a "flush" behavior. If your protocol
  ;; uses flushing and specifies a maximum record size, then use that
  ;; size as max-buffer-size. See the URL earlier about flushing. The
  ;; `dictionaries' argument is an alist of Adler-32 checksums and
  ;; dictionaries (bytevectors).
  (define (make-zlib-input-port in id max-buffer-size close-underlying-port? dictionaries)
    (let-values (((window-size dictid) (get-zlib-header in)))
      (let ((done #f)                   ;no more data?
            (buffer (make-bytevector (or max-buffer-size window-size)))
            (offsetr 0)
            (offsetw 0)
            (checksum (adler-32-init)))
        (define (sink bv start count)
          ;; Only called when there's no data in the buffer.
          (set! checksum (adler-32-update checksum bv start (+ start count)))
          (when (and max-buffer-size (> (+ offsetw count) max-buffer-size))
            (error 'zlib-sink "a chunk exceeded the maximum buffer size"
                   (+ offsetw count)))
          (let lp ()
            ;; If no maximum buffer size is given, the sender can
            ;; cause a lot of memory to be allocated while sending
            ;; very little data himself.
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
          (make-inflater in sink window-size
                         (cond ((and dictid (assv dictid dictionaries))
                                => cdr)
                               (dictid
                                (error 'make-zlib-input-port
                                       "the ZLIB stream uses an unknown dictionary"
                                       dictid))
                               (else #f))))
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
                 (if done
                     0
                     (let lp ()
                       (case (inflater)
                         ((more)        ;more deflate blocks available
                          (if (zero? offsetw)
                              (lp)  ;encountered a sync block
                              (return)))
                         ((done)        ;end of deflate data
                          (set! done #t)
                          (return))))))
                (else (return))))
        (define (close)
          (set! buffer #f)
          ;; FIXME: currently broken because the bit-reader eats the
          ;; checksum sometimes.
          #;
          (when (and done
                     (not (eqv? (get-unpack in "!L")
                                (adler-32-finish checksum))))
            (error 'close-port "bad ZLIB checksum"))
          (when close-underlying-port? (close-port in))
          (set! in #f)
          (set! inflater #f))
        (make-custom-binary-input-port id read! #f #f close)))))
