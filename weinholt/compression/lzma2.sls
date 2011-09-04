;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011 Göran Weinholt <goran@weinholt.se>
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

;; Decoder for LZMA2, a layer above LZMA.

#|
A chunk can be at most this large when uncompressed:
(let ((usize* #xFFFF) (ctrl #xFF))
  (fx+ (fxior usize* (fxarithmetic-shift-left (fxbit-field ctrl 0 5) 16))
       1)) => 2097152
|#

(library (weinholt compression lzma2 (1 0 20110904))
  (export lzma2-decode-chunk)
  (import (rnrs)
          (weinholt compression lzma)
          (weinholt compression sliding-buffer)
          (weinholt struct pack))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  ;; Decodes one LZMA2 chunk. Returns a new state. If there's no more
  ;; data in the LZMA2 block then the end-of-file object is returnred.
  (define (lzma2-decode-chunk p sink dictionary-size state)
    (define who 'lzma2-decode-chunk)
    (define fxasl fxarithmetic-shift-left)
    (define (decode-props b)
      (let* ((pb (fxdiv b (* 9 5)))
             (prop (fx- b (fx* pb (* 9 5))))
             (lp (fxdiv prop 9))
             (lc (fx- prop (fx* lp 9))))
        (trace "LZMA2: Literal context bits (lc): " lc ;[0,8]
               ", Literal position bits (lp): " lp     ;[0,4]
               ", Position bits (pb): " pb)            ;[0,4]
        (values lc lp pb)))
    (define (get-props p)
      (trace "LZMA2: reading new properties")
      (let ((b (get-u8 p)))
        (when (fx>? b (+ (* (+ (* 4 5) 4)
                            9)
                         8))
          (error who "Bad properties for LZMA2 chunk" p))
        b))
    (define (fresh-dictionary)
      (trace "LZMA2: dictionary reset")
      (make-sliding-buffer sink dictionary-size))
    (define (return-state dictionary props lzma-state position)
      (vector dictionary props lzma-state position))
    (define (empty-state)
      (vector #f #f #f 0))
    (let ((state (or state (empty-state))))
      (let ((dictionary (vector-ref state 0))
            (props (vector-ref state 1))
            (lzma-state (vector-ref state 2))
            (position (vector-ref state 3)))
        (let ((ctrl (get-u8 p)))
          (trace "LZMA2 control: #x" (number->string ctrl 16))
          (case ctrl
            ((#x00) (eof-object))       ;end of block
            ((#x01 #x02)                ;uncompressed chunk
             (let ((dictionary (if (= ctrl #x01)
                                   (fresh-dictionary)
                                   dictionary)))
               (let ((csize (fx+ (get-unpack p "!S") 1)))
                 (trace "Uncompressed chunk: " csize)
                 (sliding-buffer-read! dictionary p csize)
                 (sliding-buffer-drain! dictionary)
                 (return-state dictionary props lzma-state (+ position csize)))))
            (else
             (let-values (((usize* csize*) (get-unpack p "!SS")))
               (let ((usize (fx+ (fxior usize* (fxasl (fxbit-field ctrl 0 5) 16))
                                 1))
                     (csize (fx+ csize* 1))
                     (cmd (fxand ctrl #xE0)))
                 (trace "Uncompressed size: " usize " Compressed size: " csize)
                 ;; The control codes are instructions to reset the
                 ;; dictionary, to read new properties, or to reset
                 ;; the decoder state.
                 (case cmd
                   ((#x80 #xA0 #xC0 #xE0)
                    (let ((dictionary (if (memv cmd '(#xE0))
                                          (fresh-dictionary)
                                          dictionary))
                          (props (if (memv cmd '(#xC0 #xE0))
                                     (get-props p)
                                     props)))
                      (let*-values (((lc lp pb) (decode-props props)))
                        (let ((lzma-state*
                               (cond ((and lzma-state
                                           (not (memv cmd '(#xA0 #xC0 #xE0))))
                                      (trace "LZMA: reuse old decoder state")
                                      (lzma-state dictionary usize lc lp pb))
                                     (else
                                      (trace "LZMA2: reset decoder state")
                                      (lzma-decode-chunk p dictionary usize
                                                         lc lp pb position)))))
                          (trace "LZMA2: chunk decoded")
                          (sliding-buffer-drain! dictionary)
                          (return-state dictionary props lzma-state*
                                        (+ position usize))))))
                   (else
                    (error who "Invalid control code" ctrl))))))))))))
