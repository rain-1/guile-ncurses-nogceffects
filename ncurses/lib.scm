;; -*- Mode: scheme; -*-

;; lib.scm

;; Copyright 2009, 2010 Free Software Foundation, Inc.

;; This file is part of GNU Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

(define-module (ncurses lib)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13) ; in Guile 1.8.x string-trim is always available.
					; in Guile 1.6.x it isn't
  #:export (
	    make-xchar
	    xchar?
	    xchar-attr
	    xchar-color
	    xchar-chars
	    set-xchar-attr!
	    set-xchar-color!
	    set-xchar-chars!
	    xchar->list
	    list->xchar
	    xchar-equal?
	    ))

;; The xchar type -- a Guile version of the NCurses cchar_t
(define (color-name n)
  (cond
   ((= n COLOR_BLACK) "black")
   ((= n COLOR_RED) "red")
   ((= n COLOR_GREEN) "green")
   ((= n COLOR_YELLOW) "yellow")
   ((= n COLOR_BLUE) "blue")
   ((= n COLOR_MAGENTA) "magenta")
   ((= n COLOR_CYAN) "cyan")
   ((= n COLOR_WHITE) "white")
   (else
    (let* ((cc (color-content n))
	   (r (list-ref cc 0))
	   (g (list-ref cc 1))
	   (b (list-ref cc 2)))
      (format port "R~a G~a B~a" r g b)))))

(define (attr-name attr)
  (string-trim
   (string-append
    (if (logtest attr A_ALTCHARSET) " altcharset" "")
    (if (logtest attr A_BLINK) " blink" "")
    (if (logtest attr A_BOLD) " bold" "")
    (if (logtest attr A_DIM) " dim" "")
    (if (logtest attr A_INVIS) " invis" "")
    (if (logtest attr A_PROTECT) " protect" "")
    (if (logtest attr A_REVERSE) " reverse" "")
    (if (logtest attr A_STANDOUT) " standout" "")
    (if (logtest attr A_UNDERLINE) " underline" "")
    (if (logtest attr A_HORIZONTAL) " horizontal" "")
    (if (logtest attr A_LEFT) " left" "")
    (if (logtest attr A_LOW) " low") "")
    (if (logtest attr A_RIGHT) " right" "")
    (if (logtest attr A_TOP) " top" "")
    (if (logtest attr A_VERTICAL) " vertical" ""))))

(define (acs-char-name c)
  (let ((n (normal c)))
    (cond
     ((xchar-equal? n (acs-ulcorner)) "ULCORNER")
     ((xchar-equal? n (acs-llcorner))  "LLCORNER")
     ((xchar-equal? n (acs-urcorner)) "URCORNER")
     ((xchar-equal? n (acs-lrcorner)) "LRCORNER")
     ((xchar-equal? n (acs-ltee)) "LTEE")
     ((xchar-equal? n (acs-rtee)) "RTEE")
     ((xchar-equal? n (acs-btee)) "BTEE")
     ((xchar-equal? n (acs-ttee)) "TTEE")
     ((xchar-equal? n (acs-hline)) "HLINE")
     ((xchar-equal? n (acs-vline)) "VLINE")
     ((xchar-equal? n (acs-plus)) "PLUS")
     ((xchar-equal? n (acs-s1)) "S1")
     ((xchar-equal? n (acs-s9))  "S9")
     ((xchar-equal? n (acs-diamond))  "DIAMOND")
     ((xchar-equal? n (acs-ckboard))  "CKBOARD")
     ((xchar-equal? n (acs-degree))  "DEGREE")
     ((xchar-equal? n (acs-plminus))  "PLMINUS")
     ((xchar-equal? n (acs-bullet))  "BULLET")
     ((xchar-equal? n (acs-larrow))  "LARROW")
     ((xchar-equal? n (acs-rarrow))  "RARROW")
     ((xchar-equal? n (acs-darrow))  "DARROW")
     ((xchar-equal? n (acs-uarrow))  "UARROW")
     ((xchar-equal? n (acs-board))  "BOARD")
     ((xchar-equal? n (acs-s3))  "S3")
     ((xchar-equal? n (acs-s7))  "S7")
     ((xchar-equal? n (acs-lequal))  "LEQUAL")
     ((xchar-equal? n (acs-gequal))  "GEQUAL")
     ((xchar-equal? n (acs-pi))  "PI")
     ((xchar-equal? n (acs-nequal))  "NEQUAL")
     ((xchar-equal? n (acs-lantern))  "LANTERN")
     ((xchar-equal? n (acs-sterling))  "STERLING")
     (else
       "UNKNOWN SURROGATE"))))


(define (print-xchar x port)
  (let ((attrs (attr-name (xchar-attr x)))
        (color (xchar-color x))
        (chars (xchar-chars x)))
    (format port "#<xchar")
    (if (not (string-null? attrs))
        (format port " ~a" attrs))
    (if (false-if-exception (pair-content color))
        (let* ((pair (pair-content color))
               (fore (car pair))
               (back (cadr pair)))
          (format port " color-pair #~a" color)
          (format port " [~a on ~a]" (color-name fore) (color-name back))))
    (if (logtest (xchar-attr x) A_ALTCHARSET)
        (format port " ~s" (acs-char-name x))
        (map (lambda (c)
               (format port " ~s" c))
             chars))
    (format port ">")))

(define rtd-xchar (make-record-type "complex-char" '(attr color chars) print-xchar))
(define make-xchar (record-constructor rtd-xchar))
(define xchar? (record-predicate rtd-xchar))
(define xchar-attr (record-accessor rtd-xchar 'attr))
(define xchar-color (record-accessor rtd-xchar 'color))
(define xchar-chars (record-accessor rtd-xchar 'chars))
(define set-xchar-attr! (record-modifier rtd-xchar 'attr))
(define set-xchar-color! (record-modifier rtd-xchar 'color))
(define set-xchar-chars! (record-modifier rtd-xchar 'chars))

(define (xchar->list x)
  (append (list (xchar-attr x))
	  (list (xchar-color x))
	  (xchar-chars x)))

(define (list->xchar x)
  (make-xchar (first x)
	      (second x)
	      (drop x 2)))


;; Guile 1.8.x's equal? is sufficient to compare two xchars, but,
;; Guile 1.6.x's equal? always returns false when comparing two xchars
(define (xchar-equal? a b)
  (and
   (equal? (xchar-attr a) (xchar-attr b))
   (equal? (xchar-color a) (xchar-color b))
   (equal? (xchar-chars a) (xchar-chars b))))
