;; -*- Mode: scheme; -*-
(define-module (gucu lib)
  #:use-module (srfi srfi-1)
  #:use-module (gucu curses)
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
    (if (logtest attr A_LOW) " low" "")
    (if (logtest attr A_RIGHT) " right" "")
    (if (logtest attr A_TOP)  " top" "")
    (if (logtest attr A_VERTICAL) " vertical" ""))))

(define (acs-char-name c)
  (let ((n (logior A_ALTCHARSET (char->integer c))))
    (cond 
     ((= n (acs-ulcorner)) "ULCORNER")
     ((= n (acs-llcorner)) "LLCORNER")
     ((= n (acs-urcorner)) "URCORNER")
     ((= n (acs-lrcorner)) "LRCORNER")
     ((= n (acs-ltee)) "LTEE")
     ((= n (acs-rtee)) "RTEE")
     ((= n (acs-btee)) "BTEE")
     ((= n (acs-ttee)) "TTEE")
     ((= n (acs-hline)) "HLINE")
     ((= n (acs-vline)) "VLINE")
     ((= n (acs-plus)) "PLUS")
     ((= n (acs-s1)) "S1")
     ((= n (acs-s9)) "S9")
     ((= n (acs-diamond)) "DIAMOND")
     ((= n (acs-ckboard)) "CKBOARD")
     ((= n (acs-degree)) "DEGREE")
     ((= n (acs-plminus)) "PLMINUS")
     ((= n (acs-bullet)) "BULLET")
     ((= n (acs-larrow)) "LARROW")
     ((= n (acs-rarrow)) "RARROW")
     ((= n (acs-darrow)) "DARROW")
     ((= n (acs-uarrow)) "UARROW")
     ((= n (acs-board)) "BOARD")
     ((= n (acs-s3)) "S3")
     ((= n (acs-s7)) "S7")
     ((= n (acs-lequal)) "LEQUAL")
     ((= n (acs-gequal)) "GEQUAL")
     ((= n (acs-pi)) "PI")
     ((= n (acs-nequal)) "NEQUAL")
     ((= n (acs-lantern)) "LANTERN")
     ((= n (acs-sterling)) "STERLING")
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
        (map (lambda (c)
               (format port " ~s" (acs-char-name c)))
             chars)
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

