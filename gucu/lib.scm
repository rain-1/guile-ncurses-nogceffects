;; -*- Mode: scheme; -*-
(define-module (gucu lib)
  #:use-module (srfi srfi-1)
  #:use-module (gucu curses)
  #:use-module (srfi srfi-13)  ; in Guile 1.8.x string-trim is always available.
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
   ((= n COLOR_BLACK) (gettext "black"))
   ((= n COLOR_RED) (gettext "red"))
   ((= n COLOR_GREEN) (gettext "green"))
   ((= n COLOR_YELLOW) (gettext "yellow"))
   ((= n COLOR_BLUE) (gettext "blue"))
   ((= n COLOR_MAGENTA) (gettext "magenta"))
   ((= n COLOR_CYAN) (gettext "cyan"))
   ((= n COLOR_WHITE) (gettext "white"))
   (else
    (let* ((cc (color-content n))
           (r (list-ref cc 0))
           (g (list-ref cc 1))
           (b (list-ref cc 2)))
      (format port (gettext "R~a G~a B~a") r g b)))))

(define (attr-name attr)
  (string-trim
   (string-append
    (if (logtest attr A_ALTCHARSET) (gettext " altcharset") "")
    (if (logtest attr A_BLINK) (gettext " blink") "")
    (if (logtest attr A_BOLD) (gettext " bold") "")
    (if (logtest attr A_DIM) (gettext " dim") "")
    (if (logtest attr A_INVIS) (gettext " invis") "")
    (if (logtest attr A_PROTECT) (gettext " protect") "")
    (if (logtest attr A_REVERSE) (gettext " reverse") "")
    (if (logtest attr A_STANDOUT) (gettext " standout") "")
    (if (logtest attr A_UNDERLINE) (gettext " underline") "")
    (if (logtest attr A_HORIZONTAL) (gettext " horizontal") "")
    (if (logtest attr A_LEFT) (gettext " left") "")
    (if (logtest attr A_LOW) (gettext " low") "")
    (if (logtest attr A_RIGHT) (gettext " right") "")
    (if (logtest attr A_TOP)  (gettext " top") "")
    (if (logtest attr A_VERTICAL) (gettext " vertical") ""))))

(define (acs-char-name c)
  (let ((n (normal c)))
    (cond
     ((xchar-equal? n (acs-ulcorner)) (gettext "ULCORNER"))
     ((xchar-equal? n (acs-llcorner)) (gettext "LLCORNER"))
     ((xchar-equal? n (acs-urcorner)) (gettext "URCORNER"))
     ((xchar-equal? n (acs-lrcorner)) (gettext "LRCORNER"))
     ((xchar-equal? n (acs-ltee)) (gettext "LTEE"))
     ((xchar-equal? n (acs-rtee)) (gettext "RTEE"))
     ((xchar-equal? n (acs-btee)) (gettext "BTEE"))
     ((xchar-equal? n (acs-ttee)) (gettext "TTEE"))
     ((xchar-equal? n (acs-hline)) (gettext "HLINE"))
     ((xchar-equal? n (acs-vline)) (gettext "VLINE"))
     ((xchar-equal? n (acs-plus)) (gettext "PLUS"))
     ((xchar-equal? n (acs-s1)) (gettext "S1"))
     ((xchar-equal? n (acs-s9)) (gettext "S9"))
     ((xchar-equal? n (acs-diamond)) (gettext "DIAMOND"))
     ((xchar-equal? n (acs-ckboard)) (gettext "CKBOARD"))
     ((xchar-equal? n (acs-degree)) (gettext "DEGREE"))
     ((xchar-equal? n (acs-plminus)) (gettext "PLMINUS"))
     ((xchar-equal? n (acs-bullet)) (gettext "BULLET"))
     ((xchar-equal? n (acs-larrow)) (gettext "LARROW"))
     ((xchar-equal? n (acs-rarrow)) (gettext "RARROW"))
     ((xchar-equal? n (acs-darrow)) (gettext "DARROW"))
     ((xchar-equal? n (acs-uarrow)) (gettext "UARROW"))
     ((xchar-equal? n (acs-board)) (gettext "BOARD"))
     ((xchar-equal? n (acs-s3)) (gettext "S3"))
     ((xchar-equal? n (acs-s7)) (gettext "S7"))
     ((xchar-equal? n (acs-lequal)) (gettext "LEQUAL"))
     ((xchar-equal? n (acs-gequal)) (gettext "GEQUAL"))
     ((xchar-equal? n (acs-pi)) (gettext "PI"))
     ((xchar-equal? n (acs-nequal)) (gettext "NEQUAL"))
     ((xchar-equal? n (acs-lantern)) (gettext "LANTERN"))
     ((xchar-equal? n (acs-sterling)) (gettext "STERLING"))
     (else
      (gettext "UNKNOWN SURROGATE")))))


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
