;; -*- Mode: scheme; -*-

;; curses.scm

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

(define-module (ncurses curses)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (
	    %filter
	    %wide-ncurses
	    %ucs4-chars
	    %scheme-char-to-c-char
	    %scheme-char-from-c-char
	    %scheme-char-to-c-wchar
	    %scheme-char-from-c-wchar
	    %xchar-from-chtype
	    %xchar-to-chtype
	    addch
	    addchstr
	    addstr
	    assume-default-colors
	    attr->list
	    attr-get
	    attr-off!
	    attr-on!
	    attr-set!
	    baudrate
	    beep
	    bkgd
	    bkgdset!
	    border
	    box
	    can-change-color?
	    cbreak!
	    chgat
	    clear
	    clearok!
	    clrtobot
	    clrtoeol
	    color-pair
	    color-set!
	    copywin
	    curs-set
	    curses-version
	    def-prog-mode
	    def-shell-mode
	    define-key
	    delay-output
	    delch
	    deleteln
	    delscreen
	    derwin
	    doupdate
	    dupwin
	    echo!
	    echochar
	    endwin
	    erase
	    erasechar
	    flash
	    flushinp
	    getbkgd
	    getch
	    getmaxy
	    getmaxyx
	    getnstr
	    getwin
	    halfdelay!
	    has-colors?
	    has-ic?
	    has-il?
	    has-key?
	    hline
	    idcok!
	    idlok!
	    immedok!
	    inch
	    inchstr
	    init-color!
	    init-pair!
	    initscr
	    insch
	    insdelln
	    insertln
	    insstr
	    instr
	    intrflush!
	    is-linetouched?
	    is-wintouched?
	    isendwin?
	    key-f
	    keyname
	    keypad!
	    killchar
	    leaveok!
	    longname
	    meta!
	    mouseinterval
	    move
	    mvcur
	    mvderwin
	    mvwin
	    napms
	    newpad
	    newwin
	    nl!
	    nocbreak!
	    nodelay!
	    noecho!
	    nonl!
	    nooutrefresh
	    noqiflush!
	    noraw!
	    notimeout!
	    noutrefresh
	    overlay
	    overwrite
	    pair-number
	    pechochar
	    pnoutrefresh
	    prefresh
	    putwin
	    qiflush!
	    raw!
	    redrawln
	    redrawwin
	    refresh
	    reset-prog-mode
	    reset-shell-mode
	    resetty
	    savetty
	    scr-dump
	    scr-init
	    scr-restore
	    scr-set
	    scrl
	    scroll
	    scrollok!
	    set-term
	    setscrreg!
	    setsyx
	    standend!
	    standout!
	    start-color!
	    subpad
	    subwin
	    syncok!
	    term-attrs
	    termname
	    timeout!
	    touchline
	    touchwin
	    typeahead!
	    ungetch
	    untouchline
	    untouchwin
	    use-default-colors
	    use-env
	    vline
	    wcursyncup
	    wenclose?
	    wsyncdown
	    wsyncup
            %wmove

	    EOF
	    ERR
	    FALSE
	    OK
	    TRUE
	    A_ALTCHARSET
	    A_ATTRIBUTES
	    A_BLINK
	    A_BOLD
	    A_CHARTEXT
	    A_COLOR
	    A_DIM
	    A_INVIS
	    A_NORMAL
	    A_PROTECT
	    A_REVERSE
	    A_STANDOUT
	    A_UNDERLINE
            A_HORIZONTAL
            A_LEFT
            A_LOW
            A_RIGHT
            A_TOP
            A_VERTICAL
	    COLOR_BLACK
	    COLOR_GREEN
	    COLOR_RED
	    COLOR_YELLOW
	    COLOR_BLUE
	    COLOR_MAGENTA
	    COLOR_CYAN
	    COLOR_WHITE
	    KEY_CODE_YES
	    KEY_MIN
	    KEY_BREAK
	    KEY_SRESET
	    KEY_RESET
	    KEY_DOWN
	    KEY_UP
	    KEY_LEFT
	    KEY_RIGHT
	    KEY_HOME
	    KEY_BACKSPACE
	    KEY_F0
	    KEY_DL
	    KEY_IL
	    KEY_DC
	    KEY_IC
	    KEY_EIC
	    KEY_CLEAR
	    KEY_EOS
	    KEY_EOL
	    KEY_SF
	    KEY_SR
	    KEY_NPAGE
	    KEY_PPAGE
	    KEY_STAB
	    KEY_CTAB
	    KEY_CATAB
	    KEY_ENTER
	    KEY_PRINT
	    KEY_LL
	    KEY_A1
	    KEY_A3
	    KEY_B2
	    KEY_C1
	    KEY_C3
	    KEY_BTAB
	    KEY_BEG
	    KEY_CANCEL
	    KEY_CLOSE
	    KEY_COMMAND
	    KEY_COPY
	    KEY_CREATE
	    KEY_END
	    KEY_EXIT
	    KEY_FIND
	    KEY_HELP
	    KEY_MARK
	    KEY_MESSAGE
	    KEY_MOVE
	    KEY_NEXT
	    KEY_OPEN
	    KEY_OPTIONS
	    KEY_PREVIOUS
	    KEY_REDO
	    KEY_REFERENCE
	    KEY_REFRESH
	    KEY_REPLACE
	    KEY_RESTART
	    KEY_RESUME
	    KEY_SAVE
	    KEY_SBEG
	    KEY_SCANCEL
	    KEY_SCOMMAND
	    KEY_SCOPY
	    KEY_SCREATE
	    KEY_SDC
	    KEY_SDL
	    KEY_SELECT
	    KEY_SEND
	    KEY_SEOL
	    KEY_SEXIT
	    KEY_SFIND
	    KEY_SHELP
	    KEY_SHOME
	    KEY_SIC
	    KEY_SLEFT
	    KEY_SMESSAGE
	    KEY_SMOVE
	    KEY_SNEXT
	    KEY_SOPTIONS
	    KEY_SPREVIOUS
	    KEY_SPRINT
	    KEY_SREDO
	    KEY_SREPLACE
	    KEY_SRIGHT
	    KEY_SRSUME
	    KEY_SSAVE
	    KEY_SSUSPEND
	    KEY_SUNDO
	    KEY_SUSPEND
	    KEY_UNDO
	    KEY_MOUSE
	    KEY_RESIZE
	    BUTTON1_PRESSED
	    BUTTON1_RELEASED
	    BUTTON1_CLICKED
	    BUTTON1_DOUBLE_CLICKED
	    BUTTON1_TRIPLE_CLICKED
	    BUTTON2_PRESSED
	    BUTTON2_RELEASED
	    BUTTON2_CLICKED
	    BUTTON2_DOUBLE_CLICKED
	    BUTTON2_TRIPLE_CLICKED
	    BUTTON3_PRESSED
	    BUTTON3_RELEASED
	    BUTTON3_CLICKED
	    BUTTON3_DOUBLE_CLICKED
	    BUTTON3_TRIPLE_CLICKED
	    BUTTON4_PRESSED
	    BUTTON4_RELEASED
	    BUTTON4_CLICKED
	    BUTTON4_DOUBLE_CLICKED
	    BUTTON4_TRIPLE_CLICKED
	    BUTTON_SHIFT
	    BUTTON_CTRL
	    BUTTON_ALT
	    ALL_MOUSE_EVENTS
	    REPORT_MOUSE_POSITION
	    color-content
	    delwin
	    getmouse
	    mousemask
	    mouse-trafo
	    pair-content
	    ungetmouse
	    getbegyx
	    getparyx
	    getsyx
	    getyx
	    acs-block
	    acs-board
	    acs-btee
	    acs-bullet
	    acs-ckboard
	    acs-darrow
	    acs-degree
	    acs-diamond
	    acs-gequal
	    acs-hline
	    acs-lantern
	    acs-larrow
	    acs-lequal
	    acs-llcorner
	    acs-lrcorner
	    acs-ltee
	    acs-nequal
	    acs-pi
	    acs-plminus
	    acs-plus
	    acs-rarrow
	    acs-rtee
	    acs-s1
	    acs-s3
	    acs-s7
	    acs-s9
	    acs-sterling
	    acs-ttee
	    acs-uarrow
	    acs-ulcorner
	    acs-urcorner
	    acs-vline
	    lines
	    cols
	    colors
	    color-pairs
	    tabsize
	    set-tabsize!
	    stdscr
	    curscr
	    window?
	    screen?
	    open-curses-port

	    ;; error codes
	    &curses-error
	    &curses-wrong-type-arg-error
	    &curses-out-of-range-error
	    &curses-bad-state-error
	    curses-error?
	    curses-wrong-type-arg-error?
	    curses-out-of-range-error?
	    curses-bad-state-error?

	    ;; xchar type library
	    xchar-attr
	    xchar-color
	    xchar-equal?

            ;; xchar property modifiers
            blink
            blink-off
            blink-on
            bold
            bold-off
            bold-on
            color
            dim
            dim-off
            dim-on
            horizontal
            horizontal-off
            horizontal-on
            invis
            invis-off
            invis-on
            left
            left-off
            left-on
            low
            low-off
            low-on
            normal
            normal-off
            normal-on
            protect
            protect-off
            protect-on
            inverse
            inverse-off
            inverse-on
            right
            right-off
            right-on
	    standout
	    standout-off
	    standout-on
            top
            top-off
            top-on
	    underline
	    underline-on
	    underlline-off
            vertical
            vertical-off
            vertical-on
            ))

;;; Exceptions

(define-condition-type &curses-error &error
  curses-error?)

(define-condition-type &curses-wrong-type-arg-error &curses-error
  curses-wrong-type-arg-error?
  (arg           curses-wrong-type-arg-error:arg)
  (expected-type curses-wrong-type-arg-error:expected-type))

(define-condition-type &curses-out-of-range-error &curses-error
  curses-out-of-range-error?
  (arg           curses-wrong-type-arg-error:arg))

;; Usually this indicates and attempt to use an already freed object
(define-condition-type &curses-bad-state-error &curses-error
  curses-bad-state-error?)

;;; The xchar type library

;; The xchar type -- a Guile version of the NCurses cchar_t
(define (color-name n)
  "Returns a string describing the color number N."
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
  "Given an attribute stored as a integer, return a descriptive
string."
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
    (if (logtest attr A_VERTICAL) " vertical" "")))

(define (acs-char-name c)
  "If C is a member of the alternate character set, this returns the
name of the character as as string."
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
  "A printer for complex chars."
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
  "Converts a complex charater X to a list of properties"
  (append (list (xchar-attr x))
	  (list (xchar-color x))
	  (xchar-chars x)))

(define (list->xchar x)
  "Converts a list of properties into a complex character"
  (make-xchar (first x)
	      (second x)
	      (drop x 2)))


;; Guile 1.8.x's equal? is sufficient to compare two xchars, but,
;; Guile 1.6.x's equal? always returns false when comparing two xchars
(define (xchar-equal? a b)
  "A comparison predicate for complex characters"
  (and
   (equal? (xchar-attr a) (xchar-attr b))
   (equal? (xchar-color a) (xchar-color b))
   (equal? (xchar-chars a) (xchar-chars b))))


;;; Rendition functions

(defmacro a-attribute (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar ,A_ATTRIBUTE
                 0
                 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar (logior ,A_ATTRIBUTE A_ALTCHARSET)
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (if (logtest (xchar-attr ,x) A_ALTCHARSET)
                     (logior ,A_ATTRIBUTE A_ALTCHARSET)
                     ,A_ATTRIBUTE)
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar ,A_ATTRIBUTE 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (if (logtest (xchar-attr c) A_ALTCHARSET)
                            (logior ,A_ATTRIBUTE A_ALTCHARSET)
                            ,A_ATTRIBUTE)
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(defmacro a-attribute-on (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar ,A_ATTRIBUTE 0 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar (logior ,A_ATTRIBUTE A_ALTCHARSET)
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (logior ,A_ATTRIBUTE (xchar-attr x))
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar ,A_ATTRIBUTE 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (logior ,A_ATTRIBUTE (xchar-attr c))
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(defmacro a-attribute-off (x A_ATTRIBUTE)
  `(cond
    ((char? ,x)
     (make-xchar A_NORMAL 0 (list ,x)))
    ((and (integer? ,x) (logtest ,x A_ALTCHARSET))
     (make-xchar A_ALTCHARSET
                 0
                 (list (integer->char (logand x #xff)))))
    ((xchar? ,x)
     (make-xchar (logand (lognot ,A_ATTRIBUTE) (xchar-attr x))
                 (xchar-color ,x)
                 (xchar-chars ,x)))
    ((string? ,x)
     (map (lambda (c)
            (make-xchar A_NORMAL 0 (list c)))
          (string->list ,x)))
    ((and (list? ,x) (every xchar? ,x))
     (map (lambda (c)
            (make-xchar (logand (lognot ,A_ATTRIBUTE) (xchar-attr c))
                        (xchar-color c)
                        (xchar-chars c)))
          ,x))
    (else
     (error "Invalid input ~s" ,x))))

(define (blink x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute BLINK.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_BLINK))

(define (blink-off x) 
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BLINK attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_BLINK))

(define (blink-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BLINK attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_BLINK))

(define (bold x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute BOLD.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_BOLD))

(define (bold-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BOLD attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_BOLD))

(define (bold-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the BOLD attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_BOLD))

(define (dim x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute DIM.  If the input
X was a rendered character or a rendered string, the old attributes
are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_DIM))

(define (dim-off x) 
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the DIM attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_DIM))

(define (dim-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the DIM attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_DIM))

(define (horizontal x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute HORIZONTAL.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_HORIZONTAL))

(define (horizontal-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the HORIZONTAL attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_HORIZONTAL))

(define (horizontal-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the HORIZONTAL attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_HORIZONTAL))

(define (invis x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute INVIS.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_INVIS))

(define (invis-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVIS attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_INVIS))

(define (invis-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVIS attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_INVIS))

(define (left x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute LEFT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_LEFT))

(define (left-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_LEFT))

(define (left-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_LEFT))

(define (low x) 
    "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute LOW.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_LOW))

(define (low-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LOW attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_LOW))

(define (low-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the LEFT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_LOW))

(define (normal x) 
    "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the normal, default, rendering.
If the input X was a rendered character or a rendered string, the old
attributes are cleared, but the color pair, if any, is not modified."
  (a-attribute x A_NORMAL))

(define (normal-off x) (a-attribute-off x A_NORMAL))
(define (normal-on x) (a-attribute-on x A_NORMAL))

(define (protect x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute PROTECT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_PROTECT))

(define (protect-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the PROTECT attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_PROTECT))

(define (protect-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the PROTECT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_PROTECT))

(define (inverse x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the INVERSE (aka REVERSE)
attribute.  If the input X was a rendered character or a rendered
string, the old attributes are replaced, but the color pair, if any,
is not modified."
  (a-attribute x A_REVERSE))

(define (inverse-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVERSE (aka REVERSE) attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_REVERSE))

(define (inverse-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the INVERSE (aka REVERSE) attribute enabled,
but, other attributes are not modified."
  (a-attribute-on x A_REVERSE))

(define (right x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute RIGHT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_RIGHT))

(define (right-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the RIGHT attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_RIGHT))

(define (right-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the RIGHT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_RIGHT))

(define (standout x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute STANDOUT.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_STANDOUT))

(define (standout-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the STANDOUT attribute disabled,
but, other attributes are not modified."
  (a-attribute-off x A_STANDOUT))

(define (standout-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the STANDOUT attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_STANDOUT))

(define (top x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute TOP.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_TOP))

(define (top-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the TOP attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_TOP))

(define (top-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the TOP attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_TOP))

(define (underline x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute UNDERLINE.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_UNDERLINE))

(define (underline-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the UNDERLINE attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_UNDERLINE))

(define (underline-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the UNDERLINE attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_UNDERLINE))

(define (vertical x) 
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the attribute VERTICAL.  If the
input X was a rendered character or a rendered string, the old
attributes are replaced, but the color pair, if any, is not modified."
  (a-attribute x A_VERTICAL))

(define (vertical-off x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the VERTICAL attribute disabled, but, other
attributes are not modified."
  (a-attribute-off x A_VERTICAL))

(define (vertical-on x)
  "If the input X is a simple character or string, it returns a
complex character or string has the normal rendering. If the input X
is a rendered character or a rendered string, it returns a rendered
character or string with the VERTICAL attribute enabled, but, other
attributes are not modified."
  (a-attribute-on x A_VERTICAL))

(define (color n x)
  "These procedure takes X, which can be either a simple character,
a complex character, a simple string, or a complex string, and returns
a rendered character or string with the color pair N.  If the input X
was a rendered character or a rendered string, the old attributes are
not modified, but the color pair, if any, is modified."
  (cond
   ((char? x)
    (make-xchar A_NORMAL
                n
                (list x)))
    ((and (integer? x) (logtest x A_ALTCHARSET))
     (make-xchar A_ALTCHARSET
                 n
                 (list (integer->char (logand x #xff)))))
    ((xchar? x)
     (make-xchar (xchar-attr x)
		 n
		 (xchar-chars x)))
    ((string? x)
     (map (lambda (c)
            (make-xchar A_NORMAL n (list c)))
          (string->list x)))
    ((and (list? x) (every xchar? x))
     (map (lambda (c)
            (make-xchar (xchar-attr c)
			n
			(xchar-chars c)))
          x))
    (else
     (error "Invalid input ~s" x))))


(define (acs-block)    (list->xchar (%acs-block)))
(define (acs-board)    (list->xchar (%acs-board)))
(define (acs-btee)     (list->xchar (%acs-btee)))
(define (acs-bullet)   (list->xchar (%acs-bullet)))
(define (acs-ckboard)  (list->xchar (%acs-ckboard)))
(define (acs-darrow)   (list->xchar (%acs-darrow)))
(define (acs-degree)   (list->xchar (%acs-degree)))
(define (acs-diamond)  (list->xchar (%acs-diamond)))
(define (acs-gequal)   (list->xchar (%acs-gequal)))
(define (acs-hline)    (list->xchar (%acs-hline)))
(define (acs-lantern)  (list->xchar (%acs-lantern)))
(define (acs-larrow)   (list->xchar (%acs-larrow)))
(define (acs-lequal)   (list->xchar (%acs-lequal)))
(define (acs-llcorner) (list->xchar (%acs-llcorner)))
(define (acs-lrcorner) (list->xchar (%acs-lrcorner)))
(define (acs-ltee)     (list->xchar (%acs-ltee)))
(define (acs-nequal)   (list->xchar (%acs-nequal)))
(define (acs-pi)       (list->xchar (%acs-pi)))
(define (acs-plminus)  (list->xchar (%acs-plminus)))
(define (acs-plus)     (list->xchar (%acs-plus)))
(define (acs-rarrow)   (list->xchar (%acs-rarrow)))
(define (acs-rtee)     (list->xchar (%acs-rtee)))
(define (acs-s1)       (list->xchar (%acs-s1)))
(define (acs-s3)       (list->xchar (%acs-s3)))
(define (acs-s7)       (list->xchar (%acs-s7)))
(define (acs-s9)       (list->xchar (%acs-s9)))
(define (acs-sterling) (list->xchar (%acs-sterling)))
(define (acs-ttee)     (list->xchar (%acs-ttee)))
(define (acs-uarrow)   (list->xchar (%acs-uarrow)))
(define (acs-ulcorner) (list->xchar (%acs-ulcorner)))
(define (acs-urcorner) (list->xchar (%acs-urcorner)))
(define (acs-vline)    (list->xchar (%acs-vline)))


;; Scheme calling wrappers for C functions

(define* (addch win ch #:key y x)
  "Puts the character CH into the given window at its current window
position.  If Y and X are set, moves to (X,Y) first.  Returns #t on
success."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg ch)
			 (expected-type 'xchar)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and (if (and y x)
	   (%wmove win y x)
           #t)
       (%waddch win (xchar->list ch))))
  
(define* (addchstr win str #:key y x (n -1))
  "Adds the list of complex characters STR to the window WIN at and
after the current cursor position.  If X and Y are set, the cursor
will be moved to that position first.  If N is set, and maximum of N
complex characters will be added.  Returns #t on success or #f on
failure."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (or (not (list? str)) (not (every xchar? str)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg str)
			 (expected-type 'xstring)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (and (if (and y x)
	   (%wmove win y x)
	   #t)
       (%waddchnstr win (map xchar->list str) n)))

(define* (addstr win str #:key y x (n -1))
  "Adds the string STR to the window WIN at and after the current
cursor position.  If X and Y are set, the cursor will be moved to that
position first.  If N is set, and maximum of N characters will be
added.  Returns #t on success or #f on failure."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (string? str))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg str)
			 (expected-type 'string)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddnstr win str n)))

(define (attr-get win)
  "Returns the rendtion of the current window as a two-element list.
The first element is the integer representation of the attributes, and
the second element is the color pair number."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (let* ((ac (%wattr-get win))
         (attr (car ac))
         (color (cadr ac)))
    (list
     (logand attr (logior A_STANDOUT
                          A_UNDERLINE
                          A_REVERSE
                          A_BLINK
                          A_DIM
                          A_BOLD
                          A_ALTCHARSET
                          A_INVIS
                          A_PROTECT
                          A_HORIZONTAL
                          A_LEFT
                          A_LOW
                          A_RIGHT
                          A_TOP
                          A_VERTICAL))
     color)))

(define (attr->list attr)
  "Unpacks the integer representation of an attribute into a list of
attributes."
  (append
   (if (logtest attr A_ALTCHARSET) '(altcharset) '())
   (if (logtest attr A_BLINK) '(blink) '())
   (if (logtest attr A_BOLD) '(bold) '())
   (if (logtest attr A_DIM) '(dim) '())
   (if (logtest attr A_INVIS) '(invis) '())
   (if (logtest attr A_PROTECT) '(protect) '())
   (if (logtest attr A_REVERSE) '(inverse) '())
   (if (logtest attr A_STANDOUT) '(standout) '())
   (if (logtest attr A_UNDERLINE) '(underline) '())
   (if (logtest attr A_HORIZONTAL) '(horizontal) '())
   (if (logtest attr A_LEFT) '(left) '())
   (if (logtest attr A_LOW) '(low) '())
   (if (logtest attr A_RIGHT) '(right) '())
   (if (logtest attr A_TOP)  '(top) '())
   (if (logtest attr A_VERTICAL) '(vertical) '())))

(define (attr-off! win attrs)
  "Turns off the attributes ATTRS of the given window without turning
any other attributes on or off."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (and (integer? attrs) (exact? attrs)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg attrs)
			 (expected-type 'integer)))))
  (let ((ret (%attr-off! win attrs)))
    (if (not ret)
	(raise (condition (&ncurses-bad-state-error))))))

(define (attr-on! win attrs)
  "Turns on the attributes ATTRS of the given window without turning
any other attributes on or off."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (and (integer? attrs) (exact? attrs)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg attrs)
			 (expected-type 'integer)))))
  (let ((ret (%attr-on! win attrs)))
    (if (not ret)
	(raise (condition (&ncurses-bad-state-error))))))

(define* (attr-set! win attr #:optional color)
  "Sets the given window to have the attributes ATTRS and optionally
the color pair given by COLOR."
  (if color
      (begin
        (%wattr-set! win attr color))
      (%wattr-set! win attr (pair-number attr))))

(define (beep)
  "Sounds an audible alarm on the terminal.  Returns #t on success or
#f on failure."
  (%beep))

(define (bkgd win ch)
  "Sets the background of the named window WIN to the given complex
char CH and applies it to all characters in the window."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg ch)
			 (expected-type 'xchar)))))
  (%bkgd win (xchar->list ch)))

(define (bkgdset! win ch)
  "Sets the background for the window to the given complex character
CH.  This will be combined with all new characters added to the
window."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg ch)
			 (expected-type 'xchar)))))
  (%bkgdset! win (xchar->list ch)))

(define (border win left right top bottom topleft topright bottomleft 
		bottomright)
  "Draws a border on a given window using the given complex characters
TOP, LEFT, RIGHT, etc.  If the number 0 is used instead of a complex
character, then the default character will be used for that border
element."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (for-each 
   (lambda (ch) 
     (if (and (not (xchar? ch)) (not (eq? ch 0)))
	 (raise (condition (&curses-wrong-type-arg-error
			    (arg ch)
			    (expected-type 'xchar))))))
   (list left right top bottom topleft topright bottomleft bottomright))
  
  (let ((l (if (equal? left 0)
               (xchar->list (normal (acs-vline)))
               (xchar->list left)))
        (r (if (equal? right 0)
               (xchar->list (normal (acs-vline)))
               (xchar->list right)))
        (t (if (equal? top 0)
               (xchar->list (normal (acs-hline)))
               (xchar->list top)))
        (b (if (equal? bottom 0)
               (xchar->list (normal (acs-hline)))
               (xchar->list bottom)))
        (tl (if (equal? topleft 0)
                (xchar->list (normal (acs-ulcorner)))
                (xchar->list topleft)))
        (tr (if (equal? topright 0)
                (xchar->list (normal (acs-urcorner)))
                (xchar->list topright)))
        (bl (if (equal? bottomleft 0)
                (xchar->list (normal (acs-llcorner)))
                (xchar->list bottomleft)))
        (br (if (equal? bottomright 0)
                (xchar->list (normal (acs-lrcorner)))
                (xchar->list bottomright))))
    (%border win l r t b tl tr bl br)))

(define (box win v h)
  "Draws a box on the given window using the complex character V for
the verical lines and the complex character H for the horizontal
lines.  If the number 0 is used for H or V instead of a complex
character, the default lines will be used."
  (let ((v2 (if (equal? v 0) (xchar->list (normal (acs-vline))) (xchar->list v)))
        (h2 (if (equal? h 0) (xchar->list (normal (acs-hline))) (xchar->list h))))
    (%border win v2 v2 h2 h2
             (xchar->list (normal-on (acs-ulcorner))) (xchar->list (normal-on (acs-urcorner)))
             (xchar->list (normal-on (acs-llcorner))) (xchar->list (normal-on (acs-lrcorner))))))

(define (can-change-color?)
  "Returns #t if the terminal can change the RGB of a color number, or #f if
the terminal has preassigned, unmodifiable colors."
  (%can-change-color?))

(define* (chgat win n attr color #:key y x)
  "Changes that attributes and color pair of of a given number of
characters starting at the current cursor location in the window WIN.
If X and Y are defined, first move to that position."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (if (not (and (integer? attr) (exact? attr)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg attr)
			 (expected-type 'integer)))))
  (if (not (and (integer? color) (exact? color)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (let ((ret (and (if (and y x)
		      (%wmove win y x)
		      #t)
		  (%wchgat win n attr color))))
    (if (not ret)
	(raise (condition (&ncurses-error))))))

(define (clear win)
  "Copy blanks to every position in the window, and set it to
be cleared completely and repainted at the next window refresh."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (let ((ret (%clear win)))
    (or ret
	(raise (condition (&curses-bad-state-error))))))

(define (clrtobot win)
  "Erases from the cursor location to the end of screen."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (let ((ret (%clrtobot win)))
    (or ret
	(raise (condition (&curses-bad-state-error))))))

(define (clrtoeol win)
  "Erases from the cursor location to the end of the current line."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (%clrtoeol win))

(define (color-content c)
  "Given a color number, this procedure returns a three-element list
containing the red, green, and blue values of the color on a 0 to 1000 scale.
It can return #f if the color is out of range or colors aren't initialized."
  (if (or (not (exact? c)) (not (integer? c)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg c)
			 (expected-type 'integer)))))
  (%color-content c))  

(define (color-set! win pair)
  "Sets the window's color pair to the color pair number PAIR."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (let ((ret (%color-set! win pair)))
    (if (not ret)
	(raise (condition (&curses-out-of-range-error
			   (arg win)))))))

(define (curses-version)
  "Returns, as a string, the version number and patch level of the 
underlying ncurses library."
  (%curses-version))

(define* (delch win #:key y x)
  "Deletes the character under the cursor in the given window,
optionally first moving to the location X, Y."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%wdelch win)
       (raise (condition (&curses-bad-state-error))))))

(define* (deleteln win #:key y x)
  "Deletes the line under the cursor in the given window, optionally first
moving to the position X, Y"
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win -1)
       (raise (condition (&curses-bad-state-error))))))

(define* (echochar win ch #:key y x)
  "Puts the character CH into the given window at its current window
position and then refreshes the window.  If Y and X are set, moves
to (X,Y) first.  Returns #t on success."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg ch)
			 (expected-type 'xchar)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%wechochar win (xchar->list ch))))

(define (erase win)
  "Copy blanks to every position in the window."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (%erase win))

(define (flash)
  "Flashes the screen.  Returns #t on success or #f on failure."
  (%flash))

(define (getbkgd win)
  "Returns the default background of the given window as a complex
char."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (let ((ret (%getbkgd win)))
    (or ret
	(raise (condition (&curses-bad-state-error))))))

(define* (getch win #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wgetch win)))

(define (getmaxy win)
  (car (getmaxyx win)))

(define* (getnstr win n #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wgetnstr win n)))

(define (has-colors?)
  "Returns #t if the current terminal has color capability."
  (%has-colors?))

(define* (hline win ch n #:key y x)
  "Draws a horizontal line of length N using the complex character CH.
If X and Y are given, the cursor is first moved to that location."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg
			 (arg win)
			 (expected-type 'xchar)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (or
	(%whline win (xchar->list ch) n)
	(raise (condition (&curses-bad-state-error))))))

(define* (inch win #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (list->xchar (%winch win))))

(define* (inchstr win #:key y x (n -1))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (map list->xchar (%winchnstr win n))))

(define (init-color! color r g b)
  "Initializes the color number COLOR to have the red-green-blue value
R G B.  R G and B are integers between 0 and 1000.  Returns #t on
success or #f on failure."
  (map (lambda (x)
	 (if (not (and (integer? x) (exact? x)))
	     (raise (condition (&curses-wrong-type-arg-error
				(arg x)
				(expected-type 'integer))))))
       (list color r g b))
  (map (lambda (x)
	 (if (or (< color 0) (> color 1000))
	     (raise (condition (&curses-out-of-range-error
				(arg x))))))
       (list r g b))
  (%init-color color r g b))

(define (init-pair! pair fore back)
  "Initializes the color pair PAIR to have color number FORE as its
foreground color and color number BACK as its background color.  Returns
#t on success or #f on failure."
  (if (not (and (integer? pair) (exact? pair)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg pair)
			 (expected-type 'integer)))))
  (if (not (and (integer? fore) (exact? fore)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg fore)
			 (expected-type 'integer)))))
  (if (not (and (integer? back) (exact? back)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg back)
			 (expected-type 'integer)))))
  (%init-pair! pair fore back))

(define* (insch win ch #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%winsch win (xchar->list ch))))

(define* (instr win #:key y x (n -1))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%winnstr win n)))

(define* (insdelln win n #:key y x)
  "For positive N, inserts N lines into the specified window above the
current line.  For negative N, deletes N lines starting with the one
under the cursor.  Optionally, move to position X, Y first."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win n)
       (raise (condition (&curses-bad-state-error))))))

(define* (insertln win #:key y x)
  "Inserts a line in the current window above the current line,
optionally first moving to the location X, Y. "
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg win)
			 (expected-type 'window)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (or (%winsdelln win 1)
       (raise (condition (&curses-bad-state-error))))))

(define* (insstr win str #:key y x (n -1))
  (and
   (if (and y x)
       (%wmove win y x)
       #t))
  (%winsnstr win str n))

(define (move win y x)
  (%wmove win y x))

;; I hate it when people are 'clever' with dropping letters
(define (nooutrefresh win)
  (noutrefresh win))

(define (pair-content pair)
  "Given a color pair number, this procedure returns a two-element
list containing the foreground color number and the background color
number.  It can return #f if the color pair number is out of range or
colors aren't initialized."
  (if (or (not (exact? pair)) (not (integer? pair)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg pair)
			 (expected-type 'integer)))))
  (%pair-content pair))  

(define* (pechochar win ch #:key y x)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%pechochar win (xchar->list ch))))

(define (redrawln win beg_line end_line)
  (%wredrawln win beg_line end_line))

(define (scroll win)
  (scrl win 1))

(define (standend! win)
  "Turns off all attributes of the given window."
  (%wattr-set! win A_NORMAL (second (%wattr-get win))))

(define (standout! win)
  "Sets the attributes for in the given window to STANDOUT."
  (%wattr-set! win A_STANDOUT (second (%wattr-get win))))

(define (start-color!)
  "Enables color support for curses.  Usually called directly after 'initscr.'"
  (let ((ret (%start-color!)))
    (if (not ret)
	(raise (condition (&curses-bad-state-error))))))

(define (touchwin win)
  (%wtouchln win 0 (getmaxy win) #t))

(define (untouchwin win)
  (%wtouchln win 0 (getmaxy win) #f))

(define* (touchline win start #:optional (count 1))
  (%wtouchln win start count #t))

(define* (untouchline win start #:optional (count 1))
  (%wtouchln win start count #f))

(define (typeahead! port-or-fd)
  (cond
   ((port? port-or-fd) (%typeahead (fileno port-or-fd)))
   (else (%typeahead port-or-fd))))

(define (use-extended-names enable)
  "If ENABLE is #t, this enables whether to allow user-defined,
nonstandard names in the terminfo interface.  If ENABLE is #f, this is
disabled"
  (if (not (boolean? enable))
      (raise (condition (&curses-wrong-type-arg
			 (arg enable)
			 (expected-type 'boolean)))))
  (%use-extended-names enable))

(define* (vline win ch n #:key y x)
  "Draws a vertical line of length N using the complex character CH.
If X and Y are given, the cursor is first moved to that location."
  (if (not (window? win))
      (raise (condition (&curses-wrong-type-arg
			 (arg win)
			 (expected-type 'window)))))
  (if (not (xchar? ch))
      (raise (condition (&curses-wrong-type-arg
			 (arg win)
			 (expected-type 'xchar)))))
  (if (and y x)
      (begin
	(if (not (and (integer? y) (exact? y)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg y)
			       (expected-type 'integer)))))
	(if (not (and (integer? x) (exact? x)))
	    (raise (condition (&curses-wrong-type-arg-error
			       (arg x)
			       (expected-type 'integer)))))))
  (if (not (and (integer? n) (exact? n)))
      (raise (condition (&curses-wrong-type-arg-error
			 (arg n)
			 (expected-type 'integer)))))

  (and (if (and y x)
           (%wmove win y x)
           #t)
       (or 
	(%wvline win (xchar->list ch) n)
	(raise (condition (&curses-bad-state-error))))))

(load-extension "libguile-ncurses" "gucu_init")

;; These functions may not exist depending on the
;; compile time options
(if (defined? 'KEY_EVENT)    (export KEY_EVENT))
(if (defined? 'grantpt)      (export grantpt))
(if (defined? 'key-defined)  (export key-defined))
(if (defined? 'newterm)      (export newterm))
(if (defined? 'ptsname)      (export ptsname))
(if (defined? 'ptsraw)       (export ptsraw))
(if (defined? 'ptsmakeraw)   (export ptsmakeraw))
(if (defined? 'unlockpt)     (export unlockpt))
