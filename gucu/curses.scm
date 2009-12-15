;; -*- Mode: scheme; -*-
(define-module (gucu curses)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (gucu lib)
  #:export (
	    %filter
	    %gucu-wide-ncurses
	    %gucu-ucs4-chars
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
	    set-tabsize
	    stdscr
	    curscr
	    window?
	    screen?
	    open-curses-port

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
            top
            top-off
            top-on
            vertical
            vertical-off
            vertical-on
            )
  #:re-export (xchar-attr
               xchar-color
               xchar-equal?)
  )


;; These functions may not exist depending on the
;; compile time options
(if (defined? 'newterm)      (export newterm))
(if (defined? 'key-defined)  (export key-defined))
(if (defined? 'KEY_EVENT)    (export KEY_EVENT))





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

(define (blink x) (a-attribute x A_BLINK))
(define (blink-off x) (a-attribute-off x A_BLINK))
(define (blink-on x) (a-attribute-on x A_BLINK))
(define (bold x) (a-attribute x A_BOLD))
(define (bold-off x) (a-attribute-off x A_BOLD))
(define (bold-on x) (a-attribute-on x A_BOLD))
(define (dim x) (a-attribute x A_DIM))
(define (dim-off x) (a-attribute-off x A_DIM))
(define (dim-on x) (a-attribute-on x A_DIM))
(define (horizontal x) (a-attribute x A_HORIZONTAL))
(define (horizontal-off x) (a-attribute-off x A_HORIZONTAL))
(define (horizontal-on x) (a-attribute-on x A_HORIZONTAL))
(define (invis x) (a-attribute x A_INVIS))
(define (invis-off x) (a-attribute-off x A_INVIS))
(define (invis-on x) (a-attribute-on x A_INVIS))
(define (left x) (a-attribute x A_LEFT))
(define (left-off x) (a-attribute-off x A_LEFT))
(define (left-on x) (a-attribute-on x A_LEFT))
(define (low x) (a-attribute x A_LOW))
(define (low-off x) (a-attribute-off x A_LOW))
(define (low-on x) (a-attribute-on x A_LOW))
(define (normal x) (a-attribute x A_NORMAL))
(define (normal-off x) (a-attribute-off x A_NORMAL))
(define (normal-on x) (a-attribute-on x A_NORMAL))
(define (protect x) (a-attribute x A_PROTECT))
(define (protect-off x) (a-attribute-off x A_PROTECT))
(define (protect-on x) (a-attribute-on x A_PROTECT))
(define (inverse x) (a-attribute x A_REVERSE))
(define (inverse-off x) (a-attribute-off x A_REVERSE))
(define (inverse-on x) (a-attribute-on x A_REVERSE))
(define (right x) (a-attribute x A_RIGHT))
(define (right-off x) (a-attribute-off x A_RIGHT))
(define (right-on x) (a-attribute-on x A_RIGHT))
(define (top x) (a-attribute x A_TOP))
(define (top-off x) (a-attribute-off x A_TOP))
(define (top-on x) (a-attribute-on x A_TOP))
(define (vertical x) (a-attribute x A_VERTICAL))
(define (vertical-off x) (a-attribute-off x A_VERTICAL))
(define (vertical-on x) (a-attribute-on x A_VERTICAL))

(define (color n x)
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
  (if (not (xchar? ch))
      (scm-error 'wrong-type-arg "addch"
                 "Wrong type argument in position 2 (expecting complex-char): ~s"
                 (list ch) ch))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddch win (xchar->list ch))))

(define* (addchstr win str #:key y x (n -1))
  (if (not (list? str))
      (scm-error 'wrong-type-arg "addchstr"
                 "Wrong type argument in position 2 (expecting list of complex-chars): ~s"
                 (list str) str))
  (if (not (every xchar? str))
      (scm-error 'wrong-type-arg "addchstr"
                 "Wrong type argument in position 2 (expecting list of complex-chars): ~s"
                 (list str) str))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddchnstr win (map xchar->list str) n)))

(define* (addstr win str #:key y x (n -1))
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%waddnstr win str n)))

(define (attr-get win)
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

(define* (attr-set! win attr #:optional color)
  (if color
      (begin
        (%wattr-set! win attr color))
      (%wattr-set! win attr (pair-number attr))))

(define (bkgd win ch)
  (%bkgd win (xchar->list ch)))

(define (bkgdset! win ch)
  (%bkgdset! win (xchar->list ch)))

(define (border win left right top bottom topleft topright bottomleft bottomright)
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
  (let ((v2 (if (equal? v 0) (xchar->list (normal (acs-vline))) (xchar->list v)))
        (h2 (if (equal? h 0) (xchar->list (normal (acs-hline))) (xchar->list h))))
    (%border win v2 v2 h2 h2
             (xchar->list (normal-on (acs-ulcorner))) (xchar->list (normal-on (acs-urcorner))) 
             (xchar->list (normal-on (acs-llcorner))) (xchar->list (normal-on (acs-lrcorner))))))

(define* (chgat win n attr color #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wchgat win n attr color)))

(define* (delch win #:key y x)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%wdelch win)))

(define* (deleteln win #:key y x)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%winsdelln win -1)))

(define* (echochar win ch #:key y x)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%wechochar win (xchar->list ch))))

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

(define* (hline win ch n #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%whline win (xchar->list ch) n)))

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
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%winsdelln win n)))

(define* (insertln win #:key y x)
  (and
   (if (and y x)
       (%wmove win y x)
       #t)
   (%winsdelln win 1)))

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
  (%wattr-set! win A_NORMAL (second (%wattr-get win))))

(define (standout! win)
  (%wattr-set! win A_STANDOUT (second (%wattr-get win))))

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

(define* (vline win ch n #:key y x)
  (and (if (and y x)
           (%wmove win y x)
           #t)
       (%wvline win (xchar->list ch) n)))

(load-extension "libguile-gucu" "gucu_init")
