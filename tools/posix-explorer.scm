;; -*- Mode: scheme; indent-tabs-mode: nil -*-

;; posix-explorer.scm

;; Copyright 2014 Free Software Foundation, Inc.

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



;;; This program displays information about the system using POSIX
;;; calls.  This is really just a demo for the ncurses menus, but, I
;;; may as well make a useful program for my demo.

(use-modules 
 (ice-9 getopt-long)
 (ice-9 i18n)
 (ncurses curses)
 (ncurses menu)
 (srfi srfi-1)
 )

(define PROGRAM_NAME "posix-explorer")
(define PROGRAM_VERSION "0.0")

(define (mainwin-initialize)
  "Initialize curses. Return the main window."
  (let ([mainwin (if (defined? '%guile-ncurses-shell-stdscr)
                     %guile-ncurses-shell-stdscr
                     (initscr))])
    (getnstr mainwin 80)
    (when (has-colors?)
          (start-color!))
    (cbreak!)
    (noecho!)
    (keypad! mainwin #t)
    mainwin))

(define (mainwin-finalize mainwin)
  (endwin))

(define (do-noop win)
  (erase win)
  (addstr win "NOOP" #:x 0 #:y 0)
  (refresh win)
  (sleep 10)
  #t)

(define (get-locale-information)
  (list
   (list "LC_ALL" (setlocale LC_ALL))
   (list "LC_COLLATE" (setlocale LC_COLLATE))
   (list "LC_CTYPE" (setlocale LC_CTYPE))
   (list "LC_MESSAGES" (setlocale LC_MESSAGES))
   (list "LC_MONETARY" (setlocale LC_MONETARY))
   (list "LC_NUMERIC" (setlocale LC_NUMERIC))
   (list "LC_TIME" (setlocale LC_TIME))))

(define (get-system-information)
  (let ([un (uname)])
    (list
     (list "OS Name" (utsname:sysname un))
     (list "Node Name" (utsname:nodename un))
     (list "Release" (utsname:release un))
     (list "Version" (utsname:version un))
     (list "Machine" (utsname:machine un))
     (list "Host Name" (gethostname))
     )))

(define (get-tty-information)
  (list
   (list "Output Is a TTY?" (if (isatty? (current-output-port)) "true" "false"))
   (list "Output TTY Name" (ttyname (current-output-port)))
   (list "Output Process Group ID" (number->string (tcgetpgrp (current-output-port))))
   (list "Input Is a TTY?" (if (isatty? (current-input-port)) "true" "false"))
   (list "Input TTY Name" (ttyname (current-input-port)))
   (list "Input Process Group ID" (number->string (tcgetpgrp (current-input-port))))
   (list "Error Is a TTY?" (if (isatty? (current-error-port)) "true" "false"))
   (list "Error TTY Name" (ttyname (current-error-port)))
   (list "Error Process Group ID" (number->string (tcgetpgrp (current-error-port))))
   (list "Controlling Terminal" (ctermid))
   ))

(define (get-process-information)
  (list
   (list "CWD" (getcwd))
   (list "Mask" (number->string (umask)))
   (list "PID" (number->string (getpid)))
   (list "Groups" (format #f "~S" (getgroups)))
   (list "PPID" (number->string (getppid)))
   (list "UID" (number->string (getuid)))
   (list "UID Name" (passwd:name (getpwuid (getuid))))
   (list "GID" (number->string (getgid)))
   (list "GID Name" (group:name (getgrgid (getgid))))
   (list "EUID" (number->string (geteuid)))
   (list "EUID Name" (passwd:name (getpwuid (geteuid))))
   (list "EGID" (number->string (getegid)))
   (list "EGID Name" (group:name (getgrgid (getegid))))
   (list "PGRP" (number->string (getpgrp)))
   (list "Processor Count" (number->string (total-processor-count)))
   (list "Processors Used" (number->string (current-processor-count)))))

(define (get-environment-variables)
  (let ([EV (sort! (environ) string-locale-ci<?)])
    (map (lambda (entry)
           (string-split entry #\=))
         EV)))

(define (get-time-entries)
  (let* ([curtime (current-time)]
         [timeofday (gettimeofday)]
         [timeofday-sec (car timeofday)]
         [timeofday-usec (cdr timeofday)]
         [now-local (localtime curtime)]
         [now-gmt (gmtime curtime)]
         [now-cpu (times)])
    (list
     (list "Current Time in Epoch" (number->string curtime))
     (list "Seconds in Epoch" (format #f "~S.~S" timeofday-sec timeofday-usec))
     (list "Local Time" (strftime "%c" now-local))
     (list "Local Day of Year" (number->string (tm:yday now-local)))
     (list "Daylight Savings" (cond ([= 0 (tm:isdst now-local)] "no")
                                    ([< 0 (tm:isdst now-local)] "yes")
                                    ([> 0 (tm:isdst now-local)] "unknown")))
     (list "Seconds from GMT" (number->string (tm:gmtoff now-local)))
     (list "Time Zone" (tm:zone now-local))
     (list "GMT" (strftime "%c" now-gmt))
     (list "GMT Day of Year" (number->string (tm:yday now-gmt)))
     (list "CPU Clock Time" (number->string (tms:clock now-cpu)))
     (list "CPU Process Time" (number->string (tms:utime now-cpu)))
     (list "CPU System Time" (number->string (tms:stime now-cpu)))
     (list "CPU CU Time" (number->string (tms:cutime now-cpu)))
     (list "CPU CS Time" (number->string (tms:cstime now-cpu)))
     (list "Guile Time Units Per Sec" (number->string internal-time-units-per-second))
     (list "Guile Real Time" (number->string (get-internal-real-time)))
     (list "Guile Run Time" (number->string (get-internal-run-time))))))

(define (get-user-information-entries)
  (let ([uid (getuid)]
        [euid (geteuid)]
        [gid (getgid)]
        [egid (getegid)]
        [login (getlogin)])
    (let ([uid-pw (getpw uid)]
          [euid-pw (getpw euid)]
          [gid-gr (getgr gid)]
          [egid-gr (getgr egid)])
      (list
       (list "Login Name" login)
       (list "User Name" (passwd:name uid-pw))
       (list "User Password" (passwd:passwd uid-pw))
       (list "User ID" (number->string (passwd:uid uid-pw)))
       (list "User Group ID" (number->string (passwd:gid uid-pw)))
       (list "User Full Name" (passwd:gecos uid-pw))
       (list "User Shell" (passwd:shell uid-pw))
       (list "User Home" (passwd:dir uid-pw))
       (list "Group Name" (group:name gid-gr))
       (list "Group Password" (group:passwd gid-gr))
       (list "Group ID" (number->string (group:gid gid-gr)))
       (list "Effective User Name" (passwd:name uid-pw))
       (list "Effective User Password" (passwd:passwd uid-pw))
       (list "Effective User ID" (number->string (passwd:uid uid-pw)))
       (list "Effective User Group ID" (number->string (passwd:gid uid-pw)))
       (list "Effective User Full Name" (passwd:gecos uid-pw))
       (list "Effective User Shell" (passwd:shell uid-pw))
       (list "Effective User Home" (passwd:dir uid-pw))
       (list "Effective Group Name" (group:name egid-gr))
       (list "Effective Group Password" (group:passwd egid-gr))
       (list "Effective Group ID" (number->string (group:gid egid-gr)))))))


(define (do-single-item win entry)
  (clear win)
  (addstr win (first entry) #:y 0 #:x 0)
  (addstr win (second entry) #:y (1+ (getcury win)) #:x 0)
  (refresh win)
  (getch win))
        

(define (do-sub-menu win title entries-list)
      (let* ([menu-items 
              (map (lambda (entry)
                     (new-item (first entry) (second entry)))
                   entries-list)]
             [menu (new-menu menu-items)]
             [menu-margin 1]                ; outside the box
             [menu-padding 2]               ; inside the box
             [menu-height (- (lines) (* 2 menu-margin))]
             [menu-width (- (cols) (* 2 menu-margin))]
             [menu-win (newwin menu-height menu-width menu-margin menu-margin)])
    
        ;; Set the outer and inner menu windows
        (clear win)
        (keypad! menu-win #t)
        (set-menu-win! menu menu-win)
        (set-menu-sub! menu (derwin menu-win
                                    (- menu-height (* 2 menu-padding))
                                    (- menu-width (* 2 menu-padding))
                                    menu-padding
                                    menu-padding))
        (box menu-win 0 0)
        (addchstr win (bold title) #:x 1 #:y 0)
    
        (post-menu menu)
        (refresh win)
        
        (let loop ([c (getch menu-win)])
          (cond
           ;; Motion controls
           ([eqv? c KEY_DOWN]
            (begin
              (menu-driver menu REQ_DOWN_ITEM)
              (loop (getch menu-win))))
           ([eqv? c KEY_UP]
            (begin
              (menu-driver menu REQ_UP_ITEM)
              (loop (getch menu-win))))
           
           ;; Menu item selection
           ([or (eqv? c KEY_ENTER)
                (eqv? c #\cr)
                (eqv? c #\nl)]
            (begin
              (unpost-menu menu)
              ;; Call handler
              (let ([entry (list-ref entries-list (item-index (current-item menu)))])
                (do-single-item win entry))))

           (else
            (display c)
            (loop (getch menu-win)))))))


(define (do-main-menu win)
  (let* ([entries-list
          ;; Name    Description         Handler
          `(
            ("USER"  "User Information"  ,get-user-information-entries)
            ("TIME"  "Time Information"  ,get-time-entries)
            ("ENV"  "Environment Variables"  ,get-environment-variables)
            ("PROC"  "This Process's Information"  ,get-process-information)
            ("TTY"  "This Terminal Information"  ,get-tty-information)
            ("HOST" "System Identification" ,get-system-information)
            ("LOCALE" "Internationalization information" ,get-locale-information)
            )]
         [menu-items 
          (map (lambda (entry)
                 (new-item (first entry) (second entry)))
               entries-list)]
         [menu (new-menu menu-items)]
         [menu-margin 1]                ; outside the box
         [menu-padding 2]               ; inside the box
         [menu-height (- (lines) (* 2 menu-margin))]
         [menu-width (- (cols) (* 2 menu-margin))]
         [menu-win (newwin menu-height menu-width menu-margin menu-margin)])
    
    ;; Set the outer and inner menu windows
    (clear win)
    (keypad! menu-win #t)
    (set-menu-win! menu menu-win)
    (set-menu-sub! menu (derwin menu-win
                                (- menu-height (* 2 menu-padding))
                                (- menu-width (* 2 menu-padding))
                                menu-padding
                                menu-padding))
    (box menu-win 0 0)
    (attr-on! menu-win (color-pair 1))
    (move menu-win 1 16)
    (addstr win "Main Menu")
    (attr-off! menu-win (color-pair 1))
    
    (post-menu menu)
    (refresh win)
    
    (let loop ([c (getch menu-win)])
      (cond
       ;; Motion controls
       ([eqv? c KEY_DOWN]
        (begin
          (menu-driver menu REQ_DOWN_ITEM)
          (loop (getch menu-win))))
       ([eqv? c KEY_UP]
        (begin
          (menu-driver menu REQ_UP_ITEM)
          (loop (getch menu-win))))
       
       ;; Menu item selection
       ([or (eqv? c KEY_ENTER)
            (eqv? c #\cr)
            (eqv? c #\nl)]
        (begin
          (unpost-menu menu)
          ;; Call handler
          (let ([entry (list-ref entries-list (item-index (current-item menu)))])
            (do-sub-menu win (second entry) ((third entry))))))

       (else
        (display c)
        (loop (getch menu-win)))))
    #t))

(define (main args)
  (let* ((option-spec '((version (single-char #\v) (value #f))
                        (help    (single-char #\h) (value #f))))
         (options (getopt-long args option-spec))
         (help-wanted (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (if (or version-wanted help-wanted)
        (begin
          (if version-wanted
              (display (string-append PROGRAM_NAME " version " PROGRAM_VERSION "\n")))
          (if help-wanted
              (display (string-append "\n"
                                      PROGRAM_NAME " [options]\n"
                                      "  -v, --version   Display version\n"
                                      "  -h, --help      Display this help\n"))))
        ;; else
        (begin
          (setlocale LC_ALL "")
          (let ([mainwin (mainwin-initialize)])
            (do-main-menu mainwin)
            (mainwin-finalize mainwin))))))

(main (command-line))
