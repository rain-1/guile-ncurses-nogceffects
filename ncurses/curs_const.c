/*
curs_const.c

Copyright 2009, 2010, 2011, 2014 Free Software Foundation, Inc.

This file is part of GNU Guile-Ncurses.

Guile-Ncurses is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-Ncurses is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with Guile-Ncurses.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include <config.h>

#include <libguile.h>
#include <wchar.h>

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#else
#error "No curses.h file included"
#endif

#include "compat.h"
#include "curs_const.h"
#include "gucuconfig.h"
#include "type.h"

SCM gucu_EOF;
SCM gucu_ERR;
SCM gucu_FALSE;
SCM gucu_OK;
SCM gucu_TRUE;
SCM gucu_A_ALTCHARSET;
SCM gucu_A_ATTRIBUTES;
SCM gucu_A_BLINK;
SCM gucu_A_BOLD;
SCM gucu_A_CHARTEXT;
SCM gucu_A_COLOR;
SCM gucu_A_DIM;
SCM gucu_A_HORIZONTAL;
SCM gucu_A_INVIS;
SCM gucu_A_LEFT;
SCM gucu_A_NORMAL;
SCM gucu_A_PROTECT;
SCM gucu_A_REVERSE;
SCM gucu_A_STANDOUT;
SCM gucu_A_UNDERLINE;
SCM gucu_A_LOW;
SCM gucu_A_RIGHT;
SCM gucu_A_TOP;
SCM gucu_A_VERTICAL;
SCM gucu_COLOR_BLACK;
SCM gucu_COLOR_GREEN;
SCM gucu_COLOR_RED;
SCM gucu_COLOR_YELLOW;
SCM gucu_COLOR_BLUE;
SCM gucu_COLOR_MAGENTA;
SCM gucu_COLOR_CYAN;
SCM gucu_COLOR_WHITE;
SCM gucu_KEY_CODE_YES;
SCM gucu_KEY_MIN;
SCM gucu_KEY_BREAK;
SCM gucu_KEY_SRESET;
SCM gucu_KEY_RESET;
SCM gucu_KEY_DOWN;
SCM gucu_KEY_UP;
SCM gucu_KEY_LEFT;
SCM gucu_KEY_RIGHT;
SCM gucu_KEY_HOME;
SCM gucu_KEY_BACKSPACE;
SCM gucu_KEY_F0;
SCM gucu_KEY_DL;
SCM gucu_KEY_IL;
SCM gucu_KEY_DC;
SCM gucu_KEY_IC;
SCM gucu_KEY_EIC;
SCM gucu_KEY_CLEAR;
SCM gucu_KEY_EOS;
SCM gucu_KEY_EOL;
SCM gucu_KEY_SF;
SCM gucu_KEY_SR;
SCM gucu_KEY_NPAGE;
SCM gucu_KEY_PPAGE;
SCM gucu_KEY_STAB;
SCM gucu_KEY_CTAB;
SCM gucu_KEY_CATAB;
SCM gucu_KEY_ENTER;
SCM gucu_KEY_PRINT;
SCM gucu_KEY_LL;
SCM gucu_KEY_A1;
SCM gucu_KEY_A3;
SCM gucu_KEY_B2;
SCM gucu_KEY_C1;
SCM gucu_KEY_C3;
SCM gucu_KEY_BTAB;
SCM gucu_KEY_BEG;
SCM gucu_KEY_CANCEL;
SCM gucu_KEY_CLOSE;
SCM gucu_KEY_COMMAND;
SCM gucu_KEY_COPY;
SCM gucu_KEY_CREATE;
SCM gucu_KEY_END;
SCM gucu_KEY_EXIT;
SCM gucu_KEY_FIND;
SCM gucu_KEY_HELP;
SCM gucu_KEY_MARK;
SCM gucu_KEY_MESSAGE;
SCM gucu_KEY_MOVE;
SCM gucu_KEY_NEXT;
SCM gucu_KEY_OPEN;
SCM gucu_KEY_OPTIONS;
SCM gucu_KEY_PREVIOUS;
SCM gucu_KEY_REDO;
SCM gucu_KEY_REFERENCE;
SCM gucu_KEY_REFRESH;
SCM gucu_KEY_REPLACE;
SCM gucu_KEY_RESTART;
SCM gucu_KEY_RESUME;
SCM gucu_KEY_SAVE;
SCM gucu_KEY_SBEG;
SCM gucu_KEY_SCANCEL;
SCM gucu_KEY_SCOMMAND;
SCM gucu_KEY_SCOPY;
SCM gucu_KEY_SCREATE;
SCM gucu_KEY_SDC;
SCM gucu_KEY_SDL;
SCM gucu_KEY_SELECT;
SCM gucu_KEY_SEND;
SCM gucu_KEY_SEOL;
SCM gucu_KEY_SEXIT;
SCM gucu_KEY_SFIND;
SCM gucu_KEY_SHELP;
SCM gucu_KEY_SHOME;
SCM gucu_KEY_SIC;
SCM gucu_KEY_SLEFT;
SCM gucu_KEY_SMESSAGE;
SCM gucu_KEY_SMOVE;
SCM gucu_KEY_SNEXT;
SCM gucu_KEY_SOPTIONS;
SCM gucu_KEY_SPREVIOUS;
SCM gucu_KEY_SPRINT;
SCM gucu_KEY_SREDO;
SCM gucu_KEY_SREPLACE;
SCM gucu_KEY_SRIGHT;
SCM gucu_KEY_SRSUME;
SCM gucu_KEY_SSAVE;
SCM gucu_KEY_SSUSPEND;
SCM gucu_KEY_SUNDO;
SCM gucu_KEY_SUSPEND;
SCM gucu_KEY_UNDO;
SCM gucu_KEY_MOUSE;
SCM gucu_KEY_RESIZE;
#ifdef HAS_KEY_EVENT
SCM gucu_KEY_EVENT;
#endif
SCM gucu_BUTTON1_PRESSED;
SCM gucu_BUTTON1_RELEASED;
SCM gucu_BUTTON1_CLICKED;
SCM gucu_BUTTON1_DOUBLE_CLICKED;
SCM gucu_BUTTON1_TRIPLE_CLICKED;
SCM gucu_BUTTON2_PRESSED;
SCM gucu_BUTTON2_RELEASED;
SCM gucu_BUTTON2_CLICKED;
SCM gucu_BUTTON2_DOUBLE_CLICKED;
SCM gucu_BUTTON2_TRIPLE_CLICKED;
SCM gucu_BUTTON3_PRESSED;
SCM gucu_BUTTON3_RELEASED;
SCM gucu_BUTTON3_CLICKED;
SCM gucu_BUTTON3_DOUBLE_CLICKED;
SCM gucu_BUTTON3_TRIPLE_CLICKED;
SCM gucu_BUTTON4_PRESSED;
SCM gucu_BUTTON4_RELEASED;
SCM gucu_BUTTON4_CLICKED;
SCM gucu_BUTTON4_DOUBLE_CLICKED;
SCM gucu_BUTTON4_TRIPLE_CLICKED;
SCM gucu_BUTTON_SHIFT;
SCM gucu_BUTTON_CTRL;
SCM gucu_BUTTON_ALT;
SCM gucu_ALL_MOUSE_EVENTS;
SCM gucu_REPORT_MOUSE_POSITION;

SCM gucu_wide_ncurses;
SCM gucu_ucs4_chars;
SCM gucu_broken_is_pad;
SCM gucu_broken_is_subwin;

void
gucu_init_constant ()
{
  gucu_EOF = scm_permanent_object (scm_c_define ("EOF", scm_from_int (EOF)));
  gucu_ERR = scm_permanent_object (scm_c_define ("ERR", scm_from_int (ERR)));
  gucu_FALSE =
    scm_permanent_object (scm_c_define ("FALSE", scm_from_int (FALSE)));
  gucu_OK = scm_permanent_object (scm_c_define ("OK", scm_from_int (OK)));
  gucu_TRUE =
    scm_permanent_object (scm_c_define ("TRUE", scm_from_int (TRUE)));
  gucu_A_ALTCHARSET =
    scm_permanent_object (scm_c_define
			  ("A_ALTCHARSET", _scm_from_chtype (A_ALTCHARSET)));
  gucu_A_ATTRIBUTES =
    scm_permanent_object (scm_c_define
			  ("A_ATTRIBUTES", _scm_from_chtype (A_ATTRIBUTES)));
  gucu_A_BLINK =
    scm_permanent_object (scm_c_define
			  ("A_BLINK", _scm_from_chtype (A_BLINK)));
  gucu_A_BOLD =
    scm_permanent_object (scm_c_define ("A_BOLD", _scm_from_chtype (A_BOLD)));
  gucu_A_CHARTEXT =
    scm_permanent_object (scm_c_define
			  ("A_CHARTEXT", _scm_from_chtype (A_CHARTEXT)));
  gucu_A_COLOR =
    scm_permanent_object (scm_c_define
			  ("A_COLOR", _scm_from_chtype (A_COLOR)));
  gucu_A_DIM =
    scm_permanent_object (scm_c_define ("A_DIM", _scm_from_chtype (A_DIM)));
  gucu_A_INVIS =
    scm_permanent_object (scm_c_define
			  ("A_INVIS", _scm_from_chtype (A_INVIS)));
  gucu_A_NORMAL =
    scm_permanent_object (scm_c_define
			  ("A_NORMAL", _scm_from_chtype (A_NORMAL)));
  gucu_A_PROTECT =
    scm_permanent_object (scm_c_define
			  ("A_PROTECT", _scm_from_chtype (A_PROTECT)));
  gucu_A_REVERSE =
    scm_permanent_object (scm_c_define
			  ("A_REVERSE", _scm_from_chtype (A_REVERSE)));
  gucu_A_STANDOUT =
    scm_permanent_object (scm_c_define
			  ("A_STANDOUT", _scm_from_chtype (A_STANDOUT)));
  gucu_A_UNDERLINE =
    scm_permanent_object (scm_c_define
			  ("A_UNDERLINE", _scm_from_chtype (A_UNDERLINE)));
  gucu_A_HORIZONTAL =
    scm_permanent_object (scm_c_define
			  ("A_HORIZONTAL", _scm_from_attr (A_HORIZONTAL)));
  gucu_A_LEFT =
    scm_permanent_object (scm_c_define ("A_LEFT", _scm_from_attr (A_LEFT)));
  gucu_A_LOW =
    scm_permanent_object (scm_c_define ("A_LOW", _scm_from_attr (A_LOW)));
  gucu_A_RIGHT =
    scm_permanent_object (scm_c_define ("A_RIGHT", _scm_from_attr (A_RIGHT)));
  gucu_A_TOP =
    scm_permanent_object (scm_c_define ("A_TOP", _scm_from_attr (A_TOP)));
  gucu_A_VERTICAL =
    scm_permanent_object (scm_c_define
			  ("A_VERTICAL", _scm_from_attr (A_VERTICAL)));
  gucu_COLOR_BLACK =
    scm_permanent_object (scm_c_define
			  ("COLOR_BLACK", scm_from_int (COLOR_BLACK)));
  gucu_COLOR_GREEN =
    scm_permanent_object (scm_c_define
			  ("COLOR_GREEN", scm_from_int (COLOR_GREEN)));
  gucu_COLOR_RED =
    scm_permanent_object (scm_c_define
			  ("COLOR_RED", scm_from_int (COLOR_RED)));
  gucu_COLOR_YELLOW =
    scm_permanent_object (scm_c_define
			  ("COLOR_YELLOW", scm_from_int (COLOR_YELLOW)));
  gucu_COLOR_BLUE =
    scm_permanent_object (scm_c_define
			  ("COLOR_BLUE", scm_from_int (COLOR_BLUE)));
  gucu_COLOR_MAGENTA =
    scm_permanent_object (scm_c_define
			  ("COLOR_MAGENTA", scm_from_int (COLOR_MAGENTA)));
  gucu_COLOR_CYAN =
    scm_permanent_object (scm_c_define
			  ("COLOR_CYAN", scm_from_int (COLOR_CYAN)));
  gucu_COLOR_WHITE =
    scm_permanent_object (scm_c_define
			  ("COLOR_WHITE", scm_from_int (COLOR_WHITE)));
  gucu_KEY_CODE_YES =
    scm_permanent_object (scm_c_define
			  ("KEY_CODE_YES", scm_from_int (KEY_CODE_YES)));
  gucu_KEY_MIN =
    scm_permanent_object (scm_c_define ("KEY_MIN", scm_from_int (KEY_MIN)));
  gucu_KEY_BREAK =
    scm_permanent_object (scm_c_define
			  ("KEY_BREAK", scm_from_int (KEY_BREAK)));
  gucu_KEY_SRESET =
    scm_permanent_object (scm_c_define
			  ("KEY_SRESET", scm_from_int (KEY_SRESET)));
  gucu_KEY_RESET =
    scm_permanent_object (scm_c_define
			  ("KEY_RESET", scm_from_int (KEY_RESET)));
  gucu_KEY_DOWN =
    scm_permanent_object (scm_c_define ("KEY_DOWN", scm_from_int (KEY_DOWN)));
  gucu_KEY_UP =
    scm_permanent_object (scm_c_define ("KEY_UP", scm_from_int (KEY_UP)));
  gucu_KEY_LEFT =
    scm_permanent_object (scm_c_define ("KEY_LEFT", scm_from_int (KEY_LEFT)));
  gucu_KEY_RIGHT =
    scm_permanent_object (scm_c_define
			  ("KEY_RIGHT", scm_from_int (KEY_RIGHT)));
  gucu_KEY_HOME =
    scm_permanent_object (scm_c_define ("KEY_HOME", scm_from_int (KEY_HOME)));
  gucu_KEY_BACKSPACE =
    scm_permanent_object (scm_c_define
			  ("KEY_BACKSPACE", scm_from_int (KEY_BACKSPACE)));
  gucu_KEY_F0 =
    scm_permanent_object (scm_c_define ("KEY_F0", scm_from_int (KEY_F0)));
  gucu_KEY_DL =
    scm_permanent_object (scm_c_define ("KEY_DL", scm_from_int (KEY_DL)));
  gucu_KEY_IL =
    scm_permanent_object (scm_c_define ("KEY_IL", scm_from_int (KEY_IL)));
  gucu_KEY_DC =
    scm_permanent_object (scm_c_define ("KEY_DC", scm_from_int (KEY_DC)));
  gucu_KEY_IC =
    scm_permanent_object (scm_c_define ("KEY_IC", scm_from_int (KEY_IC)));
  gucu_KEY_EIC =
    scm_permanent_object (scm_c_define ("KEY_EIC", scm_from_int (KEY_EIC)));
  gucu_KEY_CLEAR =
    scm_permanent_object (scm_c_define
			  ("KEY_CLEAR", scm_from_int (KEY_CLEAR)));
  gucu_KEY_EOS =
    scm_permanent_object (scm_c_define ("KEY_EOS", scm_from_int (KEY_EOS)));
  gucu_KEY_EOL =
    scm_permanent_object (scm_c_define ("KEY_EOL", scm_from_int (KEY_EOL)));
  gucu_KEY_SF =
    scm_permanent_object (scm_c_define ("KEY_SF", scm_from_int (KEY_SF)));
  gucu_KEY_SR =
    scm_permanent_object (scm_c_define ("KEY_SR", scm_from_int (KEY_SR)));
  gucu_KEY_NPAGE =
    scm_permanent_object (scm_c_define
			  ("KEY_NPAGE", scm_from_int (KEY_NPAGE)));
  gucu_KEY_PPAGE =
    scm_permanent_object (scm_c_define
			  ("KEY_PPAGE", scm_from_int (KEY_PPAGE)));
  gucu_KEY_STAB =
    scm_permanent_object (scm_c_define ("KEY_STAB", scm_from_int (KEY_STAB)));
  gucu_KEY_CTAB =
    scm_permanent_object (scm_c_define ("KEY_CTAB", scm_from_int (KEY_CTAB)));
  gucu_KEY_CATAB =
    scm_permanent_object (scm_c_define
			  ("KEY_CATAB", scm_from_int (KEY_CATAB)));
  gucu_KEY_ENTER =
    scm_permanent_object (scm_c_define
			  ("KEY_ENTER", scm_from_int (KEY_ENTER)));
  gucu_KEY_PRINT =
    scm_permanent_object (scm_c_define
			  ("KEY_PRINT", scm_from_int (KEY_PRINT)));
  gucu_KEY_LL =
    scm_permanent_object (scm_c_define ("KEY_LL", scm_from_int (KEY_LL)));
  gucu_KEY_A1 =
    scm_permanent_object (scm_c_define ("KEY_A1", scm_from_int (KEY_A1)));
  gucu_KEY_A3 =
    scm_permanent_object (scm_c_define ("KEY_A3", scm_from_int (KEY_A3)));
  gucu_KEY_B2 =
    scm_permanent_object (scm_c_define ("KEY_B2", scm_from_int (KEY_B2)));
  gucu_KEY_C1 =
    scm_permanent_object (scm_c_define ("KEY_C1", scm_from_int (KEY_C1)));
  gucu_KEY_C3 =
    scm_permanent_object (scm_c_define ("KEY_C3", scm_from_int (KEY_C3)));
  gucu_KEY_BTAB =
    scm_permanent_object (scm_c_define ("KEY_BTAB", scm_from_int (KEY_BTAB)));
  gucu_KEY_BEG =
    scm_permanent_object (scm_c_define ("KEY_BEG", scm_from_int (KEY_BEG)));
  gucu_KEY_CANCEL =
    scm_permanent_object (scm_c_define
			  ("KEY_CANCEL", scm_from_int (KEY_CANCEL)));
  gucu_KEY_CLOSE =
    scm_permanent_object (scm_c_define
			  ("KEY_CLOSE", scm_from_int (KEY_CLOSE)));
  gucu_KEY_COMMAND =
    scm_permanent_object (scm_c_define
			  ("KEY_COMMAND", scm_from_int (KEY_COMMAND)));
  gucu_KEY_COPY =
    scm_permanent_object (scm_c_define ("KEY_COPY", scm_from_int (KEY_COPY)));
  gucu_KEY_CREATE =
    scm_permanent_object (scm_c_define
			  ("KEY_CREATE", scm_from_int (KEY_CREATE)));
  gucu_KEY_END =
    scm_permanent_object (scm_c_define ("KEY_END", scm_from_int (KEY_END)));
  gucu_KEY_EXIT =
    scm_permanent_object (scm_c_define ("KEY_EXIT", scm_from_int (KEY_EXIT)));
  gucu_KEY_FIND =
    scm_permanent_object (scm_c_define ("KEY_FIND", scm_from_int (KEY_FIND)));
  gucu_KEY_HELP =
    scm_permanent_object (scm_c_define ("KEY_HELP", scm_from_int (KEY_HELP)));
  gucu_KEY_MARK =
    scm_permanent_object (scm_c_define ("KEY_MARK", scm_from_int (KEY_MARK)));
  gucu_KEY_MESSAGE =
    scm_permanent_object (scm_c_define
			  ("KEY_MESSAGE", scm_from_int (KEY_MESSAGE)));
  gucu_KEY_MOVE =
    scm_permanent_object (scm_c_define ("KEY_MOVE", scm_from_int (KEY_MOVE)));
  gucu_KEY_NEXT =
    scm_permanent_object (scm_c_define ("KEY_NEXT", scm_from_int (KEY_NEXT)));
  gucu_KEY_OPEN =
    scm_permanent_object (scm_c_define ("KEY_OPEN", scm_from_int (KEY_OPEN)));
  gucu_KEY_OPTIONS =
    scm_permanent_object (scm_c_define
			  ("KEY_OPTIONS", scm_from_int (KEY_OPTIONS)));
  gucu_KEY_PREVIOUS =
    scm_permanent_object (scm_c_define
			  ("KEY_PREVIOUS", scm_from_int (KEY_PREVIOUS)));
  gucu_KEY_REDO =
    scm_permanent_object (scm_c_define ("KEY_REDO", scm_from_int (KEY_REDO)));
  gucu_KEY_REFERENCE =
    scm_permanent_object (scm_c_define
			  ("KEY_REFERENCE", scm_from_int (KEY_REFERENCE)));
  gucu_KEY_REFRESH =
    scm_permanent_object (scm_c_define
			  ("KEY_REFRESH", scm_from_int (KEY_REFRESH)));
  gucu_KEY_REPLACE =
    scm_permanent_object (scm_c_define
			  ("KEY_REPLACE", scm_from_int (KEY_REPLACE)));
  gucu_KEY_RESTART =
    scm_permanent_object (scm_c_define
			  ("KEY_RESTART", scm_from_int (KEY_RESTART)));
  gucu_KEY_RESUME =
    scm_permanent_object (scm_c_define
			  ("KEY_RESUME", scm_from_int (KEY_RESUME)));
  gucu_KEY_SAVE =
    scm_permanent_object (scm_c_define ("KEY_SAVE", scm_from_int (KEY_SAVE)));
  gucu_KEY_SBEG =
    scm_permanent_object (scm_c_define ("KEY_SBEG", scm_from_int (KEY_SBEG)));
  gucu_KEY_SCANCEL =
    scm_permanent_object (scm_c_define
			  ("KEY_SCANCEL", scm_from_int (KEY_SCANCEL)));
  gucu_KEY_SCOMMAND =
    scm_permanent_object (scm_c_define
			  ("KEY_SCOMMAND", scm_from_int (KEY_SCOMMAND)));
  gucu_KEY_SCOPY =
    scm_permanent_object (scm_c_define
			  ("KEY_SCOPY", scm_from_int (KEY_SCOPY)));
  gucu_KEY_SCREATE =
    scm_permanent_object (scm_c_define
			  ("KEY_SCREATE", scm_from_int (KEY_SCREATE)));
  gucu_KEY_SDC =
    scm_permanent_object (scm_c_define ("KEY_SDC", scm_from_int (KEY_SDC)));
  gucu_KEY_SDL =
    scm_permanent_object (scm_c_define ("KEY_SDL", scm_from_int (KEY_SDL)));
  gucu_KEY_SELECT =
    scm_permanent_object (scm_c_define
			  ("KEY_SELECT", scm_from_int (KEY_SELECT)));
  gucu_KEY_SEND =
    scm_permanent_object (scm_c_define ("KEY_SEND", scm_from_int (KEY_SEND)));
  gucu_KEY_SEOL =
    scm_permanent_object (scm_c_define ("KEY_SEOL", scm_from_int (KEY_SEOL)));
  gucu_KEY_SEXIT =
    scm_permanent_object (scm_c_define
			  ("KEY_SEXIT", scm_from_int (KEY_SEXIT)));
  gucu_KEY_SFIND =
    scm_permanent_object (scm_c_define
			  ("KEY_SFIND", scm_from_int (KEY_SFIND)));
  gucu_KEY_SHELP =
    scm_permanent_object (scm_c_define
			  ("KEY_SHELP", scm_from_int (KEY_SHELP)));
  gucu_KEY_SHOME =
    scm_permanent_object (scm_c_define
			  ("KEY_SHOME", scm_from_int (KEY_SHOME)));
  gucu_KEY_SIC =
    scm_permanent_object (scm_c_define ("KEY_SIC", scm_from_int (KEY_SIC)));
  gucu_KEY_SLEFT =
    scm_permanent_object (scm_c_define
			  ("KEY_SLEFT", scm_from_int (KEY_SLEFT)));
  gucu_KEY_SMESSAGE =
    scm_permanent_object (scm_c_define
			  ("KEY_SMESSAGE", scm_from_int (KEY_SMESSAGE)));
  gucu_KEY_SMOVE =
    scm_permanent_object (scm_c_define
			  ("KEY_SMOVE", scm_from_int (KEY_SMOVE)));
  gucu_KEY_SNEXT =
    scm_permanent_object (scm_c_define
			  ("KEY_SNEXT", scm_from_int (KEY_SNEXT)));
  gucu_KEY_SOPTIONS =
    scm_permanent_object (scm_c_define
			  ("KEY_SOPTIONS", scm_from_int (KEY_SOPTIONS)));
  gucu_KEY_SPREVIOUS =
    scm_permanent_object (scm_c_define
			  ("KEY_SPREVIOUS", scm_from_int (KEY_SPREVIOUS)));
  gucu_KEY_SPRINT =
    scm_permanent_object (scm_c_define
			  ("KEY_SPRINT", scm_from_int (KEY_SPRINT)));
  gucu_KEY_SREDO =
    scm_permanent_object (scm_c_define
			  ("KEY_SREDO", scm_from_int (KEY_SREDO)));
  gucu_KEY_SREPLACE =
    scm_permanent_object (scm_c_define
			  ("KEY_SREPLACE", scm_from_int (KEY_SREPLACE)));
  gucu_KEY_SRIGHT =
    scm_permanent_object (scm_c_define
			  ("KEY_SRIGHT", scm_from_int (KEY_SRIGHT)));
  gucu_KEY_SRSUME =
    scm_permanent_object (scm_c_define
			  ("KEY_SRSUME", scm_from_int (KEY_SRSUME)));
  gucu_KEY_SSAVE =
    scm_permanent_object (scm_c_define
			  ("KEY_SSAVE", scm_from_int (KEY_SSAVE)));
  gucu_KEY_SSUSPEND =
    scm_permanent_object (scm_c_define
			  ("KEY_SSUSPEND", scm_from_int (KEY_SSUSPEND)));
  gucu_KEY_SUNDO =
    scm_permanent_object (scm_c_define
			  ("KEY_SUNDO", scm_from_int (KEY_SUNDO)));
  gucu_KEY_SUSPEND =
    scm_permanent_object (scm_c_define
			  ("KEY_SUSPEND", scm_from_int (KEY_SUSPEND)));
  gucu_KEY_UNDO =
    scm_permanent_object (scm_c_define ("KEY_UNDO", scm_from_int (KEY_UNDO)));
  gucu_KEY_MOUSE =
    scm_permanent_object (scm_c_define
			  ("KEY_MOUSE", scm_from_int (KEY_MOUSE)));
  gucu_KEY_RESIZE =
    scm_permanent_object (scm_c_define
			  ("KEY_RESIZE", scm_from_int (KEY_RESIZE)));
#ifdef HAS_KEY_EVENT
  gucu_KEY_EVENT =
    scm_permanent_object (scm_c_define
			  ("KEY_EVENT", scm_from_int (KEY_EVENT)));
#endif
  gucu_BUTTON1_PRESSED =
    scm_permanent_object (scm_c_define
			  ("BUTTON1_PRESSED",
			   scm_from_uint (BUTTON1_PRESSED)));
  gucu_BUTTON1_RELEASED =
    scm_permanent_object (scm_c_define
			  ("BUTTON1_RELEASED",
			   scm_from_uint (BUTTON1_RELEASED)));
  gucu_BUTTON1_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON1_CLICKED",
			   scm_from_uint (BUTTON1_CLICKED)));
  gucu_BUTTON1_DOUBLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON1_DOUBLE_CLICKED",
			   scm_from_uint (BUTTON1_DOUBLE_CLICKED)));
  gucu_BUTTON1_TRIPLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON1_TRIPLE_CLICKED",
			   scm_from_uint (BUTTON1_TRIPLE_CLICKED)));
  gucu_BUTTON2_PRESSED =
    scm_permanent_object (scm_c_define
			  ("BUTTON2_PRESSED",
			   scm_from_uint (BUTTON2_PRESSED)));
  gucu_BUTTON2_RELEASED =
    scm_permanent_object (scm_c_define
			  ("BUTTON2_RELEASED",
			   scm_from_uint (BUTTON2_RELEASED)));
  gucu_BUTTON2_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON2_CLICKED",
			   scm_from_uint (BUTTON2_CLICKED)));
  gucu_BUTTON2_DOUBLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON2_DOUBLE_CLICKED",
			   scm_from_uint (BUTTON2_DOUBLE_CLICKED)));
  gucu_BUTTON2_TRIPLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON2_TRIPLE_CLICKED",
			   scm_from_uint (BUTTON2_TRIPLE_CLICKED)));
  gucu_BUTTON3_PRESSED =
    scm_permanent_object (scm_c_define
			  ("BUTTON3_PRESSED",
			   scm_from_uint (BUTTON3_PRESSED)));
  gucu_BUTTON3_RELEASED =
    scm_permanent_object (scm_c_define
			  ("BUTTON3_RELEASED",
			   scm_from_uint (BUTTON3_RELEASED)));
  gucu_BUTTON3_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON3_CLICKED",
			   scm_from_uint (BUTTON3_CLICKED)));
  gucu_BUTTON3_DOUBLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON3_DOUBLE_CLICKED",
			   scm_from_uint (BUTTON3_DOUBLE_CLICKED)));
  gucu_BUTTON3_TRIPLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON3_TRIPLE_CLICKED",
			   scm_from_uint (BUTTON3_TRIPLE_CLICKED)));
  gucu_BUTTON4_PRESSED =
    scm_permanent_object (scm_c_define
			  ("BUTTON4_PRESSED",
			   scm_from_uint (BUTTON4_PRESSED)));
  gucu_BUTTON4_RELEASED =
    scm_permanent_object (scm_c_define
			  ("BUTTON4_RELEASED",
			   scm_from_uint (BUTTON4_RELEASED)));
  gucu_BUTTON4_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON4_CLICKED",
			   scm_from_uint (BUTTON4_CLICKED)));
  gucu_BUTTON4_DOUBLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON4_DOUBLE_CLICKED",
			   scm_from_uint (BUTTON4_DOUBLE_CLICKED)));
  gucu_BUTTON4_TRIPLE_CLICKED =
    scm_permanent_object (scm_c_define
			  ("BUTTON4_TRIPLE_CLICKED",
			   scm_from_uint (BUTTON4_TRIPLE_CLICKED)));
  gucu_BUTTON_SHIFT =
    scm_permanent_object (scm_c_define
			  ("BUTTON_SHIFT", scm_from_uint (BUTTON_SHIFT)));
  gucu_BUTTON_CTRL =
    scm_permanent_object (scm_c_define
			  ("BUTTON_CTRL", scm_from_uint (BUTTON_CTRL)));
  gucu_BUTTON_ALT =
    scm_permanent_object (scm_c_define
			  ("BUTTON_ALT", scm_from_uint (BUTTON_ALT)));
  gucu_ALL_MOUSE_EVENTS =
    scm_permanent_object (scm_c_define
			  ("ALL_MOUSE_EVENTS",
			   scm_from_uint (ALL_MOUSE_EVENTS)));
  gucu_REPORT_MOUSE_POSITION =
    scm_permanent_object (scm_c_define
			  ("REPORT_MOUSE_POSITION",
			   scm_from_uint (REPORT_MOUSE_POSITION)));

#ifdef HAVE_NCURSESW
  gucu_wide_ncurses =
    scm_permanent_object (scm_c_define ("%wide-ncurses", SCM_BOOL_T));
#else
  gucu_wide_ncurses =
    scm_permanent_object (scm_c_define ("%wide-ncurses", SCM_BOOL_F));
#endif

#ifdef GUILE_CHARS_ARE_UCS4
  gucu_ucs4_chars =
    scm_permanent_object (scm_c_define ("%ucs4-chars", SCM_BOOL_T));
#else
  gucu_ucs4_chars =
    scm_permanent_object (scm_c_define ("%ucs4-chars", SCM_BOOL_F));
#endif

#if defined(HAVE_IS_PAD) || ! defined(NCURSES_OPAQUE)
  gucu_broken_is_pad = 
    scm_permanent_object (scm_c_define ("%is-pad-broken", SCM_BOOL_F));
#else
  gucu_broken_is_pad = 
    scm_permanent_object (scm_c_define ("%is-pad-broken", SCM_BOOL_T));
#endif

#if defined(HAVE_IS_SUBWIN) || ! defined(NCURSES_OPAQUE)
  gucu_broken_is_subwin = 
    scm_permanent_object (scm_c_define ("%is-subwin-broken", SCM_BOOL_F));
#else
  gucu_broken_is_subwin = 
    scm_permanent_object (scm_c_define ("%is-subwin-broken", SCM_BOOL_T));
#endif
}
