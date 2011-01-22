/*
  extra_const.c

  Copyright 2010, 2011 Free Software Foundation, Inc.

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
#define _GNU_SOURCE /* Only really need _BSD_SOURCE and _XOPEN_SOURCE */
#include <termios.h>

#if HAVE_CURSES_H
#include <curses.h>
#include <form.h>
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#include <ncurses/form.h>
#endif

#include "compat.h"
#include "extra_const.h"

SCM gucu_BS0;
SCM gucu_BS1;
SCM gucu_BSDLY;
SCM gucu_CR0;
SCM gucu_CR1;
SCM gucu_CR2;
SCM gucu_CR3;
SCM gucu_CRDLY;
SCM gucu_FF0;
SCM gucu_FF1;
SCM gucu_FFDLY;
SCM gucu_IUCLC;
SCM gucu_NL0;
SCM gucu_NL1;
SCM gucu_NLDLY;
SCM gucu_OCRNL;
SCM gucu_OFDEL;
SCM gucu_OFILL;
SCM gucu_OLCUC;
SCM gucu_ONLCR;
SCM gucu_ONLRET;
SCM gucu_ONOCR;
SCM gucu_OPOST;
SCM gucu_TAB1;
SCM gucu_TAB2;
SCM gucu_TAB3;
SCM gucu_TABDLY;
SCM gucu_TCIFLUSH;
SCM gucu_TCIOFF;
SCM gucu_TCIOFLUSH;
SCM gucu_TCION;
SCM gucu_TCOFLUSH;
SCM gucu_TCOOFF;
SCM gucu_TCOON;
SCM gucu_TCSADRAIN;
SCM gucu_TCSAFLUSH;
SCM gucu_TCSANOW;
SCM gucu_VT0;
SCM gucu_VT1;
SCM gucu_VTDLY;
#if HAVE_DECL_XCASE
SCM gucu_XCASE;
#endif

SCM gucu_NCCS;

SCM gucu_VDISCARD;
SCM gucu_VEOF;
SCM gucu_VEOL;
SCM gucu_VEOL2;
SCM gucu_VERASE;
SCM gucu_VINTR;
SCM gucu_VKILL;
SCM gucu_VLNEXT;
SCM gucu_VMIN;
SCM gucu_VQUIT;
SCM gucu_VREPRINT;
SCM gucu_VSTART;
SCM gucu_VSTOP;
SCM gucu_VSUSP;
SCM gucu_VSWTC;
SCM gucu_VWERASE;

SCM gucu_BRKINT;
SCM gucu_ICRNL;
SCM gucu_IGNBRK;
SCM gucu_IGNCR;
SCM gucu_IGNPAR;
SCM gucu_IMAXBEL;
SCM gucu_INLCR;
SCM gucu_INPCK;
SCM gucu_ISTRIP;
SCM gucu_IXANY;
SCM gucu_IXOFF;
SCM gucu_IXON;
SCM gucu_PARMRK;

SCM gucu_CLOCAL;
SCM gucu_CREAD;
SCM gucu_CS5;
SCM gucu_CS6;
SCM gucu_CS7;
SCM gucu_CS8;
SCM gucu_CSIZE;
SCM gucu_CSTOPB;
SCM gucu_HUPCL;
SCM gucu_PARENB;
SCM gucu_PARODD;
SCM gucu_VTIME;

SCM gucu_ECHO;
SCM gucu_ECHOCTL;
SCM gucu_ECHOE;
SCM gucu_ECHOK;
SCM gucu_ECHOKE;
SCM gucu_ECHONL;
#if HAVE_DECL_ECHOPRT
SCM gucu_ECHOPRT;
#endif
SCM gucu_FLUSHO;
SCM gucu_ICANON;
SCM gucu_IEXTEN;
SCM gucu_ISIG;
SCM gucu_NOFLSH;
#if HAVE_DECL_PENDIN
SCM gucu_PENDIN;
#endif
SCM gucu_TOSTOP;

SCM gucu_B0;
SCM gucu_B110;
SCM gucu_B1200;
SCM gucu_B134;
SCM gucu_B150;
SCM gucu_B1800;
SCM gucu_B19200;
SCM gucu_B200;
SCM gucu_B2400;
SCM gucu_B300;
SCM gucu_B38400;
SCM gucu_B4800;
SCM gucu_B50;
SCM gucu_B600;
SCM gucu_B75;
SCM gucu_B9600;

#define D(x) gucu_ ## x = scm_permanent_object(scm_c_define(#x, scm_from_int(x)))

void
gucu_extra_init_const ()
{
  D(BS0);			/* XOPEN */
  D(BS1);			/* XOPEN */
  D(BSDLY);			/* XOPEN */
  D(CR0);			/* XOPEN */
  D(CR1);			/* XOPEN */
  d(CR2);			/* XOPEN */
  D(CR3);			/* XOPEN */
  D(CRDLY);			/* XOPEN */
  D(FF0);			/* XOPEN */
  D(FF1);			/* XOPEN */
  D(FFDLY);			/* XOPEN */
  D(IUCLC);
  D(NL0);			/* XOPEN */
  D(NL1);			/* XOPEN */
  D(NLDLY);			/* XOPEN */
  D(OCRNL);
  D(OFDEL);
  D(OFILL);
  D(OLCUC);
  D(ONLCR);
  D(ONLRET);
  D(ONOCR);
  D(OPOST);
  D(TAB1);			/* XOPEN */
  D(TAB2);			/* XOPEN */
  D(TAB3);			/* XOPEN */
  D(TABDLY);			/* XOPEN */
  D(TCIFLUSH);
  D(TCIOFF);
  D(TCIOFLUSH);
  D(TCION);
  D(TCOFLUSH);
  D(TCOOFF);
  D(TCOON);
  D(TCSADRAIN);
  D(TCSAFLUSH);
  D(TCSANOW);
  D(VT0);
  D(VT1);
  D(VTDLY);
#if HAVE_DECL_XCASE
  D(XCASE);			/* XOPEN */
#endif

  D(NCCS);

  D(VDISCARD);
  D(VEOF);
  D(VEOL);
  D(VEOL2);
  D(VERASE);
  D(VINTR);
  D(VKILL);
  D(VLNEXT);
  D(VMIN);
  D(VQUIT);
  D(VREPRINT);
  D(VSTART);
  D(VSTOP);
  D(VSUSP);
  D(VSWTC);
  D(VWERASE);

  D(BRKINT);
  D(ICRNL);
  D(IGNBRK);
  D(IGNCR);
  D(IGNPAR);
  D(IMAXBEL);
  D(INLCR);
  D(INPCK);
  D(ISTRIP);
  D(IXANY);
  D(IXOFF);
  D(IXON);
  D(PARMRK);

  D(CLOCAL);
  D(CREAD);
  D(CS5);
  D(CS6);
  D(CS7);
  D(CS8);
  D(CSIZE);
  D(CSTOPB);
  D(HUPCL);
  D(PARENB);
  D(PARODD);
  D(VTIME);

  D(ECHO);
  D(ECHOCTL);			/* BSD/SVID */
  D(ECHOE);			
  D(ECHOK);
  D(ECHOKE);			/* BSD/SVID */
  D(ECHONL);
#if HAVE_DECL_ECHOPRT
  D(ECHOPRT);			/* BSD/SVID */
#endif
  D(FLUSHO);			/* BSD/SVID */
  D(ICANON);
  D(IEXTEN);
  D(ISIG);
  D(NOFLSH);
#if HAVE_DECL_PENDIN
  D(PENDIN);			/* BSD/SVID */
#endif
  D(TOSTOP);

  D(B0);
  D(B110);
  D(B1200);
  D(B134);
  D(B150);
  D(B1800);
  D(B19200);
  D(B200);
  D(B2400);
  D(B300);
  D(B38400);
  D(B4800);
  D(B50);
  D(B600);
  D(B75);
  D(B9600);
}
