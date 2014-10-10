/*
  curs_port.c

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
#include <stdio.h>
#include <unistd.h>

#if HAVE_CURSES_H
#include <curses.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#else
#error "No curses.h file included"
#endif

#include "type.h"
#include "curs_port.h"
#include "compat.h"

#ifdef GUCU_USE_COOKIE

#define PORT_ERR (-1)
#define PORT_OK (0)

static ssize_t port_read (void *cookie, char *buf, size_t siz);
static ssize_t port_write (void *cookie, const char *buf, size_t siz);
static int port_seek (void *cookie, off64_t * pos, int whence);
static int port_close (void *cookie);

static cookie_io_functions_t port_funcs;

static ssize_t
port_read (void *cookie, char *buf, size_t siz)
{
  SCM port = PTR2SCM (cookie);

#ifdef GUILE_CHARS_ARE_UCS4
  int c;
  if (siz >= 1)
    {
      c = scm_get_byte_or_eof (port);

      if (c == EOF)
        return 0;
      else
        buf[0] = c;

      return 1;
    }
  else
    return PORT_ERR;
#else
  /* For Guile 1.8.x, we use scm_read_char so we can preserve line
     and column information.  */
  SCM c;
  if (siz >= 1)
    {
      c = scm_read_char (port);

      if (scm_is_true (scm_eof_object_p (c)))
        return 0;
      else
        buf[0] = scm_to_char (c);

      return 1;
    }
  else
    return PORT_ERR;
#endif
}

static ssize_t
port_write (void *cookie, const char *buf, size_t siz)
{
  SCM port = PTR2SCM (cookie);

#ifdef GUILE_CHARS_ARE_UCS4
  if (siz > SSIZE_MAX)
    {
      scm_c_write (port, buf, SSIZE_MAX);
      return SSIZE_MAX;
    }
  else
    {
      scm_c_write (port, buf, siz);
      return siz;
    }
#else
  {
    size_t i;

    for (i = 0; i < siz; i++)
      {
        scm_write_char (scm_integer_to_char (scm_from_char (buf[i])), port);
      }
  }

  return siz;
#endif
}

static int
port_seek (void *cookie, off64_t * pos, int whence)
{
  SCM port = PTR2SCM (cookie);
  SCM new_pos;

  new_pos = scm_seek (port, scm_from_int64 (*pos), scm_from_int (whence));
  *pos = scm_to_int64 (new_pos);

  return PORT_OK;
}

static int
port_close (void *cookie)
{
  SCM port = PTR2SCM (cookie);

  scm_close_port (port);

  return PORT_OK;
}


/* Create a new terminal whose inputs and output are Guile ports */
SCM
gucu_newterm (SCM type, SCM outp, SCM inp)
{
  char *c_type;
  SCREEN *ret;

  /* IMPORTANT! (10/09/2014) Curses's newterm requires that the output
     FILE * is based on a true file.  In the _nc_setupscreen function
     called by newterm, it uses fileno() to extract the file number
     from the output port.

     (That's why this newterm didn't work with the old code based on
     fopencookie().  fopencookie() ports don't necessarily have file
     descriptors.) */

  /* Will throw exception if not a file port. */
  SCM outp_fileno = scm_fileno (outp);
  int c_outp_fileno_orig = scm_to_int (outp_fileno);
  int c_outp_fileno = dup (c_outp_fileno_orig);
  FILE *c_outp = fdopen(c_outp_fileno, "wb+");
  if (c_outp == NULL)
    return scm_from_int (2);

  SCM inp_fileno = scm_fileno (inp);
  int c_inp_fileno_orig = scm_to_int (inp_fileno);
  int c_inp_fileno = dup (c_inp_fileno_orig);
  FILE *c_inp = fdopen(c_inp_fileno, "rb");

  if (c_inp == NULL)
    return scm_from_int (1);

  /* N.B.: Since we've duplicated these file descriptors, we need
     to close the ports on which the originals were based, so that
     there aren't competing reads and writes to those files. */
  scm_close (scm_from_int (c_inp_fileno_orig));
  scm_close (scm_from_int (c_outp_fileno_orig));

  c_type = scm_to_locale_string (type);

  ret = newterm (c_type, c_outp, c_inp);
  free (c_type);
  if (ret == NULL)
    return scm_from_int (3);

  SCM s_ret = _scm_from_screen_and_ports (ret, c_outp, c_inp);

  return s_ret;
}

#endif

/* Create a window based on data saved by putwin */
SCM
gucu_getwin (SCM port)
{
  FILE *fp;
  WINDOW *c_win;
  SCM s_c;
  unsigned char c;

  SCM_ASSERT (scm_is_true (scm_input_port_p (port)), port, SCM_ARG1,
              "getwin");

#ifdef GUCU_USE_COOKIE
  fp = fopencookie (&port, "rb", port_funcs);

  if (fp == NULL)
    return SCM_BOOL_F;

  c_win = getwin (fp);

  fclose (fp);
#else
  /* Read all of the data in the port and cache it as a temp file */
  fp = tmpfile ();
  while (TRUE)
    {
      s_c = scm_read_char (port);

      if (scm_is_true (scm_eof_object_p (s_c)))
        break;

      c = scm_to_uint8 (scm_char_to_integer (s_c));

      if (fwrite (&c, sizeof (char), 1, fp) != 1)
        {
          scm_syserror ("getwin");
        }
    }
  rewind (fp);

  /* Create a window fromt the contents of the port */
  c_win = getwin (fp);
  fclose (fp);
#endif

  if (c_win == NULL)
    return SCM_BOOL_F;

  return _scm_from_window (c_win);
}


/* Writes all of the data associated with a window to a given port */
SCM
gucu_putwin (SCM win, SCM port)
{
  WINDOW *c_win;
  FILE *fp;
  int ret;

  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG1, "putwin");
  SCM_ASSERT (scm_is_true (scm_output_port_p (port)), port, SCM_ARG2,
              "putwin");

  c_win = _scm_to_window (win);

#ifdef GUCU_USE_COOKIE
  {
    char *debug_str;
    size_t debug_len;

    fp = fopencookie (&port, "wb", port_funcs);

    if (fp == NULL)
      return SCM_BOOL_F;

    ret = putwin (c_win, fp);

    if (ret == ERR)
      return SCM_BOOL_F;

    debug_str =
      scm_to_locale_stringn (scm_get_output_string (port), &debug_len);
    scm_display (scm_from_size_t (debug_len), scm_current_output_port ());

    /* The string is not closed here, so that its contents can be read */
    /* fclose (fp); */
  }
#else
  fp = tmpfile ();
  ret = putwin (c_win, fp);
  if (ret == OK)
    {
      char c;
      rewind (fp);
      while (fread (&c, sizeof (char), 1, fp) == 1)
        {
          scm_write_char (SCM_MAKE_CHAR (c), port);
        }
    }
  fclose (fp);
#endif

  if (ret == ERR)
    return SCM_BOOL_F;

  return SCM_BOOL_T;
}

void
gucu_init_port ()
{
  static int first = 1;

  if (first)
    {
#ifdef GUCU_USE_COOKIE
      port_funcs.read = port_read;
      port_funcs.write = port_write;
      port_funcs.seek = port_seek;
      port_funcs.close = port_close;
#endif
      scm_c_define_gsubr ("%newterm", 3, 0, 0, gucu_newterm);
      scm_c_define_gsubr ("getwin", 1, 0, 0, gucu_getwin);
      scm_c_define_gsubr ("putwin", 2, 0, 0, gucu_putwin);
      first = 0;
    }
}
