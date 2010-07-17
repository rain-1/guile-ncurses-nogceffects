#if defined HAVE_FOPENCOOKIE && defined HAVE_COOKIE_IO_FUNCTIONS_T && defined HAVE_OFF64_T
#define GUCU_USE_COOKIE
#endif

#ifdef GUCU_USE_COOKIE
#define _GNU_SOURCE
#define _LARGEFILE64_SOURCE
#endif

#include <config.h>

#include <curses.h>
#include <libguile.h>
#include <stdio.h>
#include <unistd.h>

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
  FILE *c_inp, *c_outp;
  SCREEN *ret;

  SCM_ASSERT (scm_is_string (type), type, SCM_ARG1, "newterm");
  SCM_ASSERT (scm_is_true (scm_output_port_p (outp)), outp, SCM_ARG2,
	      "newterm");
  SCM_ASSERT (scm_is_true (scm_input_port_p (inp)), inp, SCM_ARG3, "newterm");

  /* Convert the input port to a special stream */
  c_inp = fopencookie (SCM2PTR (inp), "rb", port_funcs);
  if (c_inp == NULL)
    scm_syserror ("newterm");

  /* Convert the output port to a special stream */
  c_outp = fopencookie (SCM2PTR (outp), "w", port_funcs);
  if (c_outp == NULL)
    scm_out_of_range ("newterm", outp);

  c_type = scm_to_locale_string (type);
  ret = newterm (c_type, c_outp, c_inp);
  free (c_type);
  if (ret == NULL)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ("newterm"),
		     scm_from_locale_string ("could not create a terminal"),
		     SCM_BOOL_F, SCM_BOOL_F);
    }

  SCM s_ret = _scm_from_screen (ret);

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

      scm_c_define_gsubr ("newterm", 3, 0, 0, gucu_newterm);
#endif
      scm_c_define_gsubr ("getwin", 1, 0, 0, gucu_getwin);
      scm_c_define_gsubr ("putwin", 2, 0, 0, gucu_putwin);
      first = 0;
    }
}
