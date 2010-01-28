#define _GNU_SOURCE
#include <assert.h>
#include <curses.h>
#include <errno.h>
#include <libguile.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include <config.h>

#include "type.h"
#include "unicode.h"
#include "compat.h"
#include "gucuconfig.h"

/* The maximum number of characters in a complex character */
#ifdef HAVE_LIBNCURSESW
#define GUCU_CCHARW_MAX (CCHARW_MAX)
#else
#define GUCU_CCHARW_MAX (5)
#endif

static scm_t_bits screen_tag;
static scm_t_bits window_tag;

SCM equalp_window (SCM x1, SCM x2);
size_t free_window (SCM x);
SCM mark_window (SCM x);
int print_window (SCM x, SCM port, scm_print_state *pstate);

int print_screen (SCM x, SCM port, scm_print_state *pstate);

/* attr -- character attributes, bit flags packed into an unsigned:
   probably uint32 */

int
_scm_is_attr (SCM x)
{
  return scm_is_integer (x);
}

attr_t
_scm_to_attr (SCM x)
{
  if (SIZEOF_INT == SIZEOF_ATTR_T)
    return (attr_t) scm_to_uint (x);
  else if (SIZEOF_LONG == SIZEOF_ATTR_T)
    return (attr_t) scm_to_ulong (x);
  else
    abort ();
}

SCM
_scm_from_attr (attr_t x)
{
  if (SIZEOF_INT == SIZEOF_ATTR_T)
    return scm_from_uint (x);
  else if (SIZEOF_LONG == SIZEOF_ATTR_T)
    return scm_from_ulong (x);
  else
    abort ();
}


/*
  CHARACTERS

  xchar: a wide character with possible combining characters and its
  associated renditions.  In C, a cchar_t struct.  In scheme, a list
  where element 0 is the attributes, element 1 is the color pair, and
  the rest of the list is the code points of the character and its
  accents.
*/

int
_scm_is_xchar (SCM x)
{
  int i, len;

  if (!scm_is_true (scm_list_p (x)))
    return 0;

  len = scm_to_int (scm_length (x));

  if (len > 2 + GUCU_CCHARW_MAX
      || !_scm_is_attr (scm_list_ref (x, scm_from_int (0)))
      || !scm_is_integer (scm_list_ref (x, scm_from_int (1))))
    return 0;

  for (i = 2; i < len; i ++)
    if (!SCM_CHARP (scm_list_ref (x, scm_from_int (i))))
      return 0;

  return 1;
}

#ifdef HAVE_LIBNCURSESW
/* Converts a wide NCurses complex character structure to a GuCu
   complex character */
SCM
_scm_xchar_from_cchar (cchar_t *x)
{
  int i;
  int len;
  int ret;
  wchar_t wch[10];
  attr_t attr;
  short color_pair;
  SCM element;
  SCM element_list;
  SCM total_list = SCM_EOL;

  assert (x != NULL);

  len = getcchar(x, 0, 0, 0, 0);
  /* Starting from the patch on 2009/07/18, the length returned by
     getcchar includes the trailing NULL.  Prior to that, it did not
     include the trailing NULL. */
  if (NCURSES_VERSION_MAJOR > 5
      || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR > 7)
      || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR == 7
	  && NCURSES_VERSION_PATCH >= 20090718))
    {
      len --;
    }

  ret = getcchar(x, wch, &attr, &color_pair, NULL);

  if (ret == ERR)
    scm_misc_error (NULL, "Error unpacking complex char", SCM_EOL);

  /* Strip the color info from attr */
  attr &= A_ATTRIBUTES ^ A_COLOR;

  total_list = scm_list_2 (_scm_from_attr (attr), scm_from_short (color_pair));

  for (i=0; i<len; i++)
    {
#ifdef GUILE_CHARS_ARE_UCS4
      {
	uint32_t cp;
	ret = wchar_to_codepoint (wch[i], &cp);
	if (!ret)
	  element = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT);
	else
	  element = SCM_MAKE_CHAR (cp);
      }
#else
      {
	int b = wctob ((wint_t) wch[i]);
	if (b == EOF)
	  element = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CHAR);
	else
	  element = SCM_MAKE_CHAR ((unsigned char)b);
      }
#endif
      element_list = scm_list_1 (element);
      total_list = scm_append (scm_list_2 (total_list, element_list));
    }

  return total_list;
}
#endif

/* Converts a Curses rendered character to a GuCu complex character */
SCM
_scm_xchar_from_chtype (chtype x)
{
  unsigned char c;
  attr_t attr;
  short color_pair;
  SCM total_list = SCM_EOL;

  attr = x & (A_ATTRIBUTES ^ A_COLOR);
  color_pair = PAIR_NUMBER(x);
  c = x & A_CHARTEXT;

#ifdef GUILE_CHARS_ARE_UCS4
  {
    int ret;
    uint32_t cp;
    ret = locale_char_to_codepoint (c, &cp);
    if (!ret)
      total_list = scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
			       SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT));
    else
      total_list = scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
			       SCM_MAKE_CHAR (cp));
  }
#else
  total_list = scm_list_3 (_scm_from_attr (attr), scm_from_short (color_pair),
			   SCM_MAKE_CHAR (c));
#endif

  return total_list;

}

/* Converts an 8-bit locale-encoded character to a Guile character */
SCM
_scm_schar_from_char (char c)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t cp;

  ret = locale_char_to_codepoint (c, &cp);
  if (!ret)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
#else
  return SCM_MAKE_CHAR (c);
#endif
}

/* Converts a libc wide character to a Guile characters */
SCM
_scm_schar_from_wchar (wchar_t ch)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t cp;

  ret = wchar_to_codepoint (ch, &cp);
  if (!ret)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
#else
  int b = wctob ((wint_t) ch);
  if (b == EOF)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR ((unsigned char)b);
#endif
}

#ifdef HAVE_LIBNCURSESW
/* Converts a GuCu complex character to a wide NCurses complex character */
cchar_t *
_scm_xchar_to_cchar (SCM x)
{
  int i;
  SCM member;
  wchar_t wch[GUCU_CCHARW_MAX+1];

  cchar_t *cchar = (cchar_t *) scm_malloc (sizeof (cchar_t));
  int len = scm_to_int (scm_length (x));
  attr_t attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  short color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));

  assert (_scm_is_xchar (x));

  for (i=2; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));

#ifdef GUILE_CHARS_ARE_UCS4
      {
	int ret;
	uint32_t codepoint;
	wchar_t wc;

	codepoint = SCM_CHAR (member);
	ret = codepoint_to_wchar (codepoint, &wc);
	if (ret)
	  {
	    wch[i-2] = wc;
	  }
	else
	  {
	    wch[i-2] = GUCU_REPLACEMENT_WCHAR;
	    wch[i-1] = L'\0';
	    break;
	  }
      }
#else
      {
	wint_t wint;
	wint = btowc ((int) SCM_CHAR (member));
	if (wint == WEOF)
	  {
	    wch[i-2] = GUCU_REPLACEMENT_WCHAR;
	    wch[i-1] = L'\0';
	    break;
	  }
	else
	  {
	    wch[i-2] = (wchar_t) wint;
	  }
      }
#endif
    }
  wch[len-2] = L'\0';

  if (OK != setcchar(cchar, wch, attr, color_pair, NULL))
    {
      return (cchar_t *) NULL;
    }

  return cchar;
}
#endif

chtype
_scm_xchar_to_chtype (SCM x)
{
  chtype ch;
  attr_t attr;
  short color_pair;
  char c;

#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  uint32_t codepoint;

  assert (_scm_is_xchar (x));

  codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (2)));
  ret = codepoint_to_locale_char (codepoint, &c);
  if (ret)
    ch = (chtype) (unsigned char) c;
  else
    ch = (chtype) (unsigned char) GUCU_REPLACEMENT_CHAR;
  attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
  ch = ch | attr | COLOR_PAIR (color_pair);

#else
  assert (_scm_is_xchar (x));

  c = SCM_CHAR (scm_list_ref (x, scm_from_int (2)));
  attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
  ch = (chtype) (unsigned char) c | attr | COLOR_PAIR (color_pair);
#endif

  return ch;

}

wchar_t
_scm_schar_to_wchar (SCM x)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  wchar_t c;
  uint32_t codepoint;

  assert (SCM_CHARP (x));

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_wchar (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_WCHAR;

  return c;
#else
  wint_t wc;

  assert (SCM_CHARP (x));
  wc = btowc ((int) SCM_CHAR (x));
  if (wc == WEOF)
    return GUCU_REPLACEMENT_WCHAR;

  return (wchar_t) wc;
#endif
}

char
_scm_schar_to_char (SCM x)
{
#ifdef GUILE_CHARS_ARE_UCS4
  int ret;
  char c;
  uint32_t codepoint;

  assert (SCM_CHARP (x));

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_locale_char (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_CHAR;

  return c;
#else
  assert (SCM_CHARP (x));
  return SCM_CHAR (x);
#endif
}

SCM
gucu_schar_from_char (SCM c)
{
  int c_c;
  SCM_ASSERT (scm_is_integer (c), c, SCM_ARG1, "%scheme-char-from-c-char");
  c_c = scm_to_int (c);
  return _scm_schar_from_char ((char) (unsigned char) c_c);
}

SCM
gucu_schar_to_char (SCM c)
{
  char c_c;
  SCM_ASSERT (SCM_CHARP (c), c, SCM_ARG1, "%scheme-char-to-c-char");
  c_c = _scm_schar_to_char (c);
  return scm_from_uint ((unsigned char) c_c);
}

SCM
gucu_schar_from_wchar (SCM wc)
{
  wchar_t c_wc;
  SCM_ASSERT (scm_is_integer (wc), wc, SCM_ARG1, "%scheme-char-from-c-wchar");
  c_wc = scm_to_uint (wc);
  return _scm_schar_from_wchar (c_wc);
}

SCM
gucu_schar_to_wchar (SCM c)
{
  wchar_t c_c;
  SCM_ASSERT (SCM_CHARP (c), c, SCM_ARG1, "%scheme-char-to-c-wchar");
  c_c = _scm_schar_to_wchar (c);
  return scm_from_uint (c_c);
}

SCM
gucu_xchar_from_chtype (SCM c)
{
  SCM_ASSERT (_scm_is_chtype (c), c, SCM_ARG1, "%xchar-from-chtype");
  return _scm_xchar_from_chtype (_scm_to_chtype (c));
}

SCM
gucu_xchar_to_chtype (SCM c)
{
  SCM_ASSERT (_scm_is_xchar (c), c, SCM_ARG1, "%xchar-to-chtype");
  return _scm_from_chtype (_scm_xchar_to_chtype (c));
}

///////////////////////////////////
// STRINGS

// Guile strings are either standard strings, a list of chars, or a
// list of cchars

int
_scm_is_xstring (SCM x)
{
  if (scm_is_true (scm_list_p (x)))
    {
      int i, len;

      len = scm_to_int (scm_length (x));
      for (i = 0; i < len; i ++)
        {
          if (!_scm_is_xchar (scm_list_ref (x, scm_from_int (i))))
            return 0;
        }
    }
  return 1;
}

#ifdef HAVE_LIBNCURSESW
SCM
_scm_sstring_from_wint_string (const wint_t *x)
{
  int i, len;
  SCM member, xstring;

  assert (x != NULL);

  len = 0;
  while (x[len] != 0)
    len ++;
  xstring = SCM_EOL;
  for (i = 0; i < len; i++)
    {
      if (x[i] <= WCHAR_MAX)
        member = _scm_schar_from_wchar (x[i]);
      else
        member = SCM_MAKE_CHAR (GUCU_REPLACEMENT_CODEPOINT);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
    }

  return scm_string (xstring);
}
#endif

#ifdef HAVE_LIBNCURSESW
SCM
_scm_sstring_from_wstring (const wchar_t *x)
{
  size_t i;
  SCM member, xstring;

  assert (x != NULL);

  xstring = SCM_EOL;
  for (i = 0; i < wcslen(x); i++)
    {
      member = _scm_schar_from_wchar (x[i]);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
    }

  return scm_string (xstring);
}
#endif

char *
_scm_sstring_to_locale_string (SCM x)
{
  assert (scm_is_string (x));

  return scm_to_locale_string (x);
}

wchar_t *
_scm_sstring_to_wstring (SCM x)
{
  size_t i, len;
  SCM member;
  wchar_t *wstring;
  wchar_t wchar;

  assert (scm_is_string (x));

  len = scm_c_string_length (x);
  wstring = (wchar_t *) scm_malloc (sizeof(wchar_t) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_c_string_ref (x, i);
      wchar = _scm_schar_to_wchar (member);
      memcpy(wstring+i, &wchar, sizeof(wchar_t));
    }
  wstring[len] = L'\0';

  return wstring;
}

SCM
_scm_xstring_from_chstring (const chtype *x)
{
  size_t i;
  SCM member, xstring;

  assert (x != NULL);

  xstring = SCM_EOL;
  i = 0;
  while (1)
    {
      if (x[i] == 0)
	break;
      member = _scm_xchar_from_chtype (x[i]);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
      i ++;
    }

  return xstring;
}


#ifdef HAVE_LIBNCURSESW
SCM
_scm_xstring_from_cstring (const cchar_t *x)
{
  int i, n;
  SCM member, xstring;
  wchar_t wch[GUCU_CCHARW_MAX];
  attr_t attrs;
  short color_pair;

  assert (x != NULL);

  xstring = SCM_EOL;
  i = 0;
  while (1)
    {
      if (x[i].chars[0] == 0)
        break;
      n = getcchar(&(x[i]), NULL, NULL, NULL, NULL);
      /* Starting from the patch on 2009/07/18, the length returned by
	 getcchar includes the trailing NULL.  Prior to that, it did not
	 include the trailing NULL. */

      if (NCURSES_VERSION_MAJOR > 5
	  || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR > 7)
	  || (NCURSES_VERSION_MAJOR == 5 && NCURSES_VERSION_MINOR == 7
	      && NCURSES_VERSION_PATCH >= 20090718))
	{
	  n --;
	}

      if (n == 0)
        break;
      getcchar(&(x[i]), wch, &attrs, &color_pair, NULL);
      if (n == 1)
        member = scm_list_3 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]));

      else if (n == 2)
        member = scm_list_4 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]));
      else if (n == 3)
        member = scm_list_5 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]));
      else if (n == 4)
        member = scm_list_n (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]),
                             _scm_schar_from_wchar (wch[3]), SCM_UNDEFINED);

      else if (n == 5)
        member = scm_list_n (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]),
                             _scm_schar_from_wchar (wch[1]),
                             _scm_schar_from_wchar (wch[2]),
                             _scm_schar_from_wchar (wch[3]),
                             _scm_schar_from_wchar (wch[4]), SCM_UNDEFINED);
      else
        abort ();

      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
      i ++;
    }

  return xstring;
}
#endif

chtype *
_scm_xstring_to_chstring (SCM x)
{
  int i, len;
  SCM member;
  chtype *chstring;
  chtype ch;

  assert (_scm_is_xstring (x));

  len = scm_to_int (scm_length (x));
  chstring = (chtype *) scm_malloc (sizeof(chtype) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      ch = _scm_xchar_to_chtype (member);
      memcpy(chstring+i, &ch, sizeof(chtype));
    }
  chstring[len] = 0;

  return chstring;
}

#ifdef HAVE_LIBNCURSESW
cchar_t *
_scm_xstring_to_cstring (SCM x)
{
  int i, len;
  SCM member;
  cchar_t *cstring;
  cchar_t *cchar;
  static cchar_t terminator;
  static int first = 1;

  assert (_scm_is_xstring (x));

  if (first)
    {
      wchar_t wch = L'\0';
      setcchar(&terminator, &wch, A_NORMAL, 0, NULL);
      first = 0;
    }

  len = scm_to_int (scm_length (x));
  cstring = (cchar_t *) scm_malloc (sizeof(cchar_t) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      cchar = _scm_xchar_to_cchar (member);
      memcpy(cstring+i, cchar, sizeof(cchar_t));
      free(cchar);
    }

  memcpy(cstring+len, &terminator, sizeof(cchar_t));

  return cstring;
}

#endif



// chtype -- in C, an integer that contains a characters and its rendition.
// In Scheme, an integer.

int
_scm_is_chtype (SCM x)
{
  return scm_is_integer (x);
}

chtype
_scm_to_chtype (SCM x)
{
  assert (_scm_is_chtype (x));

  if (SIZEOF_INT == SIZEOF_CHTYPE)
    return (chtype) scm_to_uint (x);
  else if (SIZEOF_LONG == SIZEOF_CHTYPE)
    return (chtype) scm_to_ulong (x);
  else
    abort ();
}

SCM
_scm_from_chtype (chtype x)
{
  if (SIZEOF_INT == SIZEOF_CHTYPE)
    return scm_from_uint (x);
  else if (SIZEOF_LONG == SIZEOF_CHTYPE)
    return scm_from_ulong (x);
  else
    abort ();
}

// mevent -- in C, a MEVENT.  In scheme, a list of 5 elements
int
_scm_is_mevent (SCM x)
{
  int err = 0;
  SCM member;
  int i;

  if (scm_is_true (scm_list_p (x)))
    {
      int len = scm_to_int (scm_length (x));

      for (i = 0; i < len; i ++)
	{
	  member = scm_list_ref (x, scm_from_int (i));
	  if (!scm_is_integer (member))
	    err ++;
	}
    }
  err ++;

  if (err > 0)
    return 0;
  else
    return 1;
}

MEVENT *
_scm_to_mevent (SCM x)
{
  MEVENT *me;

  assert (_scm_is_mevent (x));

  me = (MEVENT *) malloc (sizeof (MEVENT));

  me->id = scm_to_short (scm_list_ref (x, scm_from_int (0)));
  me->x = scm_to_int (scm_list_ref (x, scm_from_int (1)));
  me->y = scm_to_int (scm_list_ref (x, scm_from_int (2)));
  me->z = scm_to_int (scm_list_ref (x, scm_from_int (3)));
  me->bstate = scm_to_ulong (scm_list_ref (x, scm_from_int (4)));

  return (me);
}

SCM
_scm_from_mevent (MEVENT *me)
{
  assert (me != NULL);

  return scm_list_5
    (
     scm_from_short (me->id),
     scm_from_int (me->x),
     scm_from_int (me->y),
     scm_from_int (me->z),
     scm_from_ulong (me->bstate)
     );
}

// screen -- in C, a SCREEN *.  In Scheme, a smob that contains that pointer.

int
_scm_is_screen (SCM x)
{
  if (SCM_SMOB_PREDICATE (screen_tag, x))
    {
      if (SCM_SMOB_DATA (x) != 0)
	return 1;
      else
	return 0;
    }
  else
    return 0;
}

SCREEN *
_scm_to_screen (SCM x)
{
  assert (_scm_is_screen (x));

  return (SCREEN *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_screen (SCREEN *x)
{
  SCM s_screen;

  assert (x != NULL);

  SCM_NEWSMOB (s_screen, screen_tag, x);
  return s_screen;
}

int
print_screen (SCM x, SCM port, scm_print_state *pstate)
{
  SCREEN *screen;
  char *str;

  /* Don't use _scm_is_screen in this assert, because it says freed screens aren't
     screens.  */
  assert (SCM_SMOB_PREDICATE (screen_tag, x));

  screen = (SCREEN *) SCM_SMOB_DATA (x);
  scm_puts ("#<screen ", port);

  if (screen == NULL)
    {
      scm_puts ("(freed)", port);
    }
  else
    {
      if (asprintf (&str, "%p", (void *) screen) < 0)
	scm_puts ("???", port);
      else
	scm_puts (str, port);
    }

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_screen_p (SCM x)
{
  return scm_from_bool (_scm_is_screen (x));
}

// window -- in C, a WINDOW *.  In Scheme, a smob that contains the pointer

int
_scm_is_window (SCM x)
{
  if (SCM_SMOB_PREDICATE (window_tag, x))
    {
      if (SCM_SMOB_DATA (x) != 0)
	return 1;
      else
	return 0;
    }
  else
    return 0;

}

WINDOW *
_scm_to_window (SCM x)
{
  assert (_scm_is_window (x));

  return (WINDOW *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_window (WINDOW *x)
{
  SCM s_win;

  assert (x != NULL);

  SCM_NEWSMOB (s_win, window_tag, x);

  assert (x == (WINDOW *) SCM_SMOB_DATA (s_win));

  if (0)
    {
      fprintf (stderr, "Making smob from window at %d, %d\n",
	       x->_begx, x->_begy);
    }

  return (s_win);
}

// Windows are equal if they point to the same C structure
SCM
equalp_window (SCM x1, SCM x2)
{
  WINDOW *win1, *win2;

  /* This assert is thrown if x1 or x2 are already freed, as if by
     delwin.  I'm not sure if one should be able to compare windows if
     one has been freed.  */
  assert (_scm_is_window (x1));
  assert (_scm_is_window (x2));

  win1 = (WINDOW *) SCM_SMOB_DATA (x1);
  win2 = (WINDOW *) SCM_SMOB_DATA (x2);

  if ((win1 == NULL) || (win2 == NULL))
    return SCM_BOOL_F;
  else if (win1 != win2)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_window (SCM x)
{
  // No SCMs in the window type: nothing to do here.
  return (SCM_BOOL_F);
}

size_t
free_window (SCM x)
{
  WINDOW *win;

  assert (SCM_SMOB_PREDICATE (window_tag, x));

  win = (WINDOW *) SCM_SMOB_DATA (x);
  /* Windows should already be null if delwin has been called on them */
  if (win != NULL)
    {
      if (win == stdscr)
	{
	  endwin ();
	  fprintf (stderr, "Freeing stdscr #<window %p>", (void *) stdscr);
	  delwin (stdscr);
	  SCM_SET_SMOB_DATA (x, 0);
	}
      else
	{
	  /* This is going to break something */
	  delwin (win);
	  SCM_SET_SMOB_DATA (x, 0);
	}
    }

  return 0;
}

int
print_window (SCM x, SCM port, scm_print_state *pstate)
{
  WINDOW *win = (WINDOW *) SCM_SMOB_DATA (x);
  char *str;

  assert (SCM_SMOB_PREDICATE (window_tag, x));

  scm_puts ("#<window ", port);

  if (win == 0)
    scm_puts ("(freed)", port);
  else
    {
      if (asprintf (&str, "%p", (void *) win) < 0)
	scm_puts ("???", port);
      else
	scm_puts (str, port);
    }
  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_window_p (SCM x)
{
  return scm_from_bool (_scm_is_window (x));
}


void
gucu_init_type ()
{
  static int first = 1;

  if (first)
    {
      screen_tag = scm_make_smob_type ("screen", sizeof (SCREEN *));
      scm_set_smob_print (screen_tag, print_screen);
      scm_c_define_gsubr ("screen?", 1, 0, 0, gucu_is_screen_p);

      window_tag = scm_make_smob_type ("window", sizeof (WINDOW *));
      scm_set_smob_mark (window_tag, mark_window);
      scm_set_smob_free (window_tag, free_window);
      scm_set_smob_print (window_tag, print_window);
      scm_set_smob_equalp (window_tag, equalp_window);
      scm_c_define_gsubr ("window?", 1, 0, 0, gucu_is_window_p);

      scm_c_define_gsubr ("%scheme-char-to-c-char", 1, 0, 0, gucu_schar_to_char);
      scm_c_define_gsubr ("%scheme-char-to-c-wchar", 1, 0, 0, gucu_schar_to_wchar);
      scm_c_define_gsubr ("%scheme-char-from-c-char", 1, 0, 0, gucu_schar_from_char);
      scm_c_define_gsubr ("%scheme-char-from-c-wchar", 1, 0, 0, gucu_schar_from_wchar);
      scm_c_define_gsubr ("%xchar-from-chtype", 1, 0, 0, gucu_xchar_from_chtype);
      scm_c_define_gsubr ("%xchar-to-chtype", 1, 0, 0, gucu_xchar_to_chtype);

      first = 0;
    }
}
