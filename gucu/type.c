#define _GNU_SOURCE
#include <assert.h>
#include <curses.h>
#include <errno.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>

#include <config.h>

#include "type.h"
#include "unicode.h"

scm_t_bits screen_tag;
scm_t_bits window_tag;

SCM equalp_window (SCM x1, SCM x2);
size_t free_window (SCM x);
SCM mark_window (SCM x);
int print_window (SCM x, SCM port, scm_print_state *pstate);

int print_screen (SCM x, SCM port, scm_print_state *pstate);

// attr -- character attributes, bit flags packed into an unsigned:
// probably uint32

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


///////////////////////////////////
// CHARACTERS

// xchar: a wide character with possible combining characters and its
// associated renditions.  In C, a cchar_t struct.  In scheme, a list
// where element 0 is the attributes, element 1 is the color pair, and
// the rest of the list is the code points of the character and its
// accents

// Returns #t if x is a Guile charater, or a list containing an attribute, 
// a color pair number, and a 1 to 5 codepoints.
int
_scm_is_schar_or_xchar (SCM x)
{
  int i, len;

  if (SCM_CHARP (x))
    return 1;

  if (!scm_is_true (scm_list_p (x)))
    return 0;

  len = scm_to_int (scm_length (x));

  if (len > 2 + CCHARW_MAX
      || !_scm_is_attr (scm_list_ref (x, scm_from_int (0)))
      || !scm_is_integer (scm_list_ref (x, scm_from_int (1))))
    return 0;

  for (i = 2; i < len; i ++)
    if (!SCM_CHARP (scm_list_ref (x, scm_from_int (i))))
      return 0;

  return 1;
}

int
_scm_is_xchar (SCM x)
{
  int i, len;

  if (!scm_is_true (scm_list_p (x)))
    return 0;

  len = scm_to_int (scm_length (x));

  if (len > 2 + CCHARW_MAX
      || !_scm_is_attr (scm_list_ref (x, scm_from_int (0)))
      || !scm_is_integer (scm_list_ref (x, scm_from_int (1))))
    return 0;

  for (i = 2; i < len; i ++)
    if (!SCM_CHARP (scm_list_ref (x, scm_from_int (i))))
      return 0;

  return 1;
}



#ifdef HAVE_LIBNCURSESW
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

  len = getcchar(x, 0, 0, 0, 0);
  ret = getcchar(x, wch, &attr, &color_pair, NULL);
  
  assert (ret != ERR);
  
  // Strip the color info from attr
  attr &= A_ATTRIBUTES ^ A_COLOR;

  total_list = scm_list_2 (_scm_from_attr (attr), scm_from_short (color_pair));

  for (i=0; i<len; i++)
    {
      element = SCM_MAKE_CHAR (wch[i]);
      element_list = scm_list_1 (element);
      total_list = scm_append (scm_list_2 (total_list, element_list));
    }

  return total_list;
}
#endif

// char -- in C a signed 8-bit locale-encoded char.  In Scheme, a char.
// Returns #f if CH is an 8-bit character in a 7-bit locale or if
// character conversion fails for some obscure reason.
SCM
_scm_schar_from_char (char c)
{
  int ret;
  uint32_t cp;

  ret = locale_char_to_codepoint (c, &cp);
  if (!ret)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
}

// chtype -- in C an unsigned 8-bit locale-encoded char with color and
// attribute bits.  In Scheme, a complex char.  Returns #f if CH
// is an 8-bit character in a 7-bit locale or if character conversion fails
// for some obscure reason.
SCM
_scm_xchar_from_chtype (chtype ch)
{
  char c_ch;
  attr_t c_attrs;
  short c_color_pair;
  int ret;
  uint32_t cp;

  c_ch = (char) (ch & A_CHARTEXT);
  c_attrs = (attr_t) (ch & A_ATTRIBUTES);
  c_color_pair = (short) PAIR_NUMBER (ch);
  printf ("chtype %d\n", ch);
  printf ("chartext %d\n", ch & A_CHARTEXT);
  printf ("attrs %d\n", ch & A_ATTRIBUTES);
  printf ("color %d\n", PAIR_NUMBER (ch));

  if (c_attrs | A_ALTCHARSET)
    {
      cp = c_ch;
    }
  else
    {
      ret = locale_char_to_codepoint (c_ch, &cp);
      if (!ret)
        return SCM_BOOL_F;
    }

  return scm_list_3 (_scm_from_attr (c_attrs),
                     scm_from_short (c_color_pair),
                     SCM_MAKE_CHAR (cp));
}

// wchar -- in C, wchar_t.  In Scheme, a char.
SCM
_scm_schar_from_wchar (wchar_t ch)
{
  int ret;
  uint32_t cp;
  
  ret = wchar_to_codepoint (ch, &cp);
  if (!ret)
    return SCM_BOOL_F;

  return SCM_MAKE_CHAR (cp);
}

#ifdef HAVE_LIBNCURSESW
cchar_t *
_scm_schar_or_xchar_to_cchar (SCM x)
{
  int i;
  attr_t attr = A_NORMAL;
  short color_pair = 0;
  wchar_t wch[CCHARW_MAX+1], wc;
  cchar_t *cchar;

  cchar = (cchar_t *) scm_malloc (sizeof (cchar_t));

  if (SCM_CHARP (x))
    {
      uint32_t codepoint = SCM_CHAR (x);
      if (!codepoint_to_wchar (codepoint, &wc))
        wc = GUCU_REPLACEMENT_WCHAR;
      wch[0] = wc;
      wch[1] = L'\0';
    }
  else if (scm_is_true (scm_list_p (x)))
    {
      SCM member;			
      int len, ret;
      uint32_t codepoint;

      len = scm_to_int (scm_length (x));
      member = scm_list_ref (x, scm_from_int (0));
      attr = _scm_to_attr (member);
      member = scm_list_ref (x, scm_from_int (1));
      color_pair = scm_to_short (member);

      for (i=2; i<len; i++)
        {
          member = scm_list_ref (x, scm_from_int (i));
          codepoint = SCM_CHAR (member);
          ret = codepoint_to_wchar (codepoint, &wc);
          if (ret)
            wch[i-2] = wc;
          else
            wch[i-2] = GUCU_REPLACEMENT_WCHAR;
        }
      wch[len-2] = L'\0';
    }

  if (OK != setcchar(cchar, wch, attr, color_pair, NULL))
    return (cchar_t *) NULL;

  return cchar;
}

cchar_t *
_scm_xchar_to_cchar (SCM x)
{
  int i;
  SCM member;
  uint32_t codepoint;
  int ret;
  wchar_t wch[CCHARW_MAX+1];
  wchar_t wc;

  cchar_t *cchar = (cchar_t *) scm_malloc (sizeof (cchar_t));
  int len = scm_to_int (scm_length (x));
  attr_t attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  short color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));

  for (i=2; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      codepoint = SCM_CHAR (member);
      ret = codepoint_to_wchar (codepoint, &wc);
      if (ret)
        wch[i-2] = wc;
      else
        wch[i-2] = GUCU_REPLACEMENT_WCHAR;
    }
  wch[len-2] = L'\0';

  if (OK != setcchar(cchar, wch, attr, color_pair, NULL))
    return (cchar_t *) NULL;

  return cchar;
}

#endif

char
_scm_schar_or_xchar_to_char (SCM x)
{
  int ret;
  char c;
  uint32_t codepoint;

  if (SCM_CHARP (x))
    codepoint = SCM_CHAR (x);
  else 
    codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (3)));

  ret = codepoint_to_locale_char (codepoint, &c);
  if (!ret)
    c = GUCU_REPLACEMENT_CHAR;

  return c;
}
      
chtype
_scm_schar_or_xchar_to_chtype (SCM x)
{
  int ret;
  chtype ch;

  if (SCM_CHARP (x))
    {
      char c;
      uint32_t codepoint;
      codepoint = SCM_CHAR (x);
      ret = codepoint_to_locale_char (SCM_CHAR (x), &c);
      if (ret)
        ch = c;
      else
        ch = GUCU_REPLACEMENT_CHAR;
    }
  else
    {
      char c;
      attr_t attr;
      short color_pair;
      uint32_t codepoint;
      codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (3)));
      ret = codepoint_to_locale_char (SCM_CHAR (scm_list_ref (x, scm_from_int (3))), &c);
      if (ret)
        ch = c;
      else
        ch = GUCU_REPLACEMENT_CHAR;
      attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
      color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
      ch = ch & attr & COLOR_PAIR (color_pair);
    }
  return ch;
}

chtype
_scm_xchar_to_chtype (SCM x)
{
  int ret;
  chtype ch;
  char c;
  attr_t attr;
  short color_pair;
  uint32_t codepoint;

  codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (3)));
  ret = codepoint_to_locale_char (SCM_CHAR (scm_list_ref (x, scm_from_int (3))), &c);
  if (!ret)
    ch = (chtype) (unsigned char) c;
  else
    ch = (chtype) (unsigned char) GUCU_REPLACEMENT_CHAR;
  attr = _scm_to_attr (scm_list_ref (x, scm_from_int (0)));
  color_pair = scm_to_short (scm_list_ref (x, scm_from_int (1)));
  ch = ch & attr & COLOR_PAIR (color_pair);

  return ch;
}

wchar_t
_scm_schar_or_xchar_to_wchar (SCM x)
{  
  int ret;
  wchar_t c;
  uint32_t codepoint;

  if (SCM_CHARP (x))
    codepoint = SCM_CHAR (x);
  else
    codepoint = SCM_CHAR (scm_list_ref (x, scm_from_int (3)));

  ret = codepoint_to_wchar (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_WCHAR;

  return c;
}

wchar_t
_scm_schar_to_wchar (SCM x)
{  
  int ret;
  wchar_t c;
  uint32_t codepoint;

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_wchar (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_WCHAR;

  return c;
}

wchar_t
_scm_schar_to_char (SCM x)
{  
  int ret;
  char c;
  uint32_t codepoint;

  codepoint = SCM_CHAR (x);
  ret = codepoint_to_locale_char (codepoint, &c);
  if (!ret)
    return GUCU_REPLACEMENT_CHAR;

  return c;
}

///////////////////////////////////
// STRINGS

// Guile strings are either standard strings, a list of chars, or a 
// list of cchars

int
_scm_is_sstring_or_xstring (SCM x)
{
  if (scm_is_string (x))
    return 1;

  if (scm_is_true (scm_list_p (x)))
    {
      int i, len;
      
      len = scm_to_int (scm_length (x));
      for (i = 0; i < len; i ++)
        {
          if (!_scm_is_schar_or_xchar (scm_list_ref (x, scm_from_int (i))))
            return 0;
        }
    }
  return 1;
}

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
cchar_t *
_scm_sstring_or_xstring_to_cstring (SCM x)
{
  int i, len;
  SCM member;			
  cchar_t *cstring;
  cchar_t *cchar;
  cchar_t terminator;
  wchar_t wch;
  
  len = scm_to_int (scm_length (x));
  cstring = (cchar_t *) scm_malloc (sizeof(cchar_t) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      cchar = _scm_schar_or_xchar_to_cchar (member);
      memcpy(cstring+i, cchar, sizeof(cchar_t));
      free(cchar);
    }

  wch = L'\0';

  setcchar(&terminator, &wch, A_NORMAL, 0, NULL);
  memcpy(cstring+len, &terminator, sizeof(cchar_t));
  
  return cstring;
}


cchar_t *
_scm_xstring_to_cstring (SCM x)
{
  int i, len;
  SCM member;			
  cchar_t *cstring;
  cchar_t *cchar;
  cchar_t terminator;
  wchar_t wch;
  
  len = scm_to_int (scm_length (x));
  cstring = (cchar_t *) scm_malloc (sizeof(cchar_t) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      cchar = _scm_xchar_to_cchar (member);
      memcpy(cstring+i, cchar, sizeof(cchar_t));
      free(cchar);
    }

  wch = L'\0';

  setcchar(&terminator, &wch, A_NORMAL, 0, NULL);
  memcpy(cstring+len, &terminator, sizeof(cchar_t));
  
  return cstring;
}

SCM
_scm_xstring_from_cstring (cchar_t *x)
{
  int i, len, n;
  SCM member, xstring;
  wchar_t wch[CCHARW_MAX];
  attr_t attrs;
  short color_pair;
  
  xstring = SCM_EOL;
  i = 0;
  while (1)
    {
      n = getcchar(&(x[i]), NULL, &attrs, &color_pair, NULL);
      if (n == 0)
        break;
      getcchar(&(x[i]), wch, &attrs, &color_pair, NULL);
      if (n == 2)
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
        member = scm_list_3 (_scm_from_attr (attrs),
                             scm_from_short (color_pair),
                             _scm_schar_from_wchar (wch[0]));

      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
      i ++;
    }

  return xstring;
}

#endif

wchar_t *
_scm_sstring_or_xstring_to_wstring (SCM x)
{
  int i, len;
  SCM member;			
  wchar_t *wstring;
  wchar_t wchar;
  
  len = scm_to_int (scm_length (x));
  wstring = (wchar_t *) scm_malloc (sizeof(wchar_t) * (len + 1));

  for (i=0; i<len; i++)
    {
      member = scm_list_ref (x, scm_from_int (i));
      wchar = _scm_schar_or_xchar_to_wchar (member);
      memcpy(wstring+i, &wchar, sizeof(wchar_t));
    }
  wstring[len] = L'\0';
  
  return wstring;
}

wchar_t *
_scm_sstring_to_wstring (SCM x)
{
  size_t i, len;
  SCM member;			
  wchar_t *wstring;
  wchar_t wchar;
  
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
_scm_sstring_from_wstring (wchar_t *x)
{
  int i;
  SCM member, xstring;
  
  xstring = SCM_EOL;
  for (i = 0; i < wcslen(x); i++)
    {
      member = _scm_schar_from_wchar (x[i]);
      xstring = scm_append (scm_list_2 (xstring, scm_list_1 (member)));
    }

  return scm_string (xstring);
}

SCM
_scm_sstring_from_wint_string (wint_t *x)
{
  int i, len;
  SCM member, xstring;
  
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



chtype *
_scm_xstring_to_chstring (SCM x)
{
  int i, len;
  SCM member;			
  chtype *chstring;
  chtype ch;
  
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
print_screen (SCM x, SCM port, scm_print_state *pstate __attribute__ ((unused)))
{
  SCREEN *screen = (SCREEN *) SCM_SMOB_DATA (x);
  char *str;


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



// voidstring -- In C, (void *)NULL.  In Scheme, doesn't matter.  This
// is because all of the (void *) arguments in the directly wrapped
// list are unused by curses.

int
_scm_is_voidstring (SCM x __attribute__ ((unused)))
{
  return 1;
}

void *
_scm_to_voidstring (SCM x __attribute__ ((unused)))
{
  return NULL;
}

SCM
_scm_from_voidstring (void *x __attribute ((unused)))
{
  return SCM_EOL;
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
  WINDOW *win1 = (WINDOW *) SCM_SMOB_DATA (x1);
  WINDOW *win2 = (WINDOW *) SCM_SMOB_DATA (x2);

  if ((win1 == NULL) || (win2 == NULL))
    return SCM_BOOL_F;
  else if ((win1 != win2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_window (SCM x __attribute__ ((unused)))
{
  // No SCMs in the window type: nothing to do here.
  return (SCM_BOOL_F);
}

size_t
free_window (SCM x)
{
  WINDOW *win = (WINDOW *) SCM_SMOB_DATA (x);

  /* Windows should already be null if delwin has been called on them */
  if (win != NULL)
    {
      if (win == stdscr)
	{
	  endwin ();
	  fprintf (stderr, "Freeing stdscr #<window %p>", (void *) stdscr);
	  delwin (stdscr);
	  SCM_SET_SMOB_DATA (x, NULL);
	}
      else
	{
	  /* This is going to break something */
	  delwin (win);
	  SCM_SET_SMOB_DATA (x, NULL);
	}
    }

  return 0;
}

int
print_window (SCM x, SCM port, scm_print_state *pstate __attribute__ ((unused)))
{
  WINDOW *win = (WINDOW *) SCM_SMOB_DATA (x);
  char *str;

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
      
      first = 0;
    }
}
