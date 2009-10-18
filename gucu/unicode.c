#define _UNUSED_PARAMETER_ 

#include <assert.h>
#include <langinfo.h>
#include <curses.h>
#include <stdlib.h>
#include <string.h>
#include <uniconv.h>
#include <unistr.h>

#ifdef UNICODE_TEST
#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#endif

#include "unicode.h"

#ifdef __STDC_ISO_10646__
const int stdc_iso_10646 = 1;
#else
const int stdc_iso_10646 = 0;
#endif

int
locale_char_to_codepoint (char c, uint32_t *p_codepoint)
{
  char locale_str[2];
  uint32_t *u32_str;

  if (c == '\0')
    {
      *p_codepoint = 0;
      return 1;
    }
  locale_str[0] = c;
  locale_str[1] = '\0';
  u32_str = u32_strconv_from_locale (locale_str);
  if (u32_str == NULL)
    return 0;
  if (u32_strlen (u32_str) != 1)
    {
      free (u32_str);
      return 0;
    }
  *p_codepoint = u32_str[0];
  free (u32_str);
  return 1;
}
   
int
wchar_to_codepoint (wchar_t c, uint32_t *p_codepoint)
{
  wchar_t wchar_str[2];
  uint32_t *u32_str;
  size_t u32_len = 0;

  if (stdc_iso_10646)
    {
      *p_codepoint = c;
      return 1;
    }

  wchar_str[0] = c;
  wchar_str[1] = (wchar_t) 0;
  u32_str = u32_conv_from_encoding ("WCHAR_T", 
                                    iconveh_error,
                                    (const char *) wchar_str, 
                                    sizeof(wchar_t),
                                    NULL,
                                    NULL,
                                    &u32_len);
  if (u32_str == NULL)
    {
      *p_codepoint = 0;
      return 0;
    }
  if (u32_len != 1)
    {
      *p_codepoint = 0;
      free (u32_str);
      return 0;
    }
  *p_codepoint = u32_str[0];
  free (u32_str);
  return 1;
}

int
codepoint_to_locale_char (uint32_t codepoint, char *p_c)
{
  uint32_t u32_str[2];
  char *str;
  size_t str_len = 0;
  char *enc;

  if (codepoint == 0)
    {
      *p_c = '\0';
      return 1;
    }
  u32_str[0] = codepoint;
  u32_str[1] = 0;
  enc = nl_langinfo (CODESET);
  str = u32_conv_to_encoding (enc,
                              iconveh_error,
                              u32_str,
                              1,
                              NULL,
                              NULL,
                              &str_len);
  if (str == NULL)
    return 0;
  if (str_len != 1)
    {
      *p_c = '\0';
      free (str);
      return 0;
    }
  *p_c = str[0];
  free (str);
  return 1;
}
  
int
codepoint_to_wchar (uint32_t codepoint, wchar_t *p_c)
{
  uint32_t u32_str[2];
  wchar_t *wchar_str;
  size_t wchar_len = 0;

  if (stdc_iso_10646)
    {
      if ((sizeof (uint32_t) == sizeof (wchar_t))
          || (codepoint < (1UL << (8 * sizeof (wchar_t)))))
        {
          *p_c = codepoint;
          return 1;
        }
      else
        {
          *p_c = 0;
          return 0;
        }
    }

  if (codepoint == 0)
    {
      *p_c = 0;
      return 1;
    }
  u32_str[0] = codepoint;
  u32_str[1] = 0;
  wchar_str = (wchar_t *) u32_conv_to_encoding ("WCHAR_T",
                                                iconveh_error,
                                                u32_str,
                                                1,
                                                NULL,
                                                NULL,
                                                &wchar_len);
  if (wchar_str == NULL)
    {
      *p_c = 0;
      return 0;
    }
  if (wchar_len != sizeof (wchar_t))
    {
      *p_c = 0;
      free (wchar_str);
      return 0;
    }
  *p_c = wchar_str[0];
  free (wchar_str);
  return 1;
}

int
is_surrogate (uint32_t codepoint)
{
  return codepoint >= GUCU_PRIVATE_USE_START && codepoint <= GUCU_PRIVATE_USE_END;
}

uint32_t
surrogate_to_unicode_glyph (uint32_t codepoint)
{
  chtype ch = A_ALTCHARSET | (codepoint - GUCU_PRIVATE_USE_START);

  assert (codepoint >= GUCU_PRIVATE_USE_START && codepoint <= GUCU_PRIVATE_USE_END);
      
  if (ch == ACS_ULCORNER)
    codepoint = 0x250c;
  else if (ch == ACS_LLCORNER)
    codepoint = 0x2514;
  else if (ch == ACS_URCORNER)
    codepoint = 0x2510;
  else if (ch == ACS_LRCORNER)
    codepoint = 0x2518;
  else if (ch == ACS_LTEE)
    codepoint = 0x251c;
  else if (ch == ACS_RTEE)
    codepoint = 0x2524;
  else if (ch == ACS_BTEE)
    codepoint = 0x2534;
  else if (ch == ACS_TTEE)
    codepoint = 0x252c;
  else if (ch == ACS_HLINE)
    codepoint = 0x2500;
  else if (ch == ACS_VLINE)
    codepoint = 0x2502;
  else if (ch == ACS_PLUS)
    codepoint = 0x253c;
  else if (ch == ACS_S1)
    codepoint = 0x23ba;
  else if (ch == ACS_S9)
    codepoint = 0x23bd;
  else if (ch == ACS_DIAMOND)
    codepoint = 0x25c6;
  else if (ch == ACS_CKBOARD)
    codepoint = 0x2592;
  else if (ch == ACS_DEGREE)
    codepoint = 0x00b0;
  else if (ch == ACS_PLMINUS)
    codepoint = 0x00b1;
  else if (ch == ACS_BULLET)
    codepoint = 0x00b7;
  else if (ch == ACS_LARROW)
    codepoint = 0x2190;
  else if (ch == ACS_RARROW)
    codepoint = 0x2192;
  else if (ch == ACS_DARROW)
    codepoint = 0x2193;
  else if (ch == ACS_UARROW)
    codepoint = 0x2191;
  else if (ch == ACS_BOARD)
    codepoint = 0x2592;
  else if (ch == ACS_LANTERN)
    codepoint = 0x2603;
  else if (ch == ACS_BLOCK)
    codepoint = 0x25ae;
  else if (ch == ACS_S3)
    codepoint = 0x23bb;
  else if (ch == ACS_S7)
    codepoint = 0x23bc;
  else if (ch == ACS_LEQUAL)
    codepoint = 0x2264;
  else if (ch == ACS_GEQUAL)
    codepoint = 0x2265;
  else if (ch == ACS_PI)
    codepoint = 0x03c0;
  else if (ch == ACS_NEQUAL)
    codepoint = 0x2260;
  else if (ch == ACS_STERLING)
    codepoint = 0x00a3;
  else
    codepoint = GUCU_REPLACEMENT_CODEPOINT;

  return codepoint;
}

char
surrogate_to_ascii_char (uint32_t codepoint)
{
  char c;
  chtype ch = A_ALTCHARSET | (codepoint - GUCU_PRIVATE_USE_START);

  assert (codepoint >= GUCU_PRIVATE_USE_START && codepoint <= GUCU_PRIVATE_USE_END);
      
  if (ch == ACS_ULCORNER)
    c = '+';
  else if (ch == ACS_LLCORNER)
    c = '+';
  else if (ch == ACS_URCORNER)
    c = '+';
  else if (ch == ACS_LRCORNER)
    c = '+';
  else if (ch == ACS_LTEE)
    c = '+';
  else if (ch == ACS_RTEE)
    c = '+';
  else if (ch == ACS_BTEE)
    c = '+';
  else if (ch == ACS_TTEE)
    c = '+';
  else if (ch == ACS_HLINE)
    c = '-';
  else if (ch == ACS_VLINE)
    c = '|';
  else if (ch == ACS_PLUS)
    c = '+';
  else if (ch == ACS_S1)
    c = '~';
  else if (ch == ACS_S9)
    c = '_';
  else if (ch == ACS_DIAMOND)
    c = '+';
  else if (ch == ACS_CKBOARD)
    c = ':';
  else if (ch == ACS_DEGREE)
    c = '\'';
  else if (ch == ACS_PLMINUS)
    c = '#';
  else if (ch == ACS_BULLET)
    c = 'o';
  else if (ch == ACS_LARROW)
    c = '<';
  else if (ch == ACS_RARROW)
    c = '>';
  else if (ch == ACS_DARROW)
    c = 'v';
  else if (ch == ACS_UARROW)
    c = '^';
  else if (ch == ACS_BOARD)
    c = '#';
  else if (ch == ACS_LANTERN)
    c = '#';
  else if (ch == ACS_BLOCK)
    c = '#';
  else if (ch == ACS_S3)
    c = '-';
  else if (ch == ACS_S7)
    c = '-';
  else if (ch == ACS_LEQUAL)
    c = '<';
  else if (ch == ACS_GEQUAL)
    c = '>';
  else if (ch == ACS_PI)
    c = '*';
  else if (ch == ACS_NEQUAL)
    c = '!';
  else if (ch == ACS_STERLING)
    c = 'f';
  else
    c = GUCU_REPLACEMENT_CHAR;

  return c;
}


#ifdef UNICODE_TEST
int main(int argc, char **argv)
{
  int i;
  char c;
  wchar_t wc;
  uint32_t cp;
  int ret;
  char *loc;

  loc = setlocale (LC_ALL, "");
  printf ("Current locale: %s\n\n", loc);

  printf ("locale char -> codepoint\n");
  for (i = 0; i < 256; i++)
    {
      ret = locale_char_to_codepoint ((unsigned char) i, &cp);
      if (ret)
        printf ("%d (%c) -> %u\t", i, (unsigned char) i, cp);
    }
  printf ("\n\n");

  printf ("codepoint -> locale char\n");
  for (cp = 0; cp < 256; cp ++)
    {
      ret = codepoint_to_locale_char (cp, &c);
      if (ret)
        printf ("%u -> %d (%c)\t", cp, c, (unsigned char) c);
    }
  printf ("\n\n");

  printf ("wchar -> codepoint\n");
  for (wc = 0; wc < 256; wc++)
    {
      ret = wchar_to_codepoint (wc, &cp);
      if (ret)
        printf ("%d (%lc) -> %u\t", wc, wc, cp);
    }
  printf ("\n\n");

  printf ("codepoint -> wchar\n");
  for (cp = 0; cp < 256; cp ++)
    {
      ret = codepoint_to_wchar (cp, &wc);
      if (ret)
        printf ("%u -> %d (%lc)\t", cp, wc, wc);
    }
  printf ("\n");
  
        
  return EXIT_SUCCESS;
}
#endif
