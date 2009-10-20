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
          || 
          (sizeof (uint16_t) == sizeof (wchar_t) && codepoint <= 0xFFFF))
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
