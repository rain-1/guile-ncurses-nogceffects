/*
  visibility.h

  Copyright 2010 Free Software Foundation, Inc.

  This file is part of Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#ifndef VISIBILITY_H
#define VISIBILITY_H

#if defined _WIN32 || defined __CYGWIN__
#define GUCU_HELPER_DLL_IMPORT __declspec(dllimport)
#define GUCU_HELPER_DLL_EXPORT __declspec(dllexport)
#define GUCU_HELPER_DLL_LOCAL
#else
#if __GNUC__ >= 4
#define GUCU_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
#define GUCU_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
#define GUCU_HELPER_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#else
#define GUCU_HELPER_DLL_IMPORT
#define GUCU_HELPER_DLL_EXPORT
#define GUCU_HELPER_DLL_LOCAL
#endif
#endif

#ifdef GUCU_DLL		 /* defined if GUCU is compiled as a DLL */
#ifdef GUCU_DLL_EXPORTS	 /* defined if we are building the GUCU DLL */
#define GUCU_API GUCU_HELPER_DLL_EXPORT
#else
#define GUCU_API GUCU_HELPER_DLL_IMPORT
#endif /* GUCU_DLL_EXPORTS */
#define GUCU_LOCAL GUCU_HELPER_DLL_LOCAL
#else /* GUCU_DLL is not defined: this means GUCU is a static lib. */
#define GUCU_API
#define GUCU_LOCAL
#endif

#endif
