if DEMO_INSTALL
bin_SCRIPTS = gucu-keyboard-test
else
noinst_SCRIPTS = gucu-keyboard-test
endif

bin_PROGRAMS = gucushell
gucushell_SOURCES = shell.c
AM_CPPFLAGS = $(GUILE_CFLAGS)
if WARN
AM_CPPFLAGS += 	-Wall -Wextra -Wstrict-prototypes			\
-Wmissing-prototypes -Wpointer-arith -Wreturn-type -Wcast-qual		\
-Wswitch -Wshadow -Wcast-align -Wwrite-strings -Wchar-subscripts	\
-Winline -Wnested-externs -Wredundant-decls -Wunused -Wformat=2
endif
if WIDE
AM_CPPFLAGS += -D_XOPEN_SOURCE_EXTENDED
endif

LDADD = @CURSES_LIB@ -lutil @GUILE_LDFLAGS@

EXTRA_DIST = uninstalled-env.in gucu-keyboard-test gucu-vi
