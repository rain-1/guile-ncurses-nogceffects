if DEMO_INSTALL
bin_SCRIPTS = gucu-keyboard-test
else
noinst_SCRIPTS = gucu-keyboard-test
endif

bin_PROGRAMS = gucushell
gucushell_SOURCES = shell.c
gucushell_LDADD = @CURSES_LIB@ -lutil -lguile

EXTRA_DIST = uninstalled-env.in gucu-keyboard-test gucu-vi
