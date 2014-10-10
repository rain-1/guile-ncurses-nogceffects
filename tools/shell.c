/*
  shell.c

  Copyright 2009, 2010, 2014 Free Software Foundation, Inc.

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

#include <libintl.h>
#include <locale.h>

/* libc and libutil */
#include <ctype.h>
#include <locale.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

#ifdef HAVE_UTIL_H
#include <util.h>      /* openbsd: openpty; */
#endif

#ifdef HAVE_PTY_H
#include <pty.h>       /* cygwin, fedora: openpty */
#endif

#ifdef HAVE_PROCESS_H
#include <process.h>   /* cygwin: execlp; */
#endif

#include <libguile.h>  /* all: scm_shell; */

#define _(s) gettext (s)

int open_terminal (char *, int, int);
int is_unix98_pty (const char *);
int is_bsd_pty (const char *);
int is_cygwin_pty (const char *);
int is_cygwin_tty (const char *);

/* This function checks if NAME looks like a Unix98 pseudo-terminal
   device name, aka "/dev/pts/XXX".  Returns TRUE or FALSE. */
int
is_unix98_pty (const char *name)
{
  const char unix_pty_root[] = "/dev/pts/";
  const size_t unix_pty_root_length = strlen (unix_pty_root);
  size_t name_length = strlen (name);
  char const *slave_name;
  size_t i;


  name_length = strlen (name);

  if (name_length < unix_pty_root_length)
    return FALSE;

  if (strncmp (name, unix_pty_root, unix_pty_root_length) != 0)
    return FALSE;

  slave_name = name + unix_pty_root_length;
  if (strlen (slave_name) == 0)
    return FALSE;

  /* All the remaining characters in NAME should be numeric */
  for (i = 0; i < name_length - unix_pty_root_length; i++)
    if (!isdigit ((int) slave_name[i]))
      return FALSE;

  return TRUE;
}

/* This function checks if NAME looks like a BSD pseudo-terminal
   device name, aka "/dev/ttyXY".  Returns TRUE or FALSE. */
int
is_bsd_pty (const char *name)
{
  const char bsd_pty_root[] = "/dev/tty";
  const size_t bsd_pty_root_length = strlen (bsd_pty_root);
  size_t name_length = strlen (name);
  int X, Y;

  name_length = strlen (name);

  if (name_length != bsd_pty_root_length + 2)
    return FALSE;

  if (strncmp (name, bsd_pty_root, bsd_pty_root_length) != 0)
    return FALSE;

  X = name[bsd_pty_root_length];
  Y = name[bsd_pty_root_length + 1];

  return (isalnum (X) && isalnum (Y));
}

/* This function checks if NAME looks like a Cygwin serial interface
   device name, aka "/dev/ttyN".  Returns TRUE or FALSE. */
int
is_cygwin_tty (const char *name)
{
  const char cygwin_pty_root[] = "/dev/tty";
  const size_t cygwin_pty_root_length = strlen (cygwin_pty_root);
  size_t name_length = strlen (name);
  char const *slave_name;
  size_t i;

  name_length = strlen (name);

  if (name_length < cygwin_pty_root_length)
    return FALSE;

  if (strncmp (name, cygwin_pty_root, cygwin_pty_root_length) != 0)
    return FALSE;

  slave_name = name + cygwin_pty_root_length;
  if (strlen (slave_name) == 0)
    return FALSE;

  /* All the remaining characters in NAME should be numeric */
  for (i = 0; i < name_length - cygwin_pty_root_length; i++)
    if (!isdigit ((int) slave_name[i]))
      return FALSE;

  return TRUE;
}

/* This function checks if NAME looks like a Cygwin pseudo-terminal
   device name, aka "/dev/ptyN".  Returns TRUE or FALSE. */
int
is_cygwin_pty (const char *name)
{
  const char cygwin_pty_root[] = "/dev/pty";
  const size_t cygwin_pty_root_length = strlen (cygwin_pty_root);
  size_t name_length = strlen (name);
  char const *slave_name;
  size_t i;

  name_length = strlen (name);

  if (name_length < cygwin_pty_root_length)
    return FALSE;

  if (strncmp (name, cygwin_pty_root, cygwin_pty_root_length) != 0)
    return FALSE;

  slave_name = name + cygwin_pty_root_length;
  if (strlen (slave_name) == 0)
    return FALSE;

  /* All the remaining characters in NAME should be numeric */
  for (i = 0; i < name_length - cygwin_pty_root_length; i++)
    if (!isdigit ((int) slave_name[i]))
      return FALSE;

  return TRUE;
}

/* This function forks an xterm that listens to a pseudoterminal,
   PSEUDO_TERMINAL_SLAVE_NAME is the path in /dev of the pty,
   MASTER_FILE_DESCRIPTOR is the file id of the front of the pty, and
   SLAVE_FILE_DESCRIPTOR is the back of the pty.
   It returns the process id of the xterm. */
int
open_terminal (char *pseudo_terminal_slave_name,
               int master_file_descriptor,
               int slave_file_descriptor)
{
  int process_id;
  struct termios terminal_attributes;
  char *s_flag;
  const size_t unix98_offset = strlen("/dev/pts/");
  const size_t bsd_offset = strlen("/dev/tty");
  const size_t cygwin_offset = strlen("/dev/tty");

  if (is_unix98_pty (pseudo_terminal_slave_name))
  {
    asprintf (&s_flag, "-S%s/%d",
              pseudo_terminal_slave_name + unix98_offset,
              master_file_descriptor);
  }
  else if (is_bsd_pty (pseudo_terminal_slave_name))
  {
    asprintf(&s_flag, "-S%c%c%d",
             pseudo_terminal_slave_name[bsd_offset],
             pseudo_terminal_slave_name[bsd_offset+1],
             master_file_descriptor);
  }
  else if (is_cygwin_pty (pseudo_terminal_slave_name)
           || is_cygwin_tty (pseudo_terminal_slave_name))
  {
    asprintf (&s_flag, "-S%s/%d",
              pseudo_terminal_slave_name + cygwin_offset,
              master_file_descriptor);
  }
  else
  {
    fprintf (stderr, _("Unrecognized pseudo-terminal name: %s\n"),
             pseudo_terminal_slave_name);
    _exit (EXIT_FAILURE);
  }


  process_id = fork ();

  if (process_id == -1)
  {
    perror("fork");
    return 0;
  }
  else if (process_id == 0)
  {
    int ret;

    /* This is the child process */
    close (slave_file_descriptor);

    printf (_("Attemping to connect an xterm to %s\n"),
            pseudo_terminal_slave_name);
    printf (_("Calling 'xterm %s'\n"), s_flag);
    ret = execlp ("xterm", "xterm", s_flag, NULL);
    /* Should not return */
    if (ret == -1)
    {
      fprintf (stderr, "+---------------------------------------------------\n");
      fprintf (stderr, "The xterm program failed to start!\n");
      perror ("Error");
      fprintf (stderr, "Thus, this program won't display any ncurses output.\n");
      fprintf (stderr, "+---------------------------------------------------\n");
      close (master_file_descriptor);
    }
    return 0;
  }
  else
  {
    /* This is the parent process */
    close (master_file_descriptor);

    /* Set to raw mode */
    tcgetattr (slave_file_descriptor, &terminal_attributes);
#ifdef HAVE_CFMAKERAW
    cfmakeraw (&terminal_attributes);
#else
    terminal_attributes.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP
                                     |INLCR|IGNCR|ICRNL|IXON);
    terminal_attributes.c_oflag &= ~OPOST;
    terminal_attributes.c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
    terminal_attributes.c_cflag &= ~(CSIZE|PARENB);
    terminal_attributes.c_cflag |= CS8;
#endif

    tcsetattr (slave_file_descriptor, TCSANOW, &terminal_attributes);
    /* Have to wait for xterm to be created */
    sleep (1);
    /* FIXME: this is a classic race condition.  Need to rewrite
       this with proper threads and a mutex. */
    return process_id;
  }
  return 0;
}

static void
inner_main (void *data, int argc, char **argv)
{
  int master, slave;
  pid_t pid;
  char name[512], cmd[512];
  char *termtype;
  int slave_read, slave_write;
  struct termios tio;
  struct winsize win;
  int i;

  setlocale (LC_ALL, "");
  bindtextdomain (PACKAGE, LOCALEDIR);
  textdomain (PACKAGE);

  /* The command line arguments are going to be passed down to Guile,
     but, we need to check here for --version and --help. */
  for (i=1; i<argc; i++)
  {
    if (strcmp (argv[i], "--version") == 0
        || strcmp (argv[i], "-v") == 0)
    {
      printf (_("guile-ncurses-shell 0.7\n"
                "Copyright (C) 2010,2014 Free Software Foundation, Inc.\n"
                "License LGPLv3+: GNU LGPL version 3 or later <http://www.gnu.org/licenses/lgpl.html>"
                "This is free software: you are free to change and redistribute it.\n"
                "There is NO WARRANTY, to the extent permitted by law.\n"));
      return;
    }
    if (strcmp (argv[i], "--help") == 0
        || strcmp (argv[i], "-h") == 0)
    {
      printf (_("Usage: guile-ncurses-shell OPTION...\n"));
          printf (_("This program starts a Guile-Ncurses session and redirects its output into\n"
                    "a terminal window.  Any options on the command line are passed on the to\n"
                    "Guile interpreter.  Type 'guile --help' to see its command line interface.\n"));
          printf ("\n");
          printf (_("Report bugs to <%s>.\n"), PACKAGE_BUGREPORT);
          printf ("\n");
          return;
        }
    }


  /* Create a pseudo-terminal */
  if (openpty (&master, &slave, name, &tio, &win) >= 0)
    {
      /* Connect the pseudo-terminal to an xterm */
      if ((pid = open_terminal (name, master, slave)))
        {
          slave_read = dup (slave);
          slave_write = dup (slave);
          close (slave);

          /* Need to wait for xterm to be ready before trying to
             initalize curses */
          printf (_("Waiting for xterm to be ready...\n"));
          sleep (5);

          /* Set up an ncurses environment in Guile */
          scm_c_eval_string ("(set! %load-path (append %load-path (list \".\")))");
          printf (_("Loading (ncurses curses)\n"));
          scm_c_eval_string ("(use-modules (ncurses curses))");

          printf (_("Setting %%guile-ncurses-shell-read-port\n"));
          snprintf (cmd, sizeof(cmd),
                    "(define %%guile-ncurses-shell-read-port (fdopen %d \"r0\"))",
                    slave_read);
          scm_c_eval_string (cmd);
            
          printf (_("Setting %%guile-ncurses-shell-write-port\n"));
          snprintf (cmd, sizeof(cmd),
                    "(define %%guile-ncurses-shell-write-port (fdopen %d \"w0\"))",
                    slave_write);
          scm_c_eval_string (cmd);

          printf (_("Setting %%guile-ncurses-shell-stdscr to the xterm\n"));
          scm_c_eval_string ("(define %guile-ncurses-shell-screen (newterm \"xterm\" %guile-ncurses-shell-write-port %guile-ncurses-shell-read-port))");
          scm_c_eval_string ("(set-term %guile-ncurses-shell-screen)");
          scm_c_eval_string ("(define %guile-ncurses-shell-stdscr (stdscr))");
          printf (_("\nYou should define a shorter name for '%%guile-ncurses-shell-stdscr' \n"));
          printf (_("like (define mainwin %%guile-ncurses-shell-stdscr) for example\n"));
          printf (  "---------------------------------------------------------------\n\n");

          /* These all get duplicated inside of guile-ncurses. */
          close (slave_write);
          close (slave_read);

          scm_shell (argc, argv);

          kill (pid, SIGTERM);
        }
    }
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
