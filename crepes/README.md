Crepes - Course REPort Excellence System
========================================

A simple tool to keep track of time spent on various projects.
It's mainly intended as a tool for doctoral students to keep track of their
teaching hours, but is hopefully general enough to be useful in other contexts
as well.

All data is kept in SQLite format (stored in ~/.crepes/timereport.sqlite) so
it's easy to run your own reports over it if the ones provided out of the box
don't cut it. If you do, please consider sending in a patch to enable Crepes
to generate your report out of the box - if it's useful to you it will probably
be useful to someone else as well.

Another benefit of the SQLite format is that if you find Crepes lacking,
migrating to something better is trivial.

Usage
=====

Crepes may be used in REPL mode or directly from the command line. The commands
you may issue are the same in both modes. To create a project, add some
categories, log some time and view a report, you can either use the
command line:

    $ crepes create myproject
    $ crepes newcat planning
    $ crepes newcat hacking
    $ crepes myproject 5 planning
    $ crepes myproject 10 hacking
    $ crepes myproject 10
    $ crepes report

...or the REPL mode:

    $ crepes
    crepes REPL 0.1
    Type 'help' for help, 'quit' to exit.
    > newcat planning
    > newcat hacking
    > myproject 5 planning
    > myproject 10 hacking
    > myproject 10
    > report

The two modes are obviously equivalent, with REPL mode being slightly more
convenient if you're doing a bunch of operations in one go.
