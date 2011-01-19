A tool to daemonize a Lisp process without the need for screen/detachtty.

All is SBCL-specific and Linux-only (should work on any POSIX in fact).

Most of the meat in this package comes from restas-daemon [1] which is
copyright Moskvitin Andrey <archimag@gmail.com> and distributed under the
LGPL.

[1] https://github.com/archimag/restas/blob/master/contrib/restas-daemon.lisp

* API - it exports two functions:

** (daemonize &key out err stop pid)

    Detach from the current terminal and run as a daemon.  Redirects
    standard output and standard error to `out' and `err' (which default to
    "/dev/null").  If you pass `stop', it must be a function that will be
    called when the process is terminated (SIGTERM).  Optionally saves the
    PID of the daemon process if you pass `pid' (a file name or a
    pathname).  If so, an exit hook is pushed that will remove the pid file
    when the process ends.

    It installs handlers for the following signals:

    - SIGTERM, SIGINT -- remove the `pid' file, then call the `stop'
      handler.  Note that we don't force the process to quit -- if you
      define a `stop' handler, you are expected to call sb-ext:quit there.
      If there is no stop handler, this calls sb-ext:quit.

    - SIGUSR1 -- call restart-process.

** (restart-process)

    Restarts the current process.  The way this is handled, it pushes a
    function to *exit-hooks* that does the following:

    - stop swank (yes, this assumes that SWANK is running...  some cleanup
      would be in order I guess)

    - terminate all threads except the running one

    - close any open files (XXX: the way this is done is a bit brute)

    - execve itself, passing the same arguments and environment.

    Then it sends SIGTERM to our own PID.  So this is the workflow: process
    gets SIGTERM.  The stop handler that you defined in `daemonize' will run
    and does all that is necessary to cleanly stop the process, then calls
    `sb-ext:quit'.  This triggers the *exit-hooks*, which do the above.

    There are chances that the exit hook that removes the PID file doesn't
    get executed, but that's okay because the file will be overwritten as we
    restart.

* Practical usage

The main thread still needs to do something even if you call (daemonize).
Otherwise, if the main thread exits, then the process terminates.  I do it
like this:

    (defparameter *finished* nil)

    (daemonize :out "output.log"
               :err "error.log"
               :pid "my.pid"
               :stop (lambda (&rest args)
                       (declare (ignore args))
                       (setf *finished* t)))

    (start-web-server)

    (loop :do
      (sleep 1)
      (when *finished*
        (stop-web-server)
        (sb-ext:quit)))

The only safe thing we can do in a signal handler is set a flag, so that's
what `stop' does.  The main thread periodically checks for that flag and
stops the process with `sb-ext:quit'.
