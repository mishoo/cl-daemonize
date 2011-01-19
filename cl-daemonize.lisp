;;;; cl-daemonize.lisp

(in-package #:cl-daemonize)

#-sbcl (error "This is SBCL specific")

(export '(daemonize
          restart-process))

(defparameter *the-output* "/dev/null")
(defparameter *the-error* "/dev/null")

(defun close-all-files ()
  (loop :for fd :from 3 :to 32768 :do
     (ignore-errors
       (sb-posix:close fd))))

(defun exec (program args)
  ;; thanks to JDz
  (cffi:foreign-funcall "execve"
                        :string program
                        :pointer (cffi:foreign-alloc :string
                                                     :initial-contents `(,program ,@args)
                                                     :null-terminated-p t)
                        :pointer (cffi:foreign-alloc :string
                                                     :initial-contents (posix-environ)
                                                     :null-terminated-p t)
                        :int))

(defun restart-process ()
  "Restarts the currently running program, passing the same command
line arguments.  Stopping the process is done by sending a SIGTERM to
ourselves."
  (let ((executable sb-ext:*runtime-pathname*)
        (args (cdr sb-ext:*posix-argv*)))
    (format t "*** RESTARTING ~A~%" executable)
    (labels ((restart ()
               (dolist (port swank::*listener-sockets*)
                 (ignore-errors
                   (swank:stop-server port)))
               (dolist (thread (remove sb-thread:*current-thread* (sb-thread:list-all-threads)))
                 (ignore-errors
                   (sb-thread:terminate-thread thread)
                   (sb-thread:join-thread thread)))
               (close-all-files)
               (exec (namestring executable) args)))
      (push #'restart *exit-hooks*))
    ;; we're expected to have a TERM handler that does any necessary
    ;; cleanups and exits with (sb-ext:quit).  This in turn will call
    ;; the exit hook that we pushed above.
    (sb-posix:kill (sb-posix:getpid) sb-posix:sigterm)))

;;; most of the code below is from restas-daemon
;;; (c) Andrey Moskvitin, http://archimag-dev.blogspot.com/
;;; https://github.com/archimag/restas/blob/master/contrib/restas-daemon.lisp
;;; released under the GNU LGPL.

(unless (boundp 'sb-unix:tiocnotty)
  (defconstant sb-unix:tiocnotty 21538))

;;;; required path for sbcl :(
(sb-posix::define-call "grantpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "ptsname" c-string null (fd sb-posix::file-descriptor))
(sb-posix::define-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))

(defun global-error-handler (condition x)
  (declare (ignore x))
  (let ((err (with-output-to-string (out)
               (let ((*print-escape* nil))
                 (print-object condition out)))))
    (print err *error-output*)
    ;; (format out "~A" (trivial-backtrace:print-backtrace condition :output *error-output*))
    (sb-posix:syslog sb-posix:log-err err))
  (quit :unix-status 1))

(defun switch-to-slave-pseudo-terminal (&optional (out *the-output*) (err *the-error*))
  (flet ((c-bit-or (&rest args)
           (reduce #'(lambda (x y) (boole boole-ior x y))
                   args)))
    (let* ((fdm (sb-posix:open #P"/dev/ptmx" sb-posix:O-RDWR))
           (slavename (progn
                        (sb-posix::grantpt fdm)
                        (sb-posix::unlockpt fdm)
                        (sb-posix::ptsname fdm)))
           (fds (sb-posix:open slavename sb-posix:O-RDONLY))
           (out-fd (sb-posix:open out
                                  (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-APPEND)
                                  (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH)))
           (err-fd (if (not (equal err out))
                       (sb-posix:open err
                                      (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-APPEND)
                                      (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH))
                       (if out (sb-posix:dup out-fd)))))
      (sb-posix:dup2 fds 0)
      (sb-posix:dup2 out-fd 1)
      (sb-posix:dup2 err-fd 2))))

(defun daemonize (&key (out *the-output*) (err *the-error*) stop pid)
  "Daemonize the current process.  Redirects output into `out', error
output into `err'.  Sets up the following signal handlers:

- TERM, INT -- call the handler you pass in `stop'
- USR1 -- call `restart-process' (see below)

If you want this function to save the daemon PID into a file, pass the
file name in `pid'.  If that's the case, it will also care to remove
the file when the process is stopped."
  (setf *debugger-hook* #'global-error-handler)
  (sb-posix:chdir #P"/")
  (sb-posix:umask 0)
  (let ((fd (ignore-errors (sb-posix:open #P"/dev/tty" sb-posix:O-RDWR))))
    (when fd
      (sb-posix:ioctl fd sb-unix::tiocnotty)
      (sb-posix:close fd)))
  (switch-to-slave-pseudo-terminal out err)
  (unless (= (sb-posix:fork) 0)
    (sb-ext:quit :unix-status 0))
  (sb-posix:setsid)
  (flet ((stop (&rest args)
           (if stop
               (apply stop args)
               (sb-ext:quit))))
    (sb-sys:enable-interrupt sb-posix:sigterm #'stop)
    (sb-sys:enable-interrupt sb-posix:sigint #'stop))
  (sb-sys:enable-interrupt sb-posix:sigusr1 (lambda (&rest args)
                                              (declare (ignore args))
                                              (restart-process)))
  (when pid
    (with-open-file (out pid
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "~A" (sb-posix:getpid)))
    (push (lambda ()
            (ignore-errors
              (delete-file pid)))
          *exit-hooks*)))
