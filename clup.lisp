;;;; audit-utils.lisp

(in-package #:clup)

;;; "audit-utils" goes here. Hacks and glory await!

;; the passwd and group files have the same structure for the fields
;; we're concerned about in this use, so there's no need to define
;; separate functions to handle each record type.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

(defun quit (&optional code)
  "Taken from the cliki"
  ;; This group from "clocc-port/ext.lisp"
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:exit :code code)
  ;; This group from Maxima
  #+kcl (lisp::bye)                         ; XXX Does this take an arg?
  #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
  #+(or openmcl mcl) (ccl::quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)
  ;; This group from <hebi...@math.uni.wroc.pl>
  #+poplog (poplog::bye)                    ; XXX Does this take an arg?
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
        kcl scl openmcl mcl abcl ecl)
  (error 'not-implemented :proc (list 'quit code)))

(defun wc-l (file)
  (let ((buffer-size 10000))
    (with-open-file (stream file :element-type '(unsigned-byte 8))
      (let ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
            (count 0))
        (multiple-value-bind (i partial)
            (floor (file-length stream) buffer-size)
          (dotimes (j i)
            (read-sequence buffer stream)
            (incf count (count 10 buffer)))
          (read-sequence buffer stream :end partial)
          (incf count (count 10 buffer :end partial)))
        count))))

(defparameter *empty-lines* 0
  "Hold the number of observed blank lines for the input file under
  consideration.")


;; if we remove the empty subseqs here, we clean the file quite
;; nicely, but we end up creating a file whose size does not match the
;; size of the input, and we fail our sanity test on a file containing
;; blank lines.
;; :remove-empty-subseqs t
(defun read-pfile (&key (pfile "/etc/passwd"))
  "read the password file and return a list of strings for each line
   in the file."
  (let* ((plist (split-sequence #\newline (rutils:read-file pfile)))
         (blanks (length (loop for item in plist
                               if (string= item "")
                                 collect item)))
         (plist-noblanks (loop for item in plist
                               unless (string= item "")
                                 collect item))) 
    (setf *empty-lines* blanks) ;; number of newlines as value in bytes.
    (values plist-noblanks blanks)))

(defun read-gfile (&key (gfile "/etc/group"))
  "read the group file and return a list of strings for each line
   in the file."
  (read-pfile :pfile gfile))

(defun chop-pwf-line (passwd-lines)
  "chop a password/group file line into a list of colon delimited fields."
  (let* ((pieces-list (loop for line in passwd-lines
                            collect (split-sequence #\: line)))
         ;; collected parts look like this => '(65534 ("nogroup" "x" "65534" ""))
         (ranked-list (loop for pl in pieces-list
                            collect (list (parse-integer (third pl))
                                          pl)))) 
    (sort ranked-list #'< :key #'car)))

;; forex: (sorted-pw-lines (chop-pwf-line (read-pfile))) -> list of
;; lists.
(defun sorted-pw-lines (chopped-lines)
  (loop for cline in  chopped-lines
        collect (second cline)))

(defun reassemble-chopped-pwline (chopped-line)
  (rutil:strjoin #\: chopped-line))

(defun output-sorted-pwfile (&key (infile "/etc/passwd")
                               (outfile (format nil "/tmp/~A.sorted" (file-namestring infile))))
  (with-open-file (s outfile :direction :output :if-exists :supersede)
    (loop for mug in
                  (sorted-pw-lines
                   (chop-pwf-line (read-pfile :pfile infile)))
          do (format s "~A~&" (reassemble-chopped-pwline mug)))))

(defun output-sorted-gfile (&key (infile "/etc/group")
                              (outfile (format nil "/tmp/~A.sorted" (file-namestring infile))))
  (output-sorted-pwfile :infile infile :outfile outfile))

(defun f-length (file)
  ;; just give me the length of a file without having to do the
  ;; w-o-f rigmarole.
  (with-open-file (s file)
    (file-length s)))

(defun do-sort-pass (&key (inf "/etc/passwd"))
  "enter rat's nest of side effects... "
  (when inf
    (output-sorted-pwfile :infile inf))
  (assert (uiop:file-exists-p inf))
  (let* ((ouf (format nil "/tmp/~A.sorted" (file-namestring inf))) ;; sorted output file
         (original-file-length (f-length inf))
         (backup (format nil "/tmp/~A.presort.~A" (file-namestring inf) (get-universal-time))) ;; copy of the input file
         (new-file-length (f-length ouf)))
    (if (or (= original-file-length new-file-length)
            *empty-lines*)
        (handler-case
            (progn
              (cond
                ;;; if the file sizes don't match in length, but we have found empty lines:
                ((and (/= original-file-length new-file-length) *empty-lines*)
                 (format t "Sizes of original and sorted files do not match,~% but the file contained blank lines. Backing up original ~a file into ~a ... "
                         inf backup)
                 (cl-fad:copy-file inf backup :overwrite t)
                 (format t "[Done]~%")
                 (format t "Moving cleaned and sorted ~A file into its original place ... " inf)
                 (cl-fad:copy-file ouf inf :overwrite t)
                 (format t "[Done]~%"))
                ;;; if the file sizes match, just do it 
                ((= original-file-length new-file-length)
                 (format t "Sizes of original and sorted files match... ~% backing up original ~A file into ~A ... "
                         inf backup)
                 (cl-fad:copy-file inf backup :overwrite t)
                 (format t "[Done]~%")
                 (format t "Moving cleaned and sorted ~A file into its original place ... " inf)
                 (cl-fad:copy-file ouf inf :overwrite t)
                 (format t "[Done]~%"))
                (t
                 (error "This shouldn't happen, but we know from experience that sparrow likes whiskey."))))
          (file-error (c)
            (format t "~&~%A file error has occured. You likely don't have permission to write the ~A file.~%The error returned was:~%~A~%" inf c)))

        (format t "Sorted passwd file is not the same length as the original:~% ~D bytes vs. ~D bytes" new-file-length original-file-length))))


;; follows, CLON business.
(defsynopsis (:postfix "FILES ... ")
  (text :contents "A tool for auditing/sorting password and group files.")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit"))
  ;; (group (:HEADER "Sort the password file by UID")
  ;;        (path :short-name "p" :long-name "password"
  ;;              :description "run against the system password file: /etc/passwd"
  ;;              :argument-name "PASSWORD-FILE"
  ;;              :default-value (lambda () "/etc/passwd")))
  ;; (group (:HEADER "Sort the group file by GID")
  ;;        (path :short-name "g" :long-name "group"
  ;;              :description "run against the system group file: /etc/group"
  ;;              :argument-name "GROUP-FILE"
  ;;              :default-value (lambda () "/etc/passwd")))
  )

(defun muppet (argv)
  "Entry point for passwd auditing tool."
  ;; (declare (ignorable argv))
  (make-context)
  (format t "~& ~D ARGV :: ~{~A~^ ~}~%" (length argv) argv)
  (let ((possible-path (make-pathname :defaults (second argv))))
    (format t "~&OPath: ~A~%~%" possible-path)
    (if possible-path
        (clup:do-sort-pass :inf possible-path))))

;; ;;; sort the password file

;; (defpackage #:sort-passwd
;;   (:use #:cl #:audit-utils)
;;   (:export #:main))

;; (in-package :sort-passwd)

;; (defun main (argv)
;;   (declare (ignorable argv))
;;   (format t "~&ARGV:: ~{~A~^ ~}~%~%" argv)
;;   (let ((possible-path (make-pathname :defaults (second argv))))
;;     (format t "OPath: ~A" possible-path)
;;     (if possible-path
;;         (audit-utils:do-sort-pass :inf possible-path))))

;; ;;; sort the group file

;; (defpackage #:sort-group
;;   (:use #:cl #:audit-utils)
;;   (:export #:main))

;; (in-package :sort-group)

;; (defun main (argv)
;;   (declare (ignorable argv))
;;   (format t "~&ARGV:: ~{~A~^ ~}~%~%" argv)
;;   (let ((possible-path (make-pathname :defaults (second argv))))
;;     (format t "OPath: ~A" possible-path)
;;     (if possible-path
;;         (audit-utils:do-sort-group :inf possible-path))))


(defun -main (argv &optional args)
  "Entry point for passwd auditing tool."
  ;; (declare (ignorable argv))
  (make-context)
  (let ((possible-path (make-pathname :defaults (second argv))))
    (format t "~&Target: ~A~%~%" possible-path)
    (if (uiop:file-exists-p possible-path)
        (clup:do-sort-pass :inf possible-path))))

(defun disable-debugger ()
  (labels
      ((exit (c h)
         (declare (ignore h))
         (format t "~a~%" c)
         (quit)))
    (setf *debugger-hook* #'exit)))
