#!/usr/bin/sbcl --script

(defun print-lines (f)
 (let ((line (read-line f nil)))
   (unless (null line)
     (format t "~a~%" line)
     (print-lines f))))

(defun prod-field (word)
  (let ((code (char-code (char word 0)))
        (rest (subseq word 1)))
    (if (and (>= code 32) ; Space.
             (<= code 126) ; Tilde.
             (not (= code 44))) ; Comma.
        (prod-field rest)
        t)))

(defun prod-record (word)
  )

(defun parse-record (str)
  (format t "~a~%" str))

(defun parse-records (file)
  (let ((line (read-line file nil)))
    (unless (null line)
      (format t "~a~%" (prod-field line))
      (parse-records file))))

(defun parse-csv (fname)
  (with-open-file (f fname)
    (parse-records f)))

(defun usage ()
  (format t "Usage: parse-csv.lisp FILE~%"))

(defun main (args)
  (if (= (length args) 2)
      (parse-csv (cadr args))
      (usage)))

;; Only works with SBCL.
(main *posix-argv*)
