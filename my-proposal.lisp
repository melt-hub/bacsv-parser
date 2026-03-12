; my-proposal.lisp

; header production
(defun header (csv-file)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (format t "reading: ~a~%" line)
      (header csv-file))))
; end header production

; records production
(defun records (csv-file) t)
; end recors production

; file production
(defun file (csv-file)
  (if (and (header csv-file) (records csv-file))
    t
    nil))
; end file production

; usage
(defun usage () (format t "ERR: Usage: bacsv-parser.lisp FILE~%"))
; end usage

; main
(defun main (args)
  (cond
      ((= (length args) 2)
       (with-open-file (f (second args))
       (file f)))
    (t (usage))))
; end main

(main sb-ext:*posix-argv*)


; end my-proposal.lisp
