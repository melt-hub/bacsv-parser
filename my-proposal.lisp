; my-proposal.lisp

; reading test
(defun reading-test (csv-file)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (format t "reading: ~a~%" line)
      (reading-test csv-file))))
; end reading test

; ===============| HEADER PARSING |===============

; header profuction 
(defun header (line)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (names line))))
; end header production

; names production
(defun names (line cursor)
  (let ((offset-a (name line cursor)))
    (when offset-a 
      (or 
        (let ((offset-b (comma line offset-a)))
          (names line offset-b))
        (clrf line offset-b)))))
; end names production

; name production
(defun name (line cursor) 
  (or (field line cursor) 
      (enclosed-field line cursor)))
; end name production

; comma production
(defun comma (line cursor) 
  (let* ((curr-char (char line cursor))
         (curr-ascii (char-code curr-char))
         (rest-of-line (1+ cursor))))
    (if (= curr-ascii #x2C)
      rest-of-line
      -1))
; end comma production

; clrf production
(defun clrf (line cursor) 
  (let* ((curr-char (char line cursor))
         (nxt-char (char line (1+ cursor)))
         (curr-ascii (char-code curr-char))
         (nxt-ascii (char-code nxt-char))
         (rest-of-line (+ cursor 2))))
    (if (and (= curr-ascii #x0D)
             (= nxt-ascii #x0A))
      rest-of-line
      -1))
; end clrf production

; ===============| END HEADER PARSING |===============

; ===============| RECORDS PARSING |===============

; records production
(defun records (csv-file) t)
; end recors production

; record production
(defun record (line cursor) t)
; end recor production

; fields production
(defun fields (line cursor) t)
; end fields production

; enclosed fields production
(defun enclosed-fields (line cursor) t)
; end enclosed fields production

; field production
(defun field (line cursor) t)
; end field production

; enclosed field production
(defun enclosed-field (line cursor) t)
; end enclosed field production

; word production
(defun word (line cursor) t)
; end word production

; ===============| RECORDS PARSING |===============

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
