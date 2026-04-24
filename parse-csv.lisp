#!/usr/bin/sbcl --script

; parse-csv.lisp

; die
(defun die (msg)
  (format t "~a~%" msg)
  (sb-ext:exit :code 1))
; end die

; ===============| HEADER PARSING |===============

; header production 
(defun header (line)
  (let ((end-cursor (names line 0)))
    (when (and end-cursor (>= end-cursor (length line)))
      t)))
; end header production

; names production
(defun names (line cursor)
  (if (null cursor)
      nil
      (let ((after-name (name line cursor)))
        (cond
          ((null after-name) nil)
          ((>= after-name (length line)) after-name)
          (t (let ((after-comma (comma line after-name)))
               (if (and after-comma (< after-comma (length line)))
                   (names line after-comma)
                   after-name)))))))
; end names production

; name production
(defun name (line cursor) 
  (or (enclosed-field line cursor) 
      (field line cursor)))
; end name production

; comma production
(defun comma (line cursor)
  (when (= (char-code (char line cursor)) #x2C)
    (1+ cursor)))
; end comma production

; ===============| END HEADER PARSING |===============

; ===============| RECORDS PARSING |===============

; records production
(defun records (csv-file) 
  t)
; end recors production

; record production
; (defun record (line cursor) t)
; end recor production

; fields production
(defun fields (line cursor) 1)
; end fields production

; enclosed fields production
(defun enclosed-fields (line cursor) 1)
; end enclosed fields production

; field production
(defun field (line cursor)
  (cond
    ((null cursor) cursor)
    ((>= cursor (length line)) cursor)
    (t
      (let ((curr-char-code (char-code (char line cursor))))
        (if
          (or
            (and (>= curr-char-code #x20) (<= curr-char-code #x21))
            (and (>= curr-char-code #x23) (<= curr-char-code #x2B))
            (and (>= curr-char-code #x2D) (<= curr-char-code #x7E)))
          (field line (1+ cursor))
          cursor)))))
; end field production

; enclosed field production
(defun enclosed-field (line cursor)
  (when (= (char-code (char line cursor)) #x22)
    (let ((after-field (field line (1+ cursor))))
      (when (and
              (< after-field (length line))
              (= (char-code (char line after-field)) #x22))
        (1+ after-field)))))
; end enclosed field production

; word production
; (defun word (line cursor) t)
; end word production

; ===============| END RECORDS PARSING |===============

; file production
(defun file (csv-file)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (if (and (header line) (records csv-file))
        t
        (die "ERR: Error while parsing.")))))
; end file production

; usage
(defun usage () (die "Usage: parse-csv.lisp FILE"))
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

; end parse-csv.lisp
 
