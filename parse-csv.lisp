#!/usr/bin/sbcl --script

; parse-csv.lisp


;; |===============| PARSING |===============|

; file production
(defun file (csv-file)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (if (header line)
        (cheer "INFO: header is well formed.")
        (die "ERR: Error while parsing header."))
      (let ((parse-fail (records csv-file 1)))
        (if parse-fail
          (die
            "ERR: Error while parsing records at "
            (car parse-fail)
            (cdr parse-fail))
          (cheer "SUCCESS: File is well formed!"))))))
; end file

; header production 
(defun header (line)
  (let ((cursor (record line 0)))
    (when (and cursor (>= cursor (length line)))
      t)))
; end header

; records production
(defun records (csv-file line-count)
  (let ((line (read-line csv-file nil)))
    (cond
      ((null line) nil)
      ((= (length line) 0) nil)
      ((< (record line 0) (length line)) (cons (1+ line-count) (record line 0)))
      (t (records csv-file (1+ line-count))))))
; end records

; record (implements both 'fields' and 'names' productions)
(defun record (line cursor)
  (if (null cursor)
      nil
      (let ((after-field (field line cursor)))
        (cond
          ((null after-field) nil)
          ((>= after-field (length line)) after-field)
          (t (let ((after-comma (comma line after-field)))
               (if (and after-comma (< after-comma (length line)))
                   (record line after-comma)
                   after-field)))))))
; end record

; field (parses a token whether its enclosed or not)
(defun field (line cursor) 
  (or (enclosed-textdata line cursor) 
      (textdata line cursor)))
; end field

; enclosed textdata production
(defun enclosed-textdata (line cursor)
  (when (= (char-code (char line cursor)) #x22)
    (let ((after-textdata (textdata line (1+ cursor))))
      (when (and
              (< after-textdata (length line))
              (= (char-code (char line after-textdata)) #x22))
        (1+ after-textdata)))))
; end enclosed textdata

; textdata production
(defun textdata (line cursor)
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
          (textdata line (1+ cursor))
          cursor)))))
; end textdata

; comma production
(defun comma (line cursor)
  (when (= (char-code (char line cursor)) #x2C)
    (1+ cursor)))
; end comma

;; |===============| PARSING |===============|

;; |===============| HELPERS |===============|

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

; die
(defun die (msg line column)
  (format t "~a[~d:~d]~%" msg line column)
  (sb-ext:exit :code 1))
; end die

; cheer
(defun cheer (msg) (format t "~a~%" msg))
; end cheer

;; |===============| END HELPERS |===============|

(main sb-ext:*posix-argv*)

; end parse-csv.lisp
