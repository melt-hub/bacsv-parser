#!/usr/bin/sbcl --script
; parse-csv.lisp

;; |===============| DYNAMIC TABLE |===============|

(defparameter *tables* (make-hash-table :test #'equal))

(defun new-table (table-id)
  (or (gethash table-id *tables*)
    (setf (gethash table-id *tables*)
      (new-dvector 'table-id))))

(defun new-dvector (vector-id &optional (initial-capacity 1024))
  (list
    'dynamic-vector
    vector-id
    initial-capacity
    0
    (make-array initial-capacity :adjustable t)))

(defun dvector-id (vector-rep) (second vector-rep))

(defun dvector-capacity (vector-rep)
  (when vector-rep (third vector-rep)))

(defun dvector-size (vector-rep)
  (when vector-rep (fourth vector-rep)))

(defun dvector-actual-vector (vector-rep)
  (when vector-rep (fifth vector-rep)))

(defun grow (vector-rep)
  (when vector-rep
    (let* ((old-capacity (dvector-capacity vector-rep))
            (old-vector (dvector-actual-vector vector-rep))
            (new-capacity (* old-capacity 2))
            (new-vector
              (adjust-array old-vector new-capacity :initial-element nil)))
      (setf (third vector-rep) new-capacity)
      (setf (fifth vector-rep) new-vector))))

(defun dvector-append (vector-rep element)
  (when vector-rep
    (let ((actual-vector (dvector-actual-vector vector-rep))
           (current-size (dvector-size vector-rep))
           (current-capacity (dvector-capacity vector-rep)))
      (when (= current-size current-capacity)
        (grow vector-rep))
      (setf
        (aref (dvector-actual-vector vector-rep) current-size) element)
      (setf (fourth vector-rep) (1+ current-size)))))

(defun dvector-print (vector-rep &optional (index 0))
  (when (< index (dvector-size vector-rep))
    (format t "~a: ~a~%" index (aref (dvector-actual-vector vector-rep) index))
    (dvector-print vector-rep (1+ index))))

;; |===============| END DYNAMIC TABLE |===============|

;; |===============| PARSING |===============|

; file production
(defun file (csv-file)
  (let ((line (read-line csv-file nil)))
    (unless (null line)
      (new-table 'melt)
      (if (header line 'melt)
        (cheer "INFO: header is well formed.")
        (die "ERR: Error while parsing header." 0 0))
      (let ((parse-fail (records csv-file 1 'melt)))
        (if parse-fail
          (die
            "ERR: Error while parsing records at "
            (first parse-fail)
            (second parse-fail))
          (cheer "SUCCESS: File is well formed!"))))))
; end file

; header production 
(defun header (line table-id)
  (let ((cursor (record line 0 table-id)))
    (when (and cursor (>= cursor (length line)))
      t)))
; end header

; records production
(defun records (csv-file line-count table-id)
  (let ((line (read-line csv-file nil)))
    (cond
      ((null line) nil)
      ((= (length line) 0) nil)
      ((< (record line 0 table-id) (length line))
        (list (1+ line-count) (record line 0 table-id)))
      (t (records csv-file (1+ line-count) table-id)))))
; end records

; record (implements both 'fields' and 'names' productions)
(defun record (line cursor table-id)
  (if (null cursor)
      nil
      (let ((after-field (field line cursor table-id)))
        (cond
          ((null after-field) nil)
          ((= after-field (length line)) after-field)
          (t (let ((after-comma (comma line after-field)))
               (if (and after-comma (< after-comma (length line)))
                   (record line after-comma table-id)
                   after-field)))))))
; end record

; field (parses a token whether its enclosed or not)
(defun field (line cursor table-id)
  (let ((vector-rep (gethash table-id *tables*)))
    (or (enclosed-textdata line cursor vector-rep) 
        (textdata line cursor cursor vector-rep))))
; end field

; enclosed textdata production
(defun enclosed-textdata (line cursor vector-rep)
  (when (= (char-code (char line cursor)) #x22)
    (let ((after-textdata (textdata line (1+ cursor) (1+ cursor) vector-rep)))
      (when (and
              (< after-textdata (length line))
              (= (char-code (char line after-textdata)) #x22))
        (1+ after-textdata)))))
; end enclosed textdata

; textdata production
(defun textdata (line token-start token-end vector-rep)
  (cond
    ((null token-end) token-end)
    ((>= token-end (length line))
      (dvector-append vector-rep (subseq line token-start token-end))
      token-end)
    (t
      (let ((curr-char-code (char-code (char line token-end))))
        (cond
          ((or
            (and (>= curr-char-code #x20) (<= curr-char-code #x21))
            (and (>= curr-char-code #x23) (<= curr-char-code #x2B))
            (and (>= curr-char-code #x2D) (<= curr-char-code #x7E)))
          (textdata line token-start (1+ token-end) vector-rep))
          (t (dvector-append vector-rep (subseq line token-start token-end))
            token-end))))))
; end textdata 

; comma production
(defun comma (line cursor)
  (when (= (char-code (char line cursor)) #x2C)
    (1+ cursor)))
; end comma

;; |===============| END PARSING |===============|

;; |===============| HELPERS |===============|

; usage
(defun usage () (die "Usage: parse-csv.lisp FILE" 0 0))
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
