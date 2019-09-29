(ql:quickload :dexador)  ;; HTTPクライアント
(ql:quickload :plump)   ;; HTMLパーサ
(ql:quickload :clss)   ;; CSSセレクタ
(ql:quickload :cxml)
(ql:quickload :cxml-stp)
(ql:quickload :xpath)

;; usage
;; (french-japanese-conv "in-test.txt" "output.csv")
;; -> generate output.csv

(defconstant *const-match-loop-max* 10)

(defparameter *japanese-french-dic-url* "http://9.dee.cc/~hakase2/tokuken.php")

(defparameter *simplewordbook-xml* nil)

(defun get-japanese (target-word)
  (let* ((result-html (dex:post *japanese-french-dic-url*
                                :content `(("mado" . ,target-word)
                                           ("erab" . "tango")
                                           ("ktype" . "1"))))
         (parse-tree (plump:parse result-html))
         (aref-index -1)
         (meaning-tree)
         (meaning-text ""))
    (setq aref-index (get-matched-index (clss:select "span.midcs" parse-tree) target-word))
    (setq meaning-tree (clss:select "p.yakcs" parse-tree))
    (if (and (>  (length meaning-tree) 0) (>= aref-index 0))
        (progn
          (progn
            (setq meaning-tree (aref meaning-tree aref-index))
            (setq meaning-text (plump:text meaning-tree))
            (setq meaning-text (substitute #\、 #\, meaning-text)))))
    meaning-text
    ))

(defun get-matched-index (word-tree target-word)
  (let ((index -1)
        (loop-max 0))
    (if (> (length word-tree) *const-match-loop-max*)
        (setq loop-max *const-match-loop-max*)
        (setq loop-max (length word-tree)))
    (loop for ii from 0 to (- loop-max 1) do
      (if (string= (plump:text (aref word-tree ii)) target-word)
          (progn
            (setq index ii)
            (return))))
    index))

(defun read-filter-setting()
  (setf *simplewordbook-xml* (cxml:parse-file "./settings/simplewordbook.xml" (stp:make-builder))))

(defun is-filter-out-word (french-word)
  (let ((filter-out nil))
    (xpath:do-node-set
     (word (xpath:evaluate "/simplewordbook/wordbook/word" *simplewordbook-xml*))
     (if (string= french-word (xpath:string-value (xpath:evaluate "@word" word)))
         (if (not (string= "" (xpath:string-value (xpath:evaluate "@memorizedAt" word))))
             (setq filter-out t))))
    filter-out))

(defun french-japanese-conv (in-file out-file)
  (let ((translated)
        (french-word))
    (read-filter-setting)
    (with-open-file (in-stream in-file)
    (with-open-file (out-stream out-file :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf8)
      (with-open-file (no-match-stream (concatenate 'string "no-match-" in-file ) :direction :output :if-exists :supersede :external-format :utf8)
        (loop for line = (read-line in-stream nil nil)
              while line
              do (progn
                   (setq french-word (string-right-trim '(#\Return #\Newline) line))
                   (if (not (is-filter-out-word french-word))
                       (progn
                         (setq translated (get-japanese french-word))
                         (if (not (string=  translated ""))
                             (format out-stream "~A,~A~%" french-word translated)
                           (format no-match-stream "~A~%" french-word))
                         (sleep 1))))))))))
