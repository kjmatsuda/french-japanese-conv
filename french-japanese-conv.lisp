(ql:quickload :dexador)  ;; HTTPクライアント
(ql:quickload :plump)   ;; HTMLパーサ
(ql:quickload :clss)   ;; CSSセレクタ

;; usage
;; (french-japanese-conv "in-test.txt" "output.csv")
;; -> generate output.csv

(defparameter *japanese-french-dic-url* "http://9.dee.cc/~hakase2/tokuken.php")

(defun get-japanese (target-word)
  (let* ((result-html (dex:post *japanese-french-dic-url*
                                :content `(("mado" . ,target-word)
                                           ("erab" . "tango")
                                           ("ktype" . "1"))))
         (parse-tree (plump:parse result-html))
         (word-tree)
         (meaning-tree)
         (meaning-text ""))
    (setq word-tree (clss:select "span.midcs" parse-tree))
    (setq meaning-tree (clss:select "p.yakcs" parse-tree))
    (if (and (>  (length meaning-tree) 0) (>  (length meaning-tree) 0))
        (progn
          (setq word-tree (aref word-tree 0))
          (if (string= target-word (plump:text word-tree))
              (progn
                (setq meaning-tree (aref meaning-tree 0))
                (setq meaning-text (plump:text meaning-tree))
                (setq meaning-text (substitute #\、 #\, meaning-text))))))
    meaning-text
    ))

(defun french-japanese-conv (in-file out-file)
  (let ((translated)
        (french-word))
  (with-open-file (in-stream in-file)
    (with-open-file (out-stream out-file :direction :output :if-exists :append :if-does-not-exist :create :external-format :utf8)
      (with-open-file (no-match-stream (concatenate 'string "no-match-" in-file ) :direction :output :if-exists :supersede :external-format :utf8)
        (loop for line = (read-line in-stream nil nil)
              while line
              do (progn
                   (setq french-word (string-right-trim '(#\Return #\Newline) line))
                   (setq translated (get-japanese french-word))
                   (if (not (string=  translated ""))
                       (format out-stream "~A,~A~%" french-word translated)
                       (format no-match-stream "~A~%" french-word))
                   (sleep 1))))))))









