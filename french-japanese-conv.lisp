(ql:quickload :dexador)  ;; HTTPクライアント
(ql:quickload :plump)   ;; HTMLパーサ
(ql:quickload :clss)   ;; CSSセレクタ

;; usage
;; (french-japanese-conv "in-test.txt")
;; -> generate output.csv

(defparameter *japanese-french-dic-url* "http://9.dee.cc/~hakase2/tokuken.php")

(defun get-japanese (french-word)
  (let* ((result-html (dex:post *japanese-french-dic-url*
                                :content `(("mado" . ,french-word)
                                           ("erab" . "tango")
                                           ("ktype" . "1"))))
         (parse-tree (plump:parse result-html))
         (sub-tree)
         (sub-tree-text))
    (setq sub-tree (clss:select "p.yakcs" parse-tree))
    (if (<=  (length sub-tree) 0)
        (setq sub-tree-text "")
        (progn
          (setq sub-tree (aref sub-tree 0))
          (setq sub-tree-text (plump:text sub-tree))
          (setq sub-tree-text (substitute #\、 #\, sub-tree-text))))
    sub-tree-text
    ))

(defun french-japanese-conv (in-file)
  (with-open-file (in-stream in-file)
    (with-open-file (out-stream "output.csv" :direction :output :if-exists :supersede :external-format :utf8)
      (loop for line = (read-line in-stream nil nil)
            while line
            do (progn
                 (setq line (string-right-trim '(#\Return #\Newline) line))
                 (format out-stream "~A,~A~%" line (get-japanese line))
                 (sleep 1))))))
