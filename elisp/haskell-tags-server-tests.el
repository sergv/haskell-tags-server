;; haskell-tags-server-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Tuesday, 11 October 2016
;; Description:

(require 'bert)
(require 'ert)
(require 'haskell-tags-server)

(defmacro with-decoder (send-data on-decode &rest body)
  (declare (indent 2))
  (let ((decoder-var '#:decoder))
    `(let* ((,decoder-var
             (haskell-tags-server--make-bert-decoder
              ,on-decode))
            (,send-data (lambda (data) (funcall ,decoder-var data))))
       ,@body)))

(ert-deftest haskell-tags-server-tests/decode-full-response-at-once ()
  (let* ((message "FOOBAR")
         (packed-message (bert-pack message)))
    (with-decoder send-data
        (lambda (decoded)
          (should (equal decoded packed-message))
          (should (equal message (bert-unpack decoded))))
      (funcall send-data packed-message))))

(ert-deftest haskell-tags-server-tests/decode-chunked-response ()
  (let* ((message (vector 'foo "BAR" 123 (list 4 5 6 7 8 9 10)))
         (packed-message (bert-pack message)))
    (loop
      for k from 1 to (length packed-message)
      do
      (with-decoder send-data
          (lambda (decoded)
            (should (equal decoded packed-message))
            (should (equal message (bert-unpack decoded))))
        (loop
          for i from 0 to (length packed-message) by k
          do
          (funcall send-data (subseq packed-message
                                     i
                                     (min (length packed-message) (+ i k)))))))))

(progn
  (ert "haskell-tags-server-tests/.*")
  nil)

;; Local Variables:
;; End:

;; haskell-tags-server-tests.el ends here
