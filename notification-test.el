;;; notification-test.el -- Minimal test for pixbuf-to-png ; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'notification-test-data)
(require 'eosd-pixbuf)
(require 'cl)

(cl-destructuring-bind (w h rs alpha bps ch imgdata) (car notification-test-data-value)
  (let* ((thedata0 (apply #'unibyte-string imgdata))
         (thedata1 (string-to-unibyte thedata0))
         (thedata2 (eosd-pixbuf-to-png w h rs (if alpha 1 0) bps thedata1))
         (theimage (create-image thedata2 'png t)))
    (insert-image theimage)))

(provide 'notification-test)
;;; notification-test.el ends here
