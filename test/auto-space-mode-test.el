;;; auto-space-mode-test.el --- Tests for auto-space-mode -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'auto-space-mode)

(ert-deftest auto-space-adds-space-in-ordinary-buffer ()
  (with-temp-buffer
    (insert "中a")
    (auto-space--add-space-between-cjk-and-ascii)
    (should (equal (buffer-string) "中 a"))))

(ert-deftest auto-space-adds-space-in-non-file-minibuffer ()
  (skip-unless (not (active-minibuffer-window)))
  (with-current-buffer (window-buffer (minibuffer-window))
    (let ((inhibit-read-only t)
          (minibuffer-completing-file-name nil))
      (erase-buffer)
      (insert "中a")
      (auto-space--add-space-between-cjk-and-ascii)
      (should (equal (buffer-string) "中 a"))
      (erase-buffer))))

(ert-deftest auto-space-skips-file-name-minibuffer ()
  (skip-unless (not (active-minibuffer-window)))
  (with-current-buffer (window-buffer (minibuffer-window))
    (let ((inhibit-read-only t)
          (minibuffer-completing-file-name t))
      (erase-buffer)
      (insert "中a")
      (auto-space--add-space-between-cjk-and-ascii)
      (should (equal (buffer-string) "中a"))
      (erase-buffer))))

(provide 'auto-space-mode-test)

;;; auto-space-mode-test.el ends here
