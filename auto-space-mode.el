;;; auto-space-mode.el --- Auto add space between CJK and ASCII -*- lexical-binding: t -*-

;; Author: Randolph <xiaojianghuang@yahoo.com>
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, wp
;; URL: https://github.com/wowhxj/auto-space-mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; auto-space-mode automatically adds spaces between CJK characters
;; (Chinese, Japanese, Korean) and ASCII words during input.  This behavior
;; ONLY occurs during input and does NOT modify other parts of your document.
;; File name and path input in the minibuffer is left unchanged.
;;
;; To use auto-space-mode, add the following to your init file:
;;
;;   (require 'auto-space-mode)
;;   (auto-space-mode 1)
;;
;; The package provides two additional commands for working with regions:
;;
;; - `auto-space-add-in-region': Add spaces between CJK and ASCII in region
;; - `auto-space-remove-in-region': Remove spaces between CJK and ASCII in region
;;
;; The difference between auto-space-mode and pangu-spacing:
;;
;; 1. auto-space-mode ONLY adds spaces during input; it does NOT modify
;;    other parts of your document.
;; 2. auto-space-mode adds ACTUAL spaces instead of using overlays.
;; 3. You can FREELY add or remove those spaces.

;;; Code:

;;;; Customization

(defgroup auto-space nil
  "Automatic spacing between CJK and ASCII characters."
  :group 'convenience
  :prefix "auto-space-")

;;;; Character detection functions

(defun auto-space--cjk-p (char)
  "Check if CHAR is a CJK character.
This includes Chinese, Japanese Kana, and Korean Hangul."
  (and char
       (or ;; Chinese characters
           (and (>= char #x4e00) (<= char #x9fff))     ; CJK Unified Ideographs
           (and (>= char #x3400) (<= char #x4dbf))     ; CJK Extension A
           (and (>= char #x20000) (<= char #x2a6df))   ; CJK Extension B
           (and (>= char #x2a700) (<= char #x2b73f))   ; CJK Extension C
           (and (>= char #x2b740) (<= char #x2b81f))   ; CJK Extension D
           (and (>= char #x2b820) (<= char #x2ceaf))   ; CJK Extension E
           ;; Japanese Kana
           (and (>= char #x3040) (<= char #x309f))     ; Hiragana
           (and (>= char #x30a0) (<= char #x30ff))     ; Katakana
           ;; Korean Hangul
           (and (>= char #xac00) (<= char #xd7af))     ; Hangul Syllables
           (and (>= char #x1100) (<= char #x11ff))))) ; Hangul Jamo

(defun auto-space--halfwidth-p (char)
  "Check if CHAR is a halfwidth character.
This includes ASCII letters, numbers, and some punctuation."
  (and char
       (or (and (>= char ?a) (<= char ?z))
           (and (>= char ?A) (<= char ?Z))
           (and (>= char ?0) (<= char ?9))
           (member char '(?% ?$ ?&)))))

;;;; Core spacing functions

(defun auto-space--add-space-between-cjk-and-ascii ()
  "Automatically add a space between CJK and ASCII characters.
This function is intended to be used in `post-self-insert-hook'.
Do nothing while the minibuffer is completing a file name."
  (unless (and (minibufferp) minibuffer-completing-file-name)
    (let ((current-char (char-before))
          (prev-char (char-before (1- (point))))
          (next-char (char-after)))
      ;; Check if we need to insert space before current character
      (when (and current-char prev-char
                 (or (and (auto-space--cjk-p prev-char) (auto-space--halfwidth-p current-char))
                     (and (auto-space--halfwidth-p prev-char) (auto-space--cjk-p current-char)))
                 (not (eq prev-char ?\s)))
        (save-excursion
          (goto-char (1- (point)))
          (insert " ")))
      ;; Check if we need to insert space after current character
      (when (and current-char next-char
                 (or (and (auto-space--cjk-p current-char) (auto-space--halfwidth-p next-char))
                     (and (auto-space--halfwidth-p current-char) (auto-space--cjk-p next-char)))
                 (not (eq current-char ?\s)))
        (save-excursion
          (goto-char (point))
          (insert " "))))))

;;;; Obsolete function aliases for backward compatibility

(define-obsolete-function-alias 'is-chinese-character #'auto-space--cjk-p "0.2")
(define-obsolete-function-alias 'is-halfwidth-character #'auto-space--halfwidth-p "0.2")
(define-obsolete-function-alias 'add-space-between-chinese-and-english #'auto-space--add-space-between-cjk-and-ascii "0.2")
(define-obsolete-function-alias 'delayed-add-space-between-chinese-and-english #'auto-space--delayed-add-space "0.2")
(define-obsolete-function-alias 'auto-space--chinese-p #'auto-space--cjk-p "0.2")
(define-obsolete-function-alias 'auto-space--add-space-between-chinese-and-english #'auto-space--add-space-between-cjk-and-ascii "0.2")

;;;; Delayed execution

(defun auto-space--delayed-add-space ()
  "Delayed execution to add space between CJK and ASCII."
  (run-with-idle-timer 0 nil #'auto-space--add-space-between-cjk-and-ascii))

;;;; Text processing functions

(defun auto-space--process-pasted-text (text prev-char next-char)
  "Process pasted TEXT to add spaces between CJK and ASCII characters.
Consider PREV-CHAR and NEXT-CHAR for context."
  (with-temp-buffer
    (insert (if prev-char (concat (char-to-string prev-char) text) text))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-char (char-after))
            (next-char-internal (char-after (1+ (point)))))
        (when (and current-char next-char-internal
                   (or (and (auto-space--cjk-p current-char) (auto-space--halfwidth-p next-char-internal))
                       (and (auto-space--halfwidth-p current-char) (auto-space--cjk-p next-char-internal)))
                   (not (eq current-char ?\s)))
          (forward-char)
          (insert " ")))
      (forward-char))
    (let ((buffer-content (buffer-string)))
      (if prev-char
          (setq buffer-content (substring buffer-content 1)))
      ;; Add space between the last char of pasted text and next-char
      (if (and next-char
               (or (and (auto-space--cjk-p (aref buffer-content (1- (length buffer-content)))) (auto-space--halfwidth-p next-char))
                   (and (auto-space--halfwidth-p (aref buffer-content (1- (length buffer-content)))) (auto-space--cjk-p next-char)))
               (not (eq next-char ?\s)))
          (setq buffer-content (concat buffer-content " ")))
      buffer-content)))

;;;; Yanking advice (currently disabled)

;; The following functions are disabled by default because they may break
;; file paths in clipboard.  Uncomment to enable.
;;
;; (defun auto-space--yank-advice (orig-fun &rest args)
;;   "Advice to automatically add spaces between CJK and ASCII characters after yanking."
;;   (let ((beg (point))
;;         (prev-char (char-before)))
;;     (apply orig-fun args)
;;     (let ((end (point))
;;           (next-char (char-after)))
;;       (let ((pasted-text (buffer-substring-no-properties beg end)))
;;         (delete-region beg end)
;;         (insert (auto-space--process-pasted-text pasted-text prev-char next-char))))))
;;
;; (advice-add 'yank :around #'auto-space--yank-advice)
;; (advice-add 'yank-pop :around #'auto-space--yank-advice)

;;;; Region commands

;;;###autoload
(defun auto-space-add-in-region (start end)
  "Add spaces between CJK and ASCII characters in the selected region.
Works on region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((adjusted-end (copy-marker end t)))
      (while (< (point) adjusted-end)
        (let ((current-char (char-after (point)))
              (next-char (char-after (1+ (point)))))
          (when (and current-char next-char
                     (or (and (auto-space--cjk-p current-char) (auto-space--halfwidth-p next-char))
                         (and (auto-space--halfwidth-p current-char) (auto-space--cjk-p next-char)))
                     (not (eq next-char ?\s)))
            (save-excursion
              (goto-char (1+ (point)))
              (insert " ")))
          (forward-char 1))))))

;;;###autoload
(defun auto-space-remove-in-region (start end)
  "Remove spaces between CJK and ASCII characters in the selected region.
Works on region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[[:space:]]+" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (let ((prev-char (char-before match-start))
              (next-char (char-after match-end)))
          (when (and prev-char next-char
                     (or (and (auto-space--cjk-p prev-char) (auto-space--halfwidth-p next-char))
                         (and (auto-space--halfwidth-p prev-char) (auto-space--cjk-p next-char))))
            (delete-region match-start match-end)))))))

;;;; Backward compatibility aliases for region commands

(define-obsolete-function-alias 'add-space-between-chinese-and-english-in-region #'auto-space-add-in-region "0.2")
(define-obsolete-function-alias 'remove-space-between-chinese-and-english-in-region #'auto-space-remove-in-region "0.2")

;;;; Minor mode

;;;###autoload
(define-minor-mode auto-space-mode
  "Toggle automatic spacing between CJK and ASCII characters.
When enabled, automatically adds spaces between CJK characters
\(Chinese, Japanese, Korean) and ASCII words during input.
This behavior ONLY occurs during input and does NOT modify
other parts of your document.  File name and path input in the
minibuffer is left unchanged.

\\{auto-space-mode-map}"
  :lighter " Auto-Space"
  :global t
  :group 'auto-space
  (if auto-space-mode
      (add-hook 'post-self-insert-hook #'auto-space--add-space-between-cjk-and-ascii)
    (remove-hook 'post-self-insert-hook #'auto-space--add-space-between-cjk-and-ascii)))

(provide 'auto-space-mode)

;;; auto-space-mode.el ends here
