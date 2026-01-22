;;; auto-space-mode.el --- Auto adding space between Chinese and English -*- lexical-binding: t -*-

;; Author: Randolph <xiaojianghuang@yahoo.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, wp
;; URL: https://github.com/wowhxj/auto-space-mode

;;; Commentary:

;; auto-space-mode automatically adds spaces between Chinese characters and
;; English words during input.  This behavior ONLY occurs during input and does
;; NOT modify other parts of your document.
;;
;; To use auto-space-mode, add the following to your init file:
;;
;;   (require 'auto-space-mode)
;;   (auto-space-mode 1)
;;
;; The package provides two additional commands for working with regions:
;;
;; - `auto-space-add-in-region': Add spaces between Chinese and English in region
;; - `auto-space-remove-in-region': Remove spaces between Chinese and English in region
;;
;; The difference between auto-space-mode and pangu-spacing:
;;
;; 1. auto-space-mode ONLY adds spaces during input; it does NOT modify other
;;    parts of your document.
;; 2. auto-space-mode adds ACTUAL spaces instead of using overlays.
;; 3. You can FREELY add or remove those spaces.

;;; Code:

;;;; Character detection functions

(defun auto-space--chinese-p (char)
  "Check if CHAR is a Chinese character."
  (and char
       (or (and (>= char #x4e00) (<= char #x9fff))     ; CJK Unified Ideographs
           (and (>= char #x3400) (<= char #x4dbf))     ; CJK Unified Ideographs Extension A
           (and (>= char #x20000) (<= char #x2a6df))   ; CJK Unified Ideographs Extension B
           (and (>= char #x2a700) (<= char #x2b73f))   ; CJK Unified Ideographs Extension C
           (and (>= char #x2b740) (<= char #x2b81f))   ; CJK Unified Ideographs Extension D
           (and (>= char #x2b820) (<= char #x2ceaf))))) ; CJK Unified Ideographs Extension E

(defun auto-space--halfwidth-p (char)
  "Check if CHAR is a halfwidth character, including letters, numbers and some punctuation."
  (and char
       (or (and (>= char ?a) (<= char ?z))
           (and (>= char ?A) (<= char ?Z))
           (and (>= char ?0) (<= char ?9))
           (member char '(?% ?$ ?&)))))

;;;; Core spacing functions

(defun auto-space--add-space-between-chinese-and-english ()
  "Automatically add a space between Chinese and English characters.
This function is intended to be used in `post-self-insert-hook'."
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point))))
        (next-char (char-after)))
    ;; Check if we need to insert space before current character
    (when (and current-char prev-char
               (or (and (auto-space--chinese-p prev-char) (auto-space--halfwidth-p current-char))
                   (and (auto-space--halfwidth-p prev-char) (auto-space--chinese-p current-char)))
               (not (eq prev-char ?\s)))
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))
    ;; Check if we need to insert space after current character
    (when (and current-char next-char
               (or (and (auto-space--chinese-p current-char) (auto-space--halfwidth-p next-char))
                   (and (auto-space--halfwidth-p current-char) (auto-space--chinese-p next-char)))
               (not (eq current-char ?\s)))
      (save-excursion
        (goto-char (point))
        (insert " ")))))

;;;; Obsolete function aliases for backward compatibility

(define-obsolete-function-alias 'is-chinese-character 'auto-space--chinese-p "0.1")
(define-obsolete-function-alias 'is-halfwidth-character 'auto-space--halfwidth-p "0.1")
(define-obsolete-function-alias 'add-space-between-chinese-and-english 'auto-space--add-space-between-chinese-and-english "0.1")
(define-obsolete-function-alias 'delayed-add-space-between-chinese-and-english 'auto-space--delayed-add-space "0.1")

;;;; Delayed execution

(defun auto-space--delayed-add-space ()
  "Delayed execution to add space between Chinese and English."
  (run-with-idle-timer 0 nil #'auto-space--add-space-between-chinese-and-english))

;;;; Text processing functions

(defun auto-space--process-pasted-text (text prev-char next-char)
  "Process pasted TEXT to add spaces between Chinese and English characters.
Consider PREV-CHAR and NEXT-CHAR for context."
  (with-temp-buffer
    (insert (if prev-char (concat (char-to-string prev-char) text) text))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-char (char-after))
            (next-char-internal (char-after (1+ (point)))))
        (when (and current-char next-char-internal
                   (or (and (auto-space--chinese-p current-char) (auto-space--halfwidth-p next-char-internal))
                       (and (auto-space--halfwidth-p current-char) (auto-space--chinese-p next-char-internal)))
                   (not (eq current-char ?\s)))
          (forward-char)
          (insert " ")))
      (forward-char))
    (let ((buffer-content (buffer-string)))
      (if prev-char
          (setq buffer-content (substring buffer-content 1)))
      ;; Add space between the last char of pasted text and next-char
      (if (and next-char
               (or (and (auto-space--chinese-p (aref buffer-content (1- (length buffer-content)))) (auto-space--halfwidth-p next-char))
                   (and (auto-space--halfwidth-p (aref buffer-content (1- (length buffer-content)))) (auto-space--chinese-p next-char)))
               (not (eq next-char ?\s)))
          (setq buffer-content (concat buffer-content " ")))
      buffer-content)))

;;;; Yanking advice (currently disabled)

;; The following functions are disabled by default because they may break
;; file paths in clipboard.  Uncomment to enable.
;;
;; (defun auto-space--yank-advice (orig-fun &rest args)
;;   "Advice to automatically add spaces between Chinese and English characters after yanking."
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
  "Add spaces between Chinese and English characters in the selected region.
Works on region from START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((adjusted-end (copy-marker end t)))
      (while (< (point) adjusted-end)
        (let ((current-char (char-after (point)))
              (next-char (char-after (1+ (point)))))
          (when (and current-char next-char
                     (or (and (auto-space--chinese-p current-char) (auto-space--halfwidth-p next-char))
                         (and (auto-space--halfwidth-p current-char) (auto-space--chinese-p next-char)))
                     (not (eq next-char ?\s)))
            (save-excursion
              (goto-char (1+ (point)))
              (insert " ")))
          (forward-char 1))))))

;;;###autoload
(defun auto-space-remove-in-region (start end)
  "Remove spaces between Chinese and English characters in the selected region.
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
                     (or (and (auto-space--chinese-p prev-char) (auto-space--halfwidth-p next-char))
                         (and (auto-space--halfwidth-p prev-char) (auto-space--chinese-p next-char))))
            (delete-region match-start match-end)))))))

;;;; Backward compatibility aliases for region commands

(define-obsolete-function-alias 'add-space-between-chinese-and-english-in-region 'auto-space-add-in-region "0.1")
(define-obsolete-function-alias 'remove-space-between-chinese-and-english-in-region 'auto-space-remove-in-region "0.1")

;;;; Minor mode

;;;###autoload
(define-minor-mode auto-space-mode
  "Toggle automatic spacing between Chinese and English characters.
When enabled, automatically adds spaces between Chinese characters and
English words during input.  This behavior ONLY occurs during input and does
NOT modify other parts of your document.

\\{auto-space-mode-map}"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook #'auto-space--add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook #'auto-space--add-space-between-chinese-and-english)))

(provide 'auto-space-mode)

;;; auto-space-mode.el ends here
