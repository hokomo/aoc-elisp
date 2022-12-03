;; -*- lexical-binding: t; -*-

(defmacro save-kill-ring (&rest body)
  `(let* ((kill-ring '())
          (kill-ring-yank-pointer kill-ring))
     ,@body))

(defun delete-sexp ()
  (save-kill-ring
   (kill-sexp)
   (kill-line)
   (when (looking-at "\n")
     (kill-line))))

(defun delete-sexps (regexp)
  (while (re-search-forward regexp nil t)
    (let ((begin (match-beginning 0)))
      (goto-char begin)
      (delete-sexp))))

(defun aoc-clean-buffer (buffer)
  ;; Should not prefix the buffer name with a space, because uninteresting
  ;; buffers are not fontified by font-lock. See
  ;; https://stackoverflow.com/q/18418079.
  (let ((clean (generate-new-buffer "*temp*")))
    (prog1 clean
      (with-current-buffer clean
        (save-excursion
          (insert-buffer buffer)
          ;; Remove everything before and including any `require's.
          (goto-char (point-max))
          (when (re-search-backward (rx bol "(require") nil t)
            (beginning-of-line 3)
            (delete-region (point-min) (point)))
          ;; Remove `definput' and `expect'.
          (delete-sexps (rx bol "(definput"))
          (delete-sexps (rx bol "(expect"))
          (delete-trailing-whitespace))
        (funcall (buffer-local-value 'major-mode buffer))))))

(defun aoc-wrap-buffer (beg end type &optional lang)
  (let* ((marker (cl-ecase type
                   (inline "`")
                   (multiline "```"))))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (insert marker)
        (when (and (eq type 'multiline) lang)
          (insert lang "\n"))
        (goto-char (point-max))
        (insert marker)))))

(defun aoc-copy (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer (aoc-clean-buffer buffer)
    (let* ((name (symbol-name major-mode))
           (lang (and (string-match (rx (*? nonl) (group (+ (not "-"))) "-mode")
                                    name)
                      (match-string 1 name))))
      (aoc-wrap-buffer (point-min) (point-max) 'multiline lang))
    (copy-region-as-kill (point-min) (point-max))
    (message "Code block copied to kill ring")
    (read-only-mode 1)
    (let ((map (make-sparse-keymap))
          (kill-current-buffer (lambda () (interactive) (kill-buffer))))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "q") kill-current-buffer)
      (evil-define-key* 'normal map (kbd "q") kill-current-buffer)
      (use-local-map map))
    (switch-to-buffer (current-buffer))))

(defvar aoc-root
  (expand-file-name "~/Desktop/sync/aoc-2022/"))

(defun aoc-new (filename level)
  (interactive (let* ((day (read-string "Day: " (format-time-string "%d")))
                      (filename (expand-file-name (format "day-%s.el" day)
                                                  aoc-root)))
                 (list filename day)))
  (find-file filename)
  (unless (file-exists-p filename)
    (save-excursion
      (insert-file-contents (expand-file-name "day-template.el" aoc-root))
      (goto-char (point-min))
      (while (re-search-forward (rx "<level>") nil t)
        (replace-match level)))
    (normal-mode)))

(define-minor-mode aoc-mode
  "Mode for Advent of Code"
  :lighter "AoC"
  :keymap (make-sparse-keymap))

(define-key aoc-mode-map (kbd "C-c C-c") #'aoc-copy)

(provide 'aoc-emacs)
