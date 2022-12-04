;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'f)
(require 's)
(require 'url)

(defmacro save-kill-ring (&rest body)
  `(let* ((kill-ring '())
          (kill-ring-yank-pointer kill-ring))
     ,@body))

(defun delete-sexp ()
  (save-kill-ring
   (kill-sexp)
   (kill-line)
   (while (looking-at "\n")
     (kill-line))))

(defun delete-sexps (regexp)
  (while (re-search-forward regexp nil t)
    (goto-char (match-beginning 0))
    (delete-sexp)))

(defun aoc-clean-buffer (buffer &optional remove-comments)
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
          (when remove-comments
            ;; Delete comments.
            (goto-char (point-min))
            (while (re-search-forward (rx bol (* blank) ";;") nil t)
              (goto-char (match-beginning 0))
              (kill-line)
              (while (looking-at "\n")
                (kill-line))))
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

(defun aoc-copy (buffer &optional remove-comments)
  (interactive (list (current-buffer) current-prefix-arg))
  (with-current-buffer (aoc-clean-buffer buffer remove-comments)
    (let* ((name (symbol-name major-mode))
           (lang (and (string-match (rx (*? nonl) (group (+ (not "-"))) "-mode")
                                    name)
                      (match-string 1 name))))
      (aoc-wrap-buffer (point-min) (point-max) 'multiline lang))
    (let ((map (make-sparse-keymap))
          (copy (lambda ()
                  (interactive)
                  (copy-region-as-kill (point-min) (point-max))
                  (message "Code block copied to kill ring")
                  (kill-buffer))))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd "q") copy)
      (evil-define-key* 'normal map (kbd "q") copy)
      (use-local-map map))
    (switch-to-buffer (current-buffer))))

(defvar aoc-root
  (expand-file-name "~/Desktop/sync/aoc-2022/"))

(defun aoc-read-day ()
  (string-to-number (read-string "Day: " (format-time-string "%-d"))))

(defvar aoc-session nil)

(defun aoc-session ()
  (let ((session-file (expand-file-name ".session" aoc-root)))
    (and (f-exists-p session-file) (s-trim (f-read-text session-file)))))

(defun aoc-url (year day &optional kind)
  (let ((base (format "https://adventofcode.com/%d/day/%d" year day)))
    (cl-ecase kind
      ((nil) base)
      (:input (concat base "/input"))
      (:answer (concat base "/answer")))))

(defun aoc-fetch (url)
  (unless aoc-session
    (error "Missing session cookie"))
  (let ((url-request-extra-headers
         `(("Cookie" . ,(format "session=%s" aoc-session)))))
    (with-current-buffer (url-retrieve-synchronously url t)
      (prog1 (current-buffer)
        (goto-char (point-min))
        ;; Get rid of the HTTP response headers.
        (re-search-forward "\n\n")
        (delete-region (point-min) (point))))))

(defun aoc-fetch-input (year day)
  (interactive (list (string-to-number (format-time-string "%Y"))
                     (aoc-read-day)))
  (let ((input-file (expand-file-name (format "input-%02d.txt" day) aoc-root)))
    (if (file-exists-p input-file)
        (message "%s already exists" input-file)
      (with-current-buffer (aoc-fetch (aoc-url year day :input))
        (write-file input-file))
      (message "Input saved to %s" input-file))))

(cl-defun aoc-new (filename level &optional year day)
  (interactive (let* ((day (aoc-read-day))
                      (filename (expand-file-name (format "day-%02d.el" day)
                                                  aoc-root)))
                 (list filename (format "%02d" day)
                       (string-to-number (format-time-string "%Y")) day)))
  (find-file filename)
  (unless (file-exists-p filename)
    (save-excursion
      (insert-file-contents (expand-file-name "day-template.el" aoc-root))
      (goto-char (point-min))
      (while (re-search-forward (rx "<level>") nil t)
        (replace-match level)))
    (normal-mode))
  (when (y-or-n-p "Fetch input?")
    (let ((aoc-session (or aoc-session (aoc-session))))
      (aoc-fetch-input year day))))

(define-minor-mode aoc-mode
  "Mode for Advent of Code"
  :lighter "AoC"
  :keymap (make-sparse-keymap))

(define-key aoc-mode-map (kbd "C-c C-c") #'aoc-copy)

(provide 'aoc-emacs)
