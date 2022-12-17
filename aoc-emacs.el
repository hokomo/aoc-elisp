;; -*- lexical-binding: t; -*-

(require 'aoc-util)
(require 'cl-lib)
(require 'f)
(require 's)
(require 'transient)
(require 'url)

;;; Util

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

;;; AOC Fetch

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
    (prog1 input-file
      (if (file-exists-p input-file)
          (message "%s already exists" input-file)
        (with-current-buffer (aoc-fetch (aoc-url year day :input))
          (write-file input-file))
        (message "Input saved to %s" input-file)))))

;;; AOC New

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
      (display-buffer (find-file-noselect (aoc-fetch-input year day))
                      '((display-buffer-reuse-window
                         display-buffer-below-selected))))))

;;; AOC Copy

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
          (delete-sexps (rx bol "(display"))
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

;;; AOC Readme

(defun aoc-readme-insert (day)
  (interactive (list (aoc-read-day)))
  (with-current-buffer (find-file-existing
                        (expand-file-name "README.org" aoc-root))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (rx "\n\n\n" bol "- ") nil t)
        (previous-line)
        (beginning-of-line)
        (insert
         (format
          "\n- [[#file-day-%02d-el][Day %02d]] ([[#file-day-%02d-clean-el][clean]])"
          day day day))))))

;;; AOC Mode

(define-minor-mode aoc-mode
  "Mode for Advent of Code"
  :lighter "AoC"
  :keymap (make-sparse-keymap)
  (setq-local eval-expression-print-length 20
              eval-expression-print-level 10))

(define-key aoc-mode-map (kbd "C-c C-c") #'aoc-copy)
(define-key aoc-mode-map (kbd "C-c C-e") #'aoc-readme-insert)

;;; Display

(defun display (buffer &optional name)
  (with-current-buffer (get-buffer-create (or name "*display*"))
    (special-mode)
    (setq-local window-point-insertion-type t)
    (with-silent-modifications
      (erase-buffer)
      (insert-buffer buffer))
    (display-buffer (current-buffer) '((display-buffer-reuse-window
                                        display-buffer-below-selected)))))

(defvar display-buffers nil)

(defun kill-display-buffers ()
  (interactive)
  (cl-loop for i from 0 while display-buffers
           do (kill-buffer (pop display-buffers))
           finally (message "Killed %d buffers" i)))

(defvar display--done nil)
(defvar display--after-hook nil)

(defun display-exit ()
  (interactive)
  ;; Copied from `transient.el'.
  (setq transient--stack nil)
  (setq transient--exitp t)
  (transient--pre-exit)
  (transient--post-exit))

(defun display-continue ()
  (interactive)
  (if display--done
      (display-exit)
    (exit-recursive-edit)))

(cl-defun display-clear (&key buffer &allow-other-keys)
  (interactive
   ;; NOTE: Must use `transient-prefix' in a `transient--do-stay' suffix.
   (oref transient--prefix scope))
  (unless display--done
    (kill-buffer buffer))
  (call-interactively #'display-continue))

(cl-defun display-quit (&key window &allow-other-keys)
  (interactive (oref transient-current-prefix scope))
  (when (window-valid-p window)
    (delete-window window))
  (setq display--after-hook #'keyboard-quit)
  (unless display--done
    (exit-recursive-edit)))

(cl-defun display-clear-quit (&key buffer &allow-other-keys)
  (interactive (oref transient-current-prefix scope))
  (kill-buffer buffer)
  (call-interactively #'display-quit))

(defun display--exit-transient-hook ()
  (remove-hook 'transient-exit-hook #'display--exit-transient-hook)
  (unless display--done
    (display-quit)))

(transient-define-prefix display-wait (buffer &optional name)
  :transient-non-suffix t
  ["Actions"
   ("c" "Continue" display-continue :transient t)
   ("r" "Clear current buffer and continue" display-clear :transient t)
   ("q" "Quit" display-quit)
   ("d" "Clear current buffer and quit" display-clear-quit)]
  (setq display--done nil
        display--after-hook nil)
  (add-hook 'transient-exit-hook #'display--exit-transient-hook)
  (transient-setup 'display-wait nil nil
                   :scope (list :buffer buffer
                                :window (display buffer name)))
  (unwind-protect
      (progn
        (recursive-edit)
        (setq display--done t)
        (when display--after-hook
          (funcall display--after-hook)))
    ;; NOTE: Make sure to clean up properly if there's a request to return to
    ;; the top level (using `top-level') from within the recursive call. This is
    ;; the default action when e.g. the user presses `q' when inside the
    ;; debugger.
    (unless display--done
      (setq display--done t)
      (display-exit)))
  ;; NOTE: If an error occurs after we return control from this function,
  ;; entering the debugger usually ends up clobbering Transient's
  ;; transient keymap. In this case the user should call `display-exit' to
  ;; get rid of the transient.
  )

(cl-defun call-with-display (name body &key wait)
  (let* ((existsp (and name (get-buffer name)))
         (result nil)
         (buffer (with-buffer name
                   (setq result (funcall body))
                   (text-mode)
                   (current-buffer))))
    (prog1 result
      (when (and name (not existsp))
        (cl-pushnew buffer display-buffers))
      (funcall (if wait #'display-wait #'display) buffer)
      (when (not name)
        (kill-buffer buffer)))))

(defun display-underscore-components (symbol)
  (let ((regexp (rx bos "_" (group (*? nonl)) (group (? "?")) eos)))
    (seq-let [_ name wait] (s-match regexp (cat symbol))
      (and name (list (if (string-empty-p name) nil name)
                      (string= wait "?"))))))

(defmacro with-display (spec &rest body)
  (declare (indent 1))
  (cl-destructuring-bind (expr &rest flags) (ensure-list spec)
    (mmt-with-gensyms (name rest new)
      `(cl-destructuring-bind (,name &rest ,rest)
           ,(pcase expr
              ('nil
               `(list nil ,@flags))
              ((and (pred symbolp)
                    (app display-underscore-components `(,name ,wait)))
               `(list ,name ,@flags :wait ,wait))
              (expr `(list ,expr ,@flags)))
         (apply #'call-with-display ,name (lambda () ,@body) ,rest)))))

(provide 'aoc-emacs)
