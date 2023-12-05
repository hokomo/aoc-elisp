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

(defmacro save-selected-window-excursion (&rest body)
  (mmt-with-gensyms (window state)
    `(let* ((,window (selected-window))
            (,state (window-state-get ,window)))
       (unwind-protect (progn ,@body)
         (window-state-put ,state ,window)))))

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

;;; Fetch

(defcustom aoc-root nil
  "Path to a directory that acts as the working directory for
various `aoc' functionality. If nil, `default-directory' is used
instead."
  :group 'aoc
  :type 'directory)

(defcustom aoc-session nil
  "The session token to use when contacting Advent of Code. If nil,
the token is instead read from the file at `<aoc-root>/.session',
if it exists."
  :group 'aoc
  :type 'string)

(defcustom aoc-year nil
  "The Advent of Code edition to default to when using various `aoc'
functionality. If nil, the current year is used instead."
  :group 'aoc
  :type 'integer)

(defun aoc-read-day ()
  (let* ((current (format-time-string "%-d"))
         (day (string-to-number (read-string "Day: " current))))
    (if (and (integerp day) (<= 1 day 25))
        day
      (user-error "Day must be an integer between 1 and 25"))))

(defun aoc-read-year ()
  (let* ((current (if aoc-year
                      (number-to-string aoc-year)
                    (format-time-string "%Y")))
         (year (string-to-number (read-string "Year: " current))))
    (if (and (integerp year) (>= year 2015))
        year
      (user-error "Year must be an integer larger than 2015"))))

(defun aoc-file (file)
  (expand-file-name file aoc-root))

(defun aoc-read-session (&optional file)
  (let ((file (or file (aoc-file ".session"))))
    (and (f-exists-p file) (s-trim (f-read-text file)))))

(defun aoc-url (year day &optional kind)
  (let ((base (format "https://adventofcode.com/%d/day/%d" year day)))
    (cl-ecase kind
      ((nil) base)
      (:input (concat base "/input"))
      (:answer (concat base "/answer")))))

(defun aoc-fetch (url)
  (if-let ((aoc-session (or aoc-session (aoc-read-session))))
      (let ((url-request-extra-headers
             `(("Cookie" . ,(format "session=%s" aoc-session)))))
        (with-current-buffer (url-retrieve-synchronously url t)
          (prog1 (current-buffer)
            (goto-char (point-min))
            ;; Get rid of the HTTP response headers.
            (re-search-forward "\n\n")
            (delete-region (point-min) (point)))))
    (user-error "Missing session token")))

(defun aoc-fetch-input (year day)
  (interactive (list (aoc-read-year) (aoc-read-day)))
  (let ((input-file (aoc-file (format "input-%02d.txt" day))))
    (prog1 input-file
      (if (file-exists-p input-file)
          (message "%s already exists" input-file)
        (with-current-buffer (aoc-fetch (aoc-url year day :input))
          (write-file input-file))
        (message "Input saved to %s" input-file)))))

;;; New

(defcustom aoc-template nil
  "Path to a file to use as the template when creating a solution
file. Can be a path relative to `aoc-root' if `aoc-root' is
non-nil."
  :group 'aoc
  :type 'file)

(cl-defun aoc-new (file year day)
  (interactive (let ((year (aoc-read-year))
                     (day (aoc-read-day)))
                 (list (aoc-file (format "day-%02d.el" day)) year day)))
  (find-file file)
  (unless (file-exists-p file)
    (save-excursion
      (let ((template (and aoc-template (aoc-file aoc-template))))
        (when (file-exists-p template)
          (insert-file-contents template)
          (goto-char (point-min))
          (while (re-search-forward (rx "<day>") nil t)
            (replace-match (format "%02d" day))))))
    (normal-mode))
  (when (and (called-interactively-p 'any) (y-or-n-p "Fetch input?"))
    (display-buffer (find-file-noselect (aoc-fetch-input year day))
                    '((display-buffer-reuse-window
                       display-buffer-below-selected)))))

;;; Loading

(cl-defun aoc-slim-buffer (buffer &key (require t) (input t) (output t)
                                    (dev t) (comments t))
  ;; Don't prefix the buffer name with a space, because such buffers
  ;; ("uninteresting buffers") are not fontified by font-lock. See
  ;; https://stackoverflow.com/q/18418079.
  (let ((slim (generate-new-buffer "*temp*")))
    (prog1 slim
      (with-current-buffer slim
        (save-excursion
          (insert-buffer buffer)
          (goto-char (point-min))
          ;; Remove everything before and including any `require's.
          (when require
            (save-excursion
              (goto-char (point-max))
              (when (re-search-backward (rx bol "(require") nil t)
                (next-line 2)
                (beginning-of-line)
                (delete-region (point-min) (point)))))
          ;; Remove input forms (`definput').
          (when input
            (save-excursion (delete-sexps (rx bol "(definput"))))
          ;; Remove output forms (`expect', `display', `with-profiling' and
          ;; `with-sprofiling').
          (when output
            (save-excursion
              (delete-sexps
               (rx bol "(" (or "expect" "display"
                               (seq "with-" (? "s") "profiling"))))))
          ;; Remove "dev" forms useful during development (`comment').
          (when dev
            (save-excursion
              (save-excursion (delete-sexps (rx bol "(comment")))))
          ;; Remove comments.
          (when comments
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward (rx bol (* blank) ";;") nil t)
                (goto-char (match-beginning 0))
                (kill-line)
                (while (looking-at "\n")
                  (kill-line)))))
          (delete-trailing-whitespace))
        (funcall (buffer-local-value 'major-mode buffer))))))

(cl-defun aoc-call-with-slim (func &rest keys &allow-other-keys)
  (let* ((buffer (current-buffer))
         (slim (apply #'aoc-slim-buffer buffer keys)))
    (save-selected-window-excursion
     (with-silent-modifications
       ;; NOTE: Perform the operation on the slim version of the text but still
       ;; within the original buffer, so that any location information is
       ;; preserved. We silence modifications because swapping the buffer text
       ;; sets the modified flag.
       (buffer-swap-text slim)
       (unwind-protect (save-selected-window-excursion (funcall func slim))
         (buffer-swap-text slim)
         (kill-buffer slim))))))

(cl-defun aoc-compile-with-slim (func &rest keys &allow-other-keys)
  (emacs-lisp--before-compile-buffer)
  (apply #'aoc-call-with-slim
         (lambda (original)
           ;; Write out the slim version to disk so that file compilation
           ;; functions get the real thing.
           (save-buffer)
           ;; NOTE: Temporarily swap with the original to avoid the user seeing
           ;; the temporary slim version as a result of whatever FUNC might
           ;; decide to do (e.g. `emacs-lisp-native-compile-and-load' displays
           ;; its compilation buffer which can trigger a redisplay of our
           ;; buffer). Don't write anything to disk though, and pretend there
           ;; were no modifications to the buffer (e.g. so we don't get asked to
           ;; save).
           (with-silent-modifications
             (buffer-swap-text original))
           (unwind-protect (funcall func original)
             ;; NOTE: Once done, write out the original version to disk and undo
             ;; the swap for `aoc-call-with-slim'. We set the buffer modified
             ;; flag in order to force `save-buffer' to save.
             (with-silent-modifications
               (set-buffer-modified-p t)
               (save-buffer)
               (buffer-swap-text original))))
         keys))

(defun aoc-load ()
  (interactive)
  (aoc-call-with-slim (lambda (_) (eval-buffer)) :require nil :input nil))

(defun aoc-byte-compile-load ()
  (interactive)
  (aoc-compile-with-slim
   (lambda (_) (emacs-lisp-byte-compile-and-load)) :require nil :input nil))

(defun aoc-native-compile-load ()
  (interactive)
  (aoc-compile-with-slim
   (lambda (_) (emacs-lisp-native-compile-and-load)) :require nil :input nil))

;;; Run

(defun aoc-run (&optional n)
  (interactive "p")
  (let ((regexp (rx (or "(expect" "(display"))))
    (if (equal current-prefix-arg '(16))
        (save-excursion
          (goto-char (point-min))
          (cl-loop while (re-search-forward regexp nil t)
                   do (eval-defun nil)))
      (let ((arg current-prefix-arg))
        (when (called-interactively-p 'any)
          (cond
           ((equal arg '(4))
            (setf n 1))
           ((minusp n)
            (setf n (abs n)
                  arg '(4)))
           (t
            (setf arg nil))))
        (save-excursion
          (goto-char (point-min))
          (cl-loop repeat n
                   always (re-search-forward regexp nil t)
                   finally (let ((current-prefix-arg arg))
                             (call-interactively #'eval-defun))))))))

;;; Copy

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

(cl-defun aoc-copy (buffer &optional (comments t))
  (interactive (list (current-buffer) (not current-prefix-arg)))
  (with-current-buffer (aoc-slim-buffer buffer :comments (not comments))
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

;;; Readme

(defun aoc-readme-insert (day)
  (interactive (list (aoc-read-day)))
  (with-current-buffer (find-file-existing (aoc-file "README.org"))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (rx bol "- ") nil t)
        (previous-line)
        (beginning-of-line)
        (insert
         (format "\n- [[./day-%02d.el][Day %02d]] \
([[./day-%02d-slim.el][slim]], [[./input-%02d.txt][input]])"
                 day day day day))))))

(defcustom aoc-save-slim-p nil
  "Whether `aoc-mode' should add a local `after-save-hook' that
automatically invokes `aoc-save-slim' on buffer save."
  :group 'aoc
  :type 'boolean)

(defun aoc-save-slim (&optional buffer)
  (interactive (list (current-buffer)))
  (if buffer-file-name
      (let* ((base (file-name-base buffer-file-name))
             (file (expand-file-name (format "%s-slim.el" base)))
             (slim (aoc-slim-buffer (or buffer (current-buffer)))))
        (with-current-buffer (find-file-noselect file)
          (buffer-swap-text slim)
          (save-buffer)
          (kill-buffer slim)))
    (user-error "Buffer not visiting a file")))

;;; Mode

(define-minor-mode aoc-mode
  "Mode for Advent of Code"
  :lighter "AoC"
  :keymap (make-sparse-keymap)
  (when aoc-save-slim-p
    (add-hook 'after-save-hook #'aoc-save-slim nil t))
  (setq-local eval-expression-print-length 20
              eval-expression-print-level 10))

(define-key aoc-mode-map (kbd "C-c C-l") #'aoc-load)
(define-key aoc-mode-map (kbd "C-c C-b") #'aoc-byte-compile-load)
(define-key aoc-mode-map (kbd "C-c C-n") #'aoc-native-compile-load)
(define-key aoc-mode-map (kbd "C-c C-r") #'aoc-run)
(define-key aoc-mode-map (kbd "C-c C-c") #'aoc-copy)
(define-key aoc-mode-map (kbd "C-c C-s") #'aoc-save-slim)
(define-key aoc-mode-map (kbd "C-c C-e") #'aoc-readme-insert)
(define-key aoc-mode-map (kbd "C-c C-d") #'toggle-debug-on-error)

(with-eval-after-load 'core-spacemacs
  (spacemacs/set-leader-keys-for-minor-mode 'aoc-mode
    "l" #'aoc-load
    "b" #'aoc-byte-compile-load
    "n" #'aoc-native-compile-load
    "r" #'aoc-run
    "C" #'aoc-copy
    "s" #'aoc-save-slim
    "x" #'aoc-readme-insert
    "d" #'toggle-debug-on-error))

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
  ;; entering the debugger usually ends up clobbering Transient's transient
  ;; keymap. In this case the user should manually call `display-exit' to get
  ;; rid of the transient.
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
      (unwind-protect (funcall (if wait #'display-wait #'display) buffer)
        (when (not name)
          (kill-buffer buffer))))))

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

;;; Graphviz

(defun call-with-graphviz (func file)
  (require 'graphviz-dot-mode)
  (let ((original (current-buffer))
        (buffer (find-file-noselect file)))
    (prog1 buffer
      (with-buffer buffer
        (erase-buffer)
        (funcall func)
        (save-buffer)
        (graphviz-dot-preview)
        (switch-to-buffer original)))))

(defmacro with-graphviz (file &rest body)
  (declare (indent 1))
  `(call-with-graphviz (lambda () ,@body) ,file))

(provide 'aoc-emacs)
