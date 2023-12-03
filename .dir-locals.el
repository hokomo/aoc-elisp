;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   (add-to-list 'load-path (expand-file-name ""))
                   (when (require 'aoc-emacs nil t)
                     (aoc-mode 1)))))))
