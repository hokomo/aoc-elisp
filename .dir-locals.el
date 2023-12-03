;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (let ((dir (file-name-directory
                             (locate-dominating-file
                              default-directory ".dir-locals.el"))))
                   (add-to-list 'load-path dir)
                   (when (require 'aoc-emacs nil t)
                     (setq-local aoc-session
                                 (aoc-read-session
                                  (expand-file-name ".session" dir))
                                 aoc-template
                                 (expand-file-name "day-template.el" dir))
                     (aoc-mode 1))))))
 ("2022" . ((nil . ((eval . (let ((dir (file-name-directory
                                        (locate-dominating-file
                                         default-directory ".dir-locals.el"))))
                              (setq-local aoc-year 2022
                                          aoc-root
                                          (expand-file-name "2022" dir))))))))
 ("2023" . ((nil . ((eval . (let ((dir (file-name-directory
                                        (locate-dominating-file
                                         default-directory ".dir-locals.el"))))
                              (setq-local aoc-year 2023
                                          aoc-root
                                          (expand-file-name "2023" dir)))))))))
