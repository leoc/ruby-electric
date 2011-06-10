;; ruby-electric.el --- electric editing commands for unique ruby syntax
;;
;; Destroying all character closing commands, as the semantics in ruby-electric are done
;; far less comprehensively than are done in autopair.el (http://autopair.googlecode.com/svn/trunk/autopair.el)
;; Improved the semantic comprehension of pipes and blocks, so that the closing pipe or end is only inserted
;; if an appropriately matching end is not already present.
;;
;; Copyright (C) 2005 by Dee Zsombor
;;
;; Authors: Dee Zsombor <dee dot zsombor at gmail dot com>
;; Maintainer: Jakub Kuźma <qoobaa@gmail.com>
;; Gutter: Alex Redington <aredington at gmail dot com>
;; URL: http://github.com/aredington/ruby-electric/raw/master/ruby-electric.el
;; Keywords: languages ruby
;; Version: 1.1.1

;;; Code:

(require 'ruby-mode)

(defgroup ruby-electric nil
  "Minor mode providing electric editing commands for ruby files"
  :group 'ruby)

(defconst ruby-electric-expandable-do-re
  "do\\s-$")

(defconst ruby-electric-add-pipes
  "\\(\\s-do\\s-+\\|{\\s-*\\)")

(defconst ruby-electric-in-pipes
  "\\(\\s-do\\s-+\\|{\\s-*\\)|[^|]*")

(defcustom ruby-electric-simple-keywords-re
  (regexp-opt '("def" "if" "class" "module" "unless" "case" "while" "do" "until" "for" "begin") t)
  "*Regular expresion matching keywords for which closing 'end'
is to be inserted."
  :type 'regexp :group 'ruby-electric)

(defcustom ruby-electric-expand-delimiters-list '(all)
  "*List of contexts where matching delimiter should be
inserted. The word 'all' will do all insertions."
  :type '(set :extra-offset 8
              (const :tag "Everything" all )
              (const :tag "Vertical bar" ?\| ))
  :group 'ruby-electric)

(defcustom ruby-electric-newline-before-closing-bracket nil
  "*Controls whether a newline should be inserted before the
closing bracket or not."
  :type 'boolean :group 'ruby-electric)

;;;###autoload
(define-minor-mode ruby-electric-mode
  "Toggle Ruby Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When Ruby Electric mode is enabled, an indented 'end' is
heuristicaly inserted whenever typing a word like 'module',
'class', 'def', 'if', 'unless', 'case', 'until', 'for', 'begin',
'do'. Simple, double and back quotes as well as braces are paired
auto-magically. Expansion does not occur inside comments and
strings. Note that you must have Font Lock enabled."
  ;; initial value.
  nil
  ;;indicator for the mode line.
  " REl"
  ;;keymap
  ruby-mode-map
  (ruby-electric-setup-keymap))

(defun ruby-electric-setup-keymap()
  (define-key ruby-mode-map " " 'ruby-electric-space)
  (define-key ruby-mode-map "|" 'ruby-electric-bar)
  (define-key ruby-mode-map (kbd "RET") 'ruby-electric-return)
  (define-key ruby-mode-map (kbd "C-j") 'ruby-electric-return)
  (define-key ruby-mode-map (kbd "C-m") 'ruby-electric-return))

(defun ruby-electric-space (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (ruby-electric-space-can-be-expanded-p)
      (save-excursion
        (ruby-indent-line t)
        (newline)
        (ruby-insert-end))))

(defun ruby-electric-code-at-point-p()
  (and ruby-electric-mode
       (let* ((properties (text-properties-at (point))))
         (and (null (memq 'font-lock-string-face properties))
              (null (memq 'font-lock-comment-face properties))))))

(defun ruby-electric-string-at-point-p()
  (and ruby-electric-mode
       (consp (memq 'font-lock-string-face (text-properties-at (point))))))

(defun ruby-electric-is-last-command-char-expandable-punct-p()
  (or (memq 'all ruby-electric-expand-delimiters-list)
      (memq last-command-event ruby-electric-expand-delimiters-list)))

(defun ruby-electric-space-can-be-expanded-p()
  (if (ruby-electric-code-at-point-p)
      (let* ((ruby-electric-keywords-re
              (concat ruby-electric-simple-keywords-re "\\s-$"))
             (ruby-electric-single-keyword-in-line-re
              (concat "\\s-*" ruby-electric-keywords-re)))
        (save-excursion
          (ruby-backward-sexp 1)
          (or (looking-at ruby-electric-expandable-do-re)
              (and (looking-at ruby-electric-keywords-re)
                   (not (string= "do" (match-string 1)))
                   (progn
                     (beginning-of-line)
                     (looking-at ruby-electric-single-keyword-in-line-re))))))))


(defun ruby-electric-bar(arg)
  (interactive "P")
  (cond ((looking-back ruby-electric-add-pipes) 
         (self-insert-command (prefix-numeric-value arg)) (save-excursion (insert "|")))
        ((looking-back ruby-electric-in-pipes)
         (re-search-forward "|" nil t))
        (t self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-return-can-be-expanded-p()
  (if (ruby-electric-code-at-point-p)
      (let* ((ruby-electric-keywords-re
              (concat ruby-electric-simple-keywords-re "$"))
             (previous-sexp-identation
              (save-excursion (ruby-backward-sexp 1) (ruby-current-indentation)))
             (next-sexp-indentation
              (save-excursion (ruby-forward-sexp 1) (ruby-current-indentation))))
        (and (save-excursion
               (ruby-backward-sexp 1)
               (looking-at ruby-electric-keywords-re))
             (not (= previous-sexp-identation next-sexp-indentation))))))

(defun ruby-electric-return ()
  (interactive "*")
  (if (ruby-electric-return-can-be-expanded-p)
      (save-excursion
        (newline)
        (ruby-insert-end)))
  (reindent-then-newline-and-indent))

;; FIXME: it should be available in next versions of ruby-mode.el
(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(provide 'ruby-electric)

;;; ruby-electric.el ends here
