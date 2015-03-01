;; A note on comments: Emacs indents comments differently depending on the
;; number of semicolons.
;;
;;   1 semicolon - indent to the value of comment-column (40 by default)
;;   2 semicolon - indent to the current syntactical level
;;   3 semicolon - don't alter the indentation


(let ((min-version 24))
  (when (< emacs-major-version min-version)
    (error "This configuration assumes Emacs version %s or higher" min-version)))


;; For common lisp programming constructs, such as the loop function.
(require 'cl)


;;-------------------------------------------------------------------------------
;; package management


(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") nil)


;; Define a list of packages that I want installed everywhere. If any of the
;; packages aren't installed, update the package list, and then install the
;; missing packages.

(defvar required-packages
  '(markdown-mode
    rainbow-mode
    switch-window
    web-mode
    ))

(defun required-packages-installed-p ()
  (loop for pkg in required-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (required-packages-installed-p)
  (message "Refreshing package database...")
  (package-refresh-contents)
  (message "Done refreshing package database.")
  (dolist (pkg required-packages)
    (when (not (package-installed-p pkg))
      (message "Installing package %s." pkg)
      (package-install pkg))))


;; Packages not handled by the package manager should go in the site-lisp
;; subdirectory.

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))


;;-------------------------------------------------------------------------------
;; CC-mode section

(setq-default c-basic-offset 4)


;;-------------------------------------------------------------------------------
;; Python section

;; electric-indent-mode has been disabled globally below.
;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (electric-indent-local-mode 0)))


;;-------------------------------------------------------------------------------
;; Perl section

;; Use cperl-mode instead of perl-mode by default. Comment this section out
;; to use perl-mode by default.
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl[0-9.]*" . cperl-mode))

(setq perl-indent-level 4)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)

;; Don't put else on the same line as the previous closing bracket.
(setq cperl-merge-trailing-else nil)

;; Disable the highlighting of trailing whitespace with underscores.
(setq cperl-invalid-face nil)

(add-hook 'cperl-mode-hook 'on-cperl-mode t)
(defun on-cperl-mode ()
  ;; This is present in my font-lock section below, but something in the
  ;; cperl-mode startup must override it. Redo it once cperl is loaded.
  (copy-face 'default 'cperl-nonoverridable-face)
  )


;;-------------------------------------------------------------------------------
;; ESS section

;; Don't ask for the starting directory, just use the working directory of the
;; current buffer.
(setq ess-ask-for-ess-directory nil)

;; Don't replace _ with <-
(ess-toggle-underscore nil)

;;(add-hook 'ess-mode-hook (lambda () (font-lock-mode 0)) t)


;;-------------------------------------------------------------------------------
;; HTML section

(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(copy-face 'default 'web-mode-html-tag-face)
(copy-face 'default 'web-mode-html-attr-name-face)

(add-to-list 'auto-mode-alist '("\\.thtml$" . web-mode))


;;-------------------------------------------------------------------------------
;; CSS section

(setq css-indent-offset 4)
(add-hook 'css-mode-hook 'rainbow-mode)


;;-------------------------------------------------------------------------------
;; javascript

;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; (setq js2-highlight-level 0)


;;-------------------------------------------------------------------------------
;; markdown

;; Different Markdown implementations support different features. The ones I'll
;; interact with most frequently are the ones from GitHub and BitBucket. Ideally,
;; I would detect whether I'm inside a GitHub repository, a BitBucket repository,
;; or neither, and configure things accordingly. But I'm not doing that for now.
;;
;; Even though GitHub and BitBucket don't necessarily support the same syntax,
;; they are the same in one major way that deviates from regular Markdown. They
;; both treat underscores in the middle of words (like in file names or variable
;; names) as literal underscores instead of emphasis markers. Regular Markdown
;; mode applies emphasis in that situation.

;; BitBucket's Markdown implementation uses python-markdown. Look into grip for
;; GitHub if python-markdown turns out not to be satisfactory for GitHub. Also
;; note that readme files can be edited directly on the GitHub and BitBucket
;; websites.
(setq markdown-command "python3 -m markdown")

;; gfm is GitHub-flavored Markdown. There is also markdown-mode for regular
;; Markdown. I'm not aware of a BitBucket-specific Markdown mode.
;;
;; gfm-mode automatically enables visual-line-mode, which "soft-wraps" long lines
;; in the buffer. Some sources say newlines are significant in GFM. Testing shows
;; this may not be true in readme files, but possibly it is elsewhere. Or maybe
;; it has changed over time. In any case, avoid using fill-paragraph (M-q), which
;; wraps long lines by inserting newline characters.
(add-to-list 'auto-mode-alist '("readme\\.md$" . gfm-mode))


;;-------------------------------------------------------------------------------
;; template package

;;(require 'template)
;;(template-initialize)
;;(setq template-auto-update nil)


;;-------------------------------------------------------------------------------
;; line-wrapping

(setq-default fill-column 80)
(setq sentence-end-double-space nil)
(setq-default truncate-lines t)


;;-------------------------------------------------------------------------------
;; buffer switching

;; Enable ido-mode only for buffer switching, not for finding files, too.
;; It can be annoying when finding files.
(ido-mode 'buffers)

(global-set-key (kbd "C-<tab>") 'switch-window)
(setq switch-window-increase 6)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)


;;-------------------------------------------------------------------------------
;; indentation

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Simply disabling electric-mode-hook would usually be fine, but sometimes a
;; major mode will set it for you, so use this hook to override it.
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode 0)))


;;-------------------------------------------------------------------------------
;; miscellaneous section

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(tool-bar-mode 0)
(set-scroll-bar-mode 'right)
(setq column-number-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq frame-title-format "%b - emacs")

(delete-selection-mode t)
(setq transient-mark-mode nil)

;; This adds part of the directory to buffer names when two buffers with the same
;; name are opened. Without this, emacs appends <2>, <3>, ...
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator ":")

(setq-default comint-scroll-to-bottom-on-output t)
(setq-default scroll-error-top-bottom nil)

(setq backup-directory-alist '(("." . "~/.backups/emacs")))

; For stopping on word boundaries in camelCase when using right-word and
; left-word.
(subword-mode t)


;;-------------------------------------------------------------------------------
;; settings applicable in X

(if window-system
    (progn
      (set-background-color "lightgray")
      (set-foreground-color "black")
      (set-cursor-color "black")
      (set-mouse-color "black")
      ;;(set-default-font "7x13")
      (set-default-font "Bitstream Vera Sans Mono-10") ;; available in emacs23
      )
)


;;-------------------------------------------------------------------------------
;; font lock

;;(global-font-lock-mode nil)
(setq font-lock-maximum-decoration 1)

(copy-face 'default 'font-lock-comment-face)
(set-face-foreground 'font-lock-comment-face "RoyalBlue4")
(copy-face 'default 'font-lock-builtin-face)
(copy-face 'default 'font-lock-constant-face)
(copy-face 'default 'font-lock-function-name-face)
(copy-face 'default 'font-lock-keyword-face)
(copy-face 'default 'font-lock-string-face)
(copy-face 'default 'font-lock-type-face)
(copy-face 'default 'font-lock-variable-name-face)
(copy-face 'default 'font-lock-warning-face)

(copy-face 'default 'cperl-nonoverridable-face)

(copy-face 'default 'comint-highlight-prompt)
(set-face-bold-p 'comint-highlight-prompt t)
(copy-face 'default 'comint-highlight-input)


;;-------------------------------------------------------------------------------
;; keybindings

;; Unset existing undo keybindings, and set others. Keep (kbd "C-x u") as undo
;; for now, since I'm used to that one.
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-/"))
;;(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; These screw up emacs -nw, because the terminal maps the arrow keys
;; to sequences that start with M-[
;; (global-set-key (kbd "M-]") 'enlarge-window-horizontally)
;; (global-set-key (kbd "M-[") 'shrink-window-horizontally)

;; Swap the default keybindings for these. This way is more intuitive to me.
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)

;; Replace list-buffers with buffer-menu to open the buffer menu in the current buffer.
(global-set-key (kbd "C-x C-b") 'buffer-menu)


;;-------------------------------------------------------------------------------
;; enabled/disabled commands

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
