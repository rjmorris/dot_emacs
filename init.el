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


;; In Windows, store packages on my hard drive instead of my home directory,
;; which may be on a network share. (Having them on a network share can a
;; problem at work because of disk space quotas.)
;;
;; This variable needs to be set before calling package-initialize.

(if (eq system-type 'windows-nt)
    (setq package-user-dir "C:/Program Files (x86)/emacs-packages/elpa"))


;; By default, installed packages are activated after the init file (that is,
;; this file) is read. Disable that and instead activate the packages
;; immediately.

(setq package-enable-at-startup nil)
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
    ess
    company
    flycheck
    typescript-mode
    tide
    smart-compile
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
;; subdirectory. For Windows, also check Program Files.

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(if (eq system-type 'windows-nt)
    (add-to-list 'load-path "C:/Program Files (x86)/emacs-packages/site-lisp"))


;;-------------------------------------------------------------------------------
;; CC-mode section

(setq-default c-basic-offset 4)


;;-------------------------------------------------------------------------------
;; Python section

(setq python-fill-docstring-style 'django)

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
  ;; Resetting cperl-mode faces must be performed after the mode has loaded, not
  ;; in my syntax-highlighting section below.
  (reset-face 'cperl-nonoverridable-face)
  )


;;-------------------------------------------------------------------------------
;; SH section

(add-hook 'sh-mode-hook 'on-sh-mode t)
(defun on-sh-mode ()
  ;; Resetting sh-mode faces must be performed after the mode has loaded, not
  ;; in my syntax-highlighting section below.
  (reset-face 'sh-quoted-exec)
  )


;;-------------------------------------------------------------------------------
;; ESS section

(require 'ess-site)

;; Don't ask for the starting directory, just use the working directory of the
;; current buffer.
(setq ess-ask-for-ess-directory nil)

;; Don't replace _ with <-
(ess-toggle-underscore nil)

;; Function to start an R process and set up the emacs windows with the source
;; file on the left and the R process on the right.
(defun rjm/set-up-r-environment ()
  (interactive)
  (delete-other-windows)
  (setq w1 (selected-window))
  (setq w1name (buffer-name))
  (setq w2 (split-window w1 nil t))
  (R)
  (set-window-buffer w2 "*R*")
  (set-window-buffer w1 w1name))

;; "Smart" evaluation function:
;;   If an R process hasn't been started, start it.
;;   Else, if a region is active, evaluate the region.
;;   Else, if inside a function definition, evaluate the function.
;;   Else, evaluate the current line.
(defun rjm/smart-r-eval ()
  (interactive)
  (cond
   ((not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (rjm/set-up-r-environment))
   ((and transient-mark-mode mark-active)
    (call-interactively 'ess-eval-region))
   ((ess-beginning-of-function 'no-error)
    (call-interactively 'ess-eval-function)
    (ess-goto-end-of-function-or-para)
    (ess-next-code-line))
   (t
    (call-interactively 'ess-eval-line-and-step))))

(add-hook 'ess-mode-hook 'on-ess-mode t)
(defun on-ess-mode ()
  ;; Override ESS's binding of the Return key. It's set to newline-and-indent by
  ;; default. Note: Bind to "RET" here instead of "<return>". RET works in the
  ;; terminal and GUI, but <return> works only in the GUI.
  (local-set-key (kbd "RET") 'newline)

  ;; Override ESS's binding of C-Return. It's set to
  ;; ess-eval-region-or-line-and-step by default. Contrary to the comment about
  ;; the "RET" keybinding, note that "C-RET" doesn't work in the GUI or the
  ;; terminal, so just use the more standard "C-<return>". This doesn't work in
  ;; the terminal, but at least it works in the GUI.
  (local-set-key (kbd "C-<return>") #'dabbrev-expand)

  (local-set-key (kbd "<f8>") #'rjm/smart-r-eval)
)


;;-------------------------------------------------------------------------------
;; HTML section

(autoload 'web-mode "web-mode")
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.thtml$" . web-mode))

(add-hook 'web-mode-hook 'on-web-mode t)
(defun on-web-mode ()
  ;; Resetting web-mode faces must be performed after the mode has loaded, not
  ;; in my syntax-highlighting section below. Reset only those that don't
  ;; inherit from another face. (See web-mode.el to figure out which ones
  ;; inherit and which ones don't.)
  (reset-face 'web-mode-error-face)
  (reset-face 'web-mode-symbol-face)
  (reset-face 'web-mode-doctype-face)
  (reset-face 'web-mode-html-tag-face)
  (reset-face 'web-mode-html-tag-bracket-face)
  (reset-face 'web-mode-html-attr-name-face)
  (reset-face 'web-mode-block-attr-name-face)
  (reset-face 'web-mode-block-attr-value-face)
  (reset-face 'web-mode-json-key-face)
  (reset-face 'web-mode-json-context-face)
  (reset-face 'web-mode-param-name-face)
  (reset-face 'web-mode-whitespace-face)
  (reset-face 'web-mode-inlay-face)
  (reset-face 'web-mode-block-face)
  (reset-face 'web-mode-part-face)
  (reset-face 'web-mode-folded-face)
  (reset-face 'web-mode-bold-face)
  (reset-face 'web-mode-italic-face)
  (reset-face 'web-mode-underline-face)
  (reset-face 'web-mode-current-element-highlight-face)
  (reset-face 'web-mode-current-column-highlight-face)
  (reset-face 'web-mode-comment-keyword-face)
  (reset-face 'web-mode-sql-keyword-face)

  ;; web-mode has its own comment/uncomment command. Attach it to the same
  ;; keybinding I use for the general version.
  (local-set-key (kbd "C-/") 'web-mode-comment-or-uncomment)
)


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
;; TypeScript

(add-hook 'typescript-mode-hook 'on-typescript-mode t)
(defun on-typescript-mode ()
  (tide-setup)
  (flycheck-mode)
  (eldoc-mode)
  (company-mode)
)


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
;; copy/paste

;; Other Windows applications don't distinguish between the clipboard and the
;; primary selection, so when Emacs does it's more of a nuisance than a benefit.

(when (eq system-type 'windows-nt)
  ;; This causes highlighted text to be copied to the clipboard instead of the
  ;; primary selection.
  (setq mouse-drag-copy-region t)
  (setq select-active-regions nil)

  ;; This causes a middle click to paste from the clipboard instead of the primary
  ;; selection.
  (global-set-key (kbd "<mouse-2>") 'mouse-yank-at-click)
)


;;-------------------------------------------------------------------------------
;; auto-completion

(add-hook 'after-init-hook #'global-company-mode)

;; When looking for replacement candidates, ignore case. (Setting this variable
;; isn't strictly necessary, because it defaults to the value of case-fold-search,
;; which is t by default. However, set it anyway just to be explicit.) For
(setq dabbrev-case-fold-search t)

;; When looking for replacement candidates, ignore case only if the abbreviation
;; contains all lowercase letters. If it contains any uppercase letters, case is
;; significant.
(setq dabbrev-upcase-means-case-search t)

;; If replacement candidates differ only in case, treat them as distinct
;; candidates.
(setq dabbrev-case-distinction nil)

;; When applying a replacement, preserve the replacement's case. That is, don't
;; modify the replacement's case by applying the abbreviation's case pattern to
;; it. For example:
;;
;;   dabbrev-case-replace  abbreviation  replacement    expansion
;;   --------------------  ------------  -------------  -------------
;;           t                sor        SortDirection  sortdirection
;;          nil               sor        SortDirection  SortDirection
(setq dabbrev-case-replace nil)

(setq company-idle-delay nil)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)

(global-set-key (kbd "C-<return>") 'dabbrev-expand)
(global-set-key (kbd "C-S-<return>") 'company-complete)


;;-------------------------------------------------------------------------------
;; syntax checking

;; Check syntax only after saving the file or after enabling the mode.
(defvar flycheck-check-syntax-automatically '(save mode-enabled))


;;-------------------------------------------------------------------------------
;; compiling

(require 'smart-compile)
(add-to-list 'smart-compile-alist '(typescript-mode . "tsc"))

(setq compilation-read-command nil)

(global-set-key (kbd "<f5>") 'smart-compile)


;;-------------------------------------------------------------------------------
;; command interpreters (like the shell, R console, etc.)

(setq-default comint-scroll-to-bottom-on-output t)

;; Rebind the history navigation keys to show only those commands that match
;; what's currently entered at the prompt.
(define-key comint-mode-map (kbd "C-<up>") #'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "C-<down>") #'comint-next-matching-input-from-input)
(define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)


;;-------------------------------------------------------------------------------
;; miscellaneous section

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(setq column-number-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq frame-title-format "%b - emacs")

(delete-selection-mode t)
(transient-mark-mode t)

;; This adds part of the directory to buffer names when two buffers with the same
;; name are opened. Without this, emacs appends <2>, <3>, ...
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator ":")

;; If set to non-nil, PageUp when you're close to the top of the buffer moves
;; point to the top of the buffer. If nil, point doesn't move and Emacs gives an
;; error message.
(setq-default scroll-error-top-bottom t)

(setq backup-directory-alist '(("." . "~/.backups/emacs")))

;; For stopping on word boundaries in camelCase when using right-word and
;; left-word.
(global-subword-mode)

;; Don't automatically add a newline to the end of the file.
(setq require-final-newline nil)
;; Modes that think they need a newline at the end should override
;; require-final-newline with the value of mode-require-final-newline, which
;; defaults to t. Keep that behavior for now. If some modes abuse it, it's
;; probably better to change it in the mode hook instead of globally here.
;; (setq mode-require-final-newline nil)


;;-------------------------------------------------------------------------------
;; settings applicable only with graphical displays

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (set-scroll-bar-mode 'right)
      (set-background-color "lightgray")
      (set-foreground-color "black")
      (set-cursor-color "black")
      (set-mouse-color "black")
      ;;(set-default-font "7x13")
      (if (eq system-type 'windows-nt)
          ;;(set-default-font "DejaVu Sans Mono-10")
          ;;(set-default-font "Courier New-10")
          (set-default-font "Source Code Pro Light-10")
        (set-default-font "Office Code Pro Light-10"))
      )
)


;;-------------------------------------------------------------------------------
;; syntax highlighting

;; My goal is to disable almost all syntax highlighting. The primary exception
;; is comments, but there may be a few others such as syntax errors. To
;; accomplish this, I will first make all faces match the default face. Then I
;; will override a few select faces.
;;
;; TODO: When I start emacs in a terminal, sometimes the background is dark and
;; other times it's light. In cases where I'm specifying a color, figure out how
;; to use a different color when the background is dark and light.
;;
;; TODO: Look into creating a custom theme for this.

;; Some modes support limiting the degree of font-lock applied. Take them up on
;; it. Even less font-lock could be requested by setting this to nil, but some
;; modes may interpret that as disabling it altogether.
(setq font-lock-maximum-decoration 1)

;; Define a function to "reset" a face. This tries to make the face a copy of
;; the default face.
;;
;; Note that I previously used the copy-face function to copy the default face
;; to the target face. However, it didn't work well in the terminal, where the
;; default face's foreground and background have the value 'unspecified.
;; copy-face doesn't appear to copy 'unspecified to the target face, so the
;; target face's initial foreground and background colors were unchanged.
(defun reset-face (face)
  (set-face-attribute face nil
                      :inherit 'default
                      :foreground 'unspecified
                      :background 'unspecified
                      :weight 'unspecified
                      )
)

(reset-face 'font-lock-builtin-face)
(reset-face 'font-lock-comment-face)
(set-face-attribute 'font-lock-comment-face nil :foreground "RoyalBlue4")
(reset-face 'font-lock-constant-face)
(reset-face 'font-lock-function-name-face)
(reset-face 'font-lock-keyword-face)
(reset-face 'font-lock-string-face)
(reset-face 'font-lock-type-face)
(reset-face 'font-lock-variable-name-face)
(reset-face 'font-lock-warning-face)

(reset-face 'comint-highlight-prompt)
(set-face-attribute 'comint-highlight-prompt nil :weight 'bold)
(reset-face 'comint-highlight-input)

;; On Linux, the active region's background color is taken from GTK by default.
;; The default color doesn't have enough contrast against the lightgray
;; background. Set the foreground to nil so that the foreground text has the
;; same face as when it isn't in the active region.
(reset-face 'region)
(set-face-attribute 'region nil :foreground nil :background "white")


;;-------------------------------------------------------------------------------
;; keybindings

;; Unset existing undo keybindings, and set others. Keep (kbd "C-x u") as undo
;; for now, since I'm used to that one.
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-/"))
;;(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c s") 'shell)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; For enlarging and shrinking windows, it's best to use a single key combo
;; instead of a chained one. That way it can be held down to conveniently expand
;; or shrink repeatedly.
;;
;; These screw up emacs -nw, because the terminal maps the arrow keys
;; to sequences that start with M-[, so the arrows fail to function.
;; (global-set-key (kbd "M-]") 'enlarge-window-horizontally)
;; (global-set-key (kbd "M-[") 'shrink-window-horizontally)
(global-set-key (kbd "C->") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<") 'shrink-window-horizontally)
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-_") 'shrink-window)

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
