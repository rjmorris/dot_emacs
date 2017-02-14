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
    ;; web-mode
    ;; multi-web-mode
    mmm-mode
    ess
    company
    flycheck
    typescript-mode
    tide
    smart-compile
    smex
    visual-regexp
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
;; themes section

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

(load-theme 'commentator t)


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

;; Function to start a Python process and set up the emacs windows with the
;; source file on the left and the Python process on the right.
(defun rjm/set-up-python-environment ()
  (interactive)
  (delete-other-windows)
  (setq w1 (selected-window))
  (setq w1name (buffer-name))
  (setq w2 (split-window w1 nil t))
  (let ((current-prefix-arg '(1)))
    (call-interactively 'run-python))
  (set-window-buffer w2 "*Python*")
  (set-window-buffer w1 w1name))

;; "Smart" evaluation function:
;;   If a Python process hasn't been started, start it.
;;   Else, if a region is active, evaluate the region.
;;   Else, evaluate the current line.
(defun rjm/smart-python-eval ()
  (interactive)
  (cond
   ((not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
    (rjm/set-up-python-environment))
   ((region-active-p)
    (call-interactively 'python-shell-send-region))
   (t
    (python-shell-send-region (line-beginning-position) (line-end-position)))))

(add-hook 'python-mode-hook 'rjm/on-python-mode t)
(defun rjm/on-python-mode ()
  (local-set-key (kbd "<f8>") #'rjm/smart-python-eval)
)


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

(add-hook 'cperl-mode-hook 'rjm/on-cperl-mode t)
(defun rjm/on-cperl-mode ()
  )


;;-------------------------------------------------------------------------------
;; SH section

(add-hook 'sh-mode-hook 'rjm/on-sh-mode t)
(defun rjm/on-sh-mode ()
  )


;;-------------------------------------------------------------------------------
;; ESS section

(require 'ess-site)

;; Don't ask for the starting directory, just use the working directory of the
;; current buffer.
(setq ess-ask-for-ess-directory nil)

;; Don't replace _ with <-
(ess-toggle-underscore nil)

;; Provide command line arguments to R when starting it. --no-save disables the
;; prompt to save the history when quitting R.
(setq inferior-R-args "--no-restore --no-save")

;; Override some of the indentation settings. Be sure to change
;; ess-default-style to OWN so that ESS will pick up my indentation settings
;; instead of using the ones from a built-in style.
(setq ess-first-continued-statement-offset 4)
(setq ess-continued-statement-offset 0)
(setq ess-default-style 'OWN)

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

(add-hook 'ess-mode-hook 'rjm/on-ess-mode t)
(defun rjm/on-ess-mode ()
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

;; The biggest issue with HTML files is that they often have HTML, CSS, and JS
;; all embedded within them. I've tried a few different approaches for dealing
;; with this. I'm saving my config for the inactive approaches in case I want to
;; go back to them later.

;; ;; web-mode: A major mode that handles HTML/CSS/JS natively instead of using
;; ;; existing emacs modes. Negative: Keybinding/indentation doesn't work the
;; ;; same way as in standalone js-mode or css-mode.
;; (autoload 'web-mode "web-mode")
;; (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.thtml$" . web-mode))
;; (add-hook 'web-mode-hook 'rjm/on-web-mode t)
;; (defun rjm/on-web-mode ()
;;   ;; web-mode has its own comment/uncomment command. Attach it to the same
;;   ;; keybinding I use for the general version.
;;   (local-set-key (kbd "C-/") 'web-mode-comment-or-uncomment)
;; )

;; ;; multi-web-mode: Switches to existing emacs modes when in regions defined
;; ;; by a regex. Negative: Switching is slower than mmm-mode.
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((js-mode "<script[^>]*>" "</script>")
;;                   (css-mode "<style[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("htm" "html" "thtml"))
;; (multi-web-global-mode 1)

;; mmm-mode: Switches to existing emacs modes when in regions defined
;; by a regex.
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)
(mmm-add-mode-ext-class 'html-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-mode nil 'html-css)


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

(add-hook 'typescript-mode-hook 'rjm/on-typescript-mode t)
(defun rjm/on-typescript-mode ()
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
(setq markdown-command (concat "python -m markdown -x markdown.extensions.codehilite -c " (expand-file-name "~/.emacs.d/python-markdown-options.json")))

;; gfm is GitHub-flavored Markdown. There is also markdown-mode for regular
;; Markdown. I'm not aware of a BitBucket-specific Markdown mode.
(add-to-list 'auto-mode-alist '("readme\\.md$" . gfm-mode))

;; Some sources say newlines are significant in GFM. Testing shows this may not
;; be true in readme files, but possibly it is elsewhere. Or maybe it has
;; changed over time. In any case, avoid using fill-paragraph (M-q), which wraps
;; long lines by inserting newline characters. Instead, use visual-line-model to
;; "soft wrap" long lines.
(add-hook 'gfm-mode-hook 'rjm/on-gfm-mode t)
(defun rjm/on-gfm-mode ()
  (local-set-key (kbd "M-q") 'ignore)
  (visual-line-mode 1)
)


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
;; Smex

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
;; Rebind the original M-x just in case.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Number of recent commands shown at the beginning of the list.
(setq smex-history-length 3)


;;-------------------------------------------------------------------------------
;; search and replace

(global-set-key (kbd "C-c r") 'query-replace)

;; Use this in place of query-replace-regexp for visual feedback of matches and
;; replacements.
(global-set-key (kbd "C-c R") 'vr/query-replace)


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

;; When a file changes on disk, update the buffer to reflect the new contents.
(global-auto-revert-mode)

;; Function for computing a random number suitable as a seed and inserting it.
(defun rjm/insert-seed-at-point ()
  (interactive)
  (let
      ((max_int (expt 2 28)))
    (insert (number-to-string (random max_int)))))


;;-------------------------------------------------------------------------------
;; settings applicable only with graphical displays


(setq rjm/font-candidates
      '(
        "Office Code Pro Light-10"
        "Source Code Pro-10"
        "DejaVu Sans Mono-10"
        "Courier New-10"
        ))

(defun rjm/set-first-available-font (fonts)
  "Set the font to the first available from the list of candidates."
  (loop for font in fonts
        when (x-list-fonts font)
        do (progn
             (set-frame-font font)
             (return))))

(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (set-scroll-bar-mode 'right)
      (set-background-color "lightgray")
      (set-foreground-color "black")
      (set-cursor-color "black")
      (set-mouse-color "black")
      (rjm/set-first-available-font rjm/font-candidates)
      ))


;;-------------------------------------------------------------------------------
;; keybindings

;; Unset existing undo keybindings, and set others. Keep (kbd "C-x u") as undo
;; for now, since I'm used to that one.
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-/"))
;;(global-unset-key (kbd "C-x u"))
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
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
