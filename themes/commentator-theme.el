;;; commentator-theme.el --- minimalist theme in which only comments appear in a different color

;; URL: https://github.com/rjmorris/emacs-commentator-theme
;; Version: 20160206.0
;; Author: Joey Morris

;;; Commentary:

;; A minimalist theme. In programming modes, syntax highlighting is applied only
;; to comments; all other text is the default color. In non-programming modes,
;; minimalism is a guiding principle but isn't as strictly enforced as in
;; programming modes.

;;; Code:

(deftheme commentator
  "minimalist theme in which only comments appear in a different color")

;; Documentation on the backquote, comma, and comma-at forms is found at
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html

(let ((class '((class color) (min-colors 89)))
      (fg "#000000")
      (bg "#d3d3d3")
      (comment "#27408b"))
  (custom-theme-set-faces
   'commentator
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,fg))))
   `(fringe ((,class (:foreground ,fg :background "#f2f2f2"))))
   `(mode-line ((,class (:foreground ,fg :background "#bebebe" :box (:line-width -1 :style released-button)))))
   `(mode-line-inactive ((,class (:foreground "#333333" :background "#e5e5e5" :box (:line-width -1 :color "#bfbfbf" :style nil)))))

   `(font-lock-comment-face ((,class (:foreground ,comment :background ,bg))))
   `(font-lock-builtin-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-constant-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-function-name-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-keyword-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-string-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-type-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-variable-face ((,class (:foreground ,fg :background ,bg))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg :background ,bg))))

   `(region ((,class (:foreground ,fg :background "white"))))

   `(comint-highlight-prompt ((,class (:weight bold))))

   `(sh-quoted-exec ((,class (:foreground ,fg :background ,bg))))
   `(sh-heredoc ((,class (:foreground ,fg :background ,bg))))

   `(cperl-nonoverridable-face ((,class (:foreground ,fg :background ,bg))))
   `(cperl-array-face ((,class (:foreground ,fg :background ,bg))))
   `(cperl-hash-face ((,class (:foreground ,fg :background ,bg))))

   `(web-mode-error-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-symbol-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-doctype-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-html-tag-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-block-attr-name-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-block-attr-value-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-json-key-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-json-context-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-param-name-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-whitespace-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-inlay-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-block-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-part-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-folded-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-bold-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-italic-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-underline-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-current-element-highlight-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-current-column-highlight-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-comment-keyword-face ((,class (:foreground ,fg :background ,bg))))
   `(web-mode-sql-keyword-face ((,class (:foreground ,fg :background ,bg))))
  ))

;; Define code to run when the package is initialized. Note that the package
;; manager looks for that weird autoload comment line (the "magic" line), so
;; don't alter it.

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'commentator)

;;; commentator-theme.el ends here
