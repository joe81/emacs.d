;;;;;;;;;;;;;;;
;; Packaging ;;
;;;;;;;;;;;;;;;

(require 'package)

;; MELPA
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))

;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;
;; Main Configs ;;
;;;;;;;;;;;;;;;;;;

;; it will place the Emacs meta key on the Mac CMD key, and free up
;; the ALT key for normal Mac use:
(if (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'meta))

(if (boundp 'ns-option-modifier)
    (setq ns-option-modifier nil))

;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 110)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; load-paths
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elpa/")
        (expand-file-name "~/.emacs.d/mixed/")
	) load-path))

;; include sub-directories of elpa
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;;;;;;;;;;;;;;;;;;;;
;; Package Config ;;
;;;;;;;;;;;;;;;;;;;;

;; Lorem Ipsum
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/elpa/color-theme-20080305.834/themes/color-theme-blackboard.el")
(color-theme-blackboard)

;; linum mode
(require 'linum+)
(global-linum-mode 1)

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M--" 'etags-select-find-tag)

;; cut and paste
(transient-mark-mode 1)

;; set line/column numbers
(line-number-mode 1)
(column-number-mode 1)

;; disable blinked cursor
(blink-cursor-mode 0)

;; always indent using spaces, never tabs
(setq-default indent-tabs-mode nil)

;; format of comments in code
(require 'filladapt)
(setq-default filladapt-mode t)

;; disable tool-bar
(tool-bar-mode 0)
;; disable scroll-bar
(scroll-bar-mode 0)

(global-set-key [(control tab)] 'other-window)

;; IDO
(ido-mode)
(setq ido-max-prospects 6)             ; don't spam my minibuffer
(setq ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; apache-mode
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))

;; Rails
(require 'rails)
(setq auto-mode-alist (append auto-mode-alist '(("\\Gemfile$" . ruby-mode))))

;; CoffeeScript
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

(autoload 'rubydb "rubydb3x" "Ruby debugger" t)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; personal snippets
        "~/.emacs.d/elpa/yasnippet-20131010.2/snippets" ;; the default collection
        ))

;; add rails-mode snippets
(add-hook 'ruby-mode-hook '(lambda ()
                             (make-local-variable 'yas-extra-modes)
                             (add-to-list 'yas-extra-modes 'rails-mode)
                             (yas-minor-mode 1)
                             ))
(yas-global-mode 1)


;; Autocomplete
(add-to-list 'load-path "~/.emacs.d/mixed/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/mixed/auto-complete-1.3.1/ac-dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(require 'auto-complete-etags)

;; Autocomplete
(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-gtags)
            ;; (add-to-list 'ac-sources 'ac-source-abbrev)
            ;; (add-to-list 'ac-sources 'ac-source-yasnippet)
            (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
            ))

;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (push 'ac-source-robe ac-sources)

;;;;;;;;;;;;;;;;;;;;
;; -- ORG-MODE -- ;;
;;;;;;;;;;;;;;;;;;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/documents/org"))

;; Clocking: http://orgmode.org/manual/Clocking-work-time.html#Clocking-work-time
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)

;;;;;;;;;;;;;;;;;;;;;;;
;; -- KEYBINDINGS -- ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-skeleton ruby-puts-skeleton
  "Inserts puts"
  "PP: "
  "puts '*********************************'"\n
  "pp " str \n
  "puts '*********************************'"\n
 )
(global-set-key "\C-l" 'ruby-puts-skeleton)

;; disable emacs shutdown
(global-set-key "\C-x\C-c" 'f )

(global-set-key [(control tab)] 'other-window)

(global-set-key (kbd "<M-down>") 'forward-paragraph)
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-right>") 'forward-sexp)
(global-set-key (kbd "<M-left>") 'backward-sexp)
(global-set-key (kbd "<C-down>") '(lambda () (interactive) (forward-line 6)))
(global-set-key (kbd "<C-up>") '(lambda () (interactive) (forward-line -6)))

(global-set-key (kbd "C-c C-d") 'duplicate-thing)

(global-set-key "\C-xB" 'bury-buffer)
(global-set-key "\C-xE" 'apply-macro-to-region-lines)
(global-set-key "\C-cv" 'comment-region)
(global-set-key "\C-cy" 'uncomment-region)
(global-set-key "\M-n" 'goto-line)

(global-set-key [f7] '(lambda () (interactive)
                      (setq truncate-lines (not truncate-lines))
                      (recenter)))
(global-set-key [f3] 'call-last-kbd-macro)
(global-set-key [f8] 'font-lock-fontify-buffer)
(global-set-key [f4] 'indent-region)
(global-set-key [f5] 'compile)

;; Hide/Show blocks
(global-set-key "\C-c\C-a" 'hs-hide-all)
(global-set-key "\C-c\C-h" 'hs-hide-all)
(global-set-key "\C-c\C-q" 'hs-show-all)
(global-set-key "\C-c\C-y" 'hs-show-block)

(require 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [C-delete] 'kill-line)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (read-kbd-macro "<M-backspace>") 'backward-delete-word)
(global-set-key (read-kbd-macro "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'delete-word)

;;;;;;;;;;;;;;;;;;;;;
;; -- VARIABLES -- ;;
;;;;;;;;;;;;;;;;;;;;;

;; for emacs using ruby 1.9 and new hash syntax
(font-lock-add-keywords
 'ruby-mode
 '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))
(font-lock-add-keywords
 'rhtml_mode
 '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.4)
 '(ac-comphist-threshold 1.6)
 '(autopair-global-mode t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(global-pabbrev-mode t)
 '(ido-enable-flex-matching t)
 '(kill-ring-max 2000)
 '(linum-mode t t)
 '(rails-always-use-text-menus t)
 '(rails-chm-file nil)
 '(rails-default-environment "development")
 '(rails-enable-ruby-electric t)
 '(rails-number-of-lines-shown-when-opening-log-file 130)
 '(rails-ri-command "ri")
 '(rails-ruby-command "~/.rvm/rubies/ruby-2.0.0-p247/bin/ruby")
 '(rails-text-menu-function nil)
 '(rails-ws:default-server-type "mongrel")
 '(rails-ws:port "3000")
 '(rails-ws:server-name "http://localhost")
 '(rails-indent-and-complete nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(yas/fallback-behavior (quote call-other-command))
 '(yas/next-field-key (quote ("<tab>")))
 '(yas/prev-field-key (quote ("<backtab>" "<S-tab>")))
 '(yas/prompt-functions (quote (yas/x-prompt yas/dropdown-prompt)))
 '(yas/skip-and-clear-key "C-d")
 '(yas/snippet-revival nil)
 '(yas/trigger-key "TAB")
 '(yas/trigger-symbol " =>"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((nil (:foreground "#FBDE2D"))))
 '(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
 '(font-lock-variable-name-face ((nil (:foreground "#ff0" :weight semi-bold)))))
