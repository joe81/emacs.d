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

(cua-mode)

;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :height 100)

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
        (expand-file-name "~/.emacs.d/packages/")
	) load-path))

;; include sub-directories of elpa and packages
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

;; backups
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.d/backups"))
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)

;;;;;;;;;;;;;;;;;;;;
;; Package Config ;;
;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

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
(setq auto-mode-alist (append auto-mode-alist '(("\\Berksfile$" . ruby-mode))))
(setq auto-mode-alist (append auto-mode-alist '(("\\.god$" . ruby-mode))))

;; CoffeeScript
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

(autoload 'rubydb "rubydb3x" "Ruby debugger" t)

(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; personal snippets
        "~/.emacs.d/elpa/current_yasnippet/snippets" ;; the default collection
        ))

;; add rails-mode snippets
(add-hook 'ruby-mode-hook '(lambda ()
                             (make-local-variable 'yas-extra-modes)
                             (add-to-list 'yas-extra-modes 'rails-mode)
                             (yas-minor-mode 1)
                             ))
(yas-global-mode 1)

;; flymake
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; ruby-tools-mode
;;   https://github.com/rejeep/ruby-tools.el
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'robe-start)

(add-hook 'after-init-hook 'global-company-mode)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-inf-ruby))
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; RuboCop
(require 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; Ruby-Hash-Syntax-Switcher
;; (define-key ruby-mode-map (kbd "M-S") 'ruby-toggle-hash-syntax)
(define-key global-map (kbd "M-S") 'ruby-toggle-hash-syntax)

;; Speedbar
(require 'sr-speedbar)
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "DejaVu Sans Mono 11")
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))


;; org mode
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

;;;;;;;;;;;;;;;;;;;;;
;; -- Functions -- ;;
;;;;;;;;;;;;;;;;;;;;;

(defun search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

(global-set-key (kbd "C-d") 'search-current-word)

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )

;; ag - search
(setq ag-arguments '("--ignore" "tmp" "--ignore" "log" "--ignore" "backups"
                     "--ignore" "TAGS" "--ignore" ".rsync_cache" "--ignore" "coverage"
                     "--ignore" "cache"
                     "--smart-case" "--column" "--nogroup" "--"))
(global-set-key (kbd "C-3") 'ag-project) ; Strg - 3

;; find file in project
(global-set-key (kbd "C-1") 'find-file-in-project)
;; do NOT search files in below directories
(setq ffip-prune-patterns '(".git" ".rsync_cache" "tmp" "vendor/cache" ".routes"))

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

(global-set-key "\C-f" 'vc-git-grep)

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

(defun highlight-operators-ruby()
  (font-lock-add-keywords
   nil '(("[&|]" . font-lock-constant-face))
   t))
(add-hook 'ruby-mode-hook 'highlight-operators-ruby)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(autopair-global-mode t)
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-idle-delay 0.6)
 '(company-tooltip-limit 5)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(ecb-options-version "2.40")
 '(fill-column 90)
 '(ido-enable-flex-matching t)
 '(js-indent-level 2)
 '(kill-ring-max 2000)
 '(linum-mode t t)
 '(rails-always-use-text-menus t)
 '(rails-chm-file nil)
 '(rails-default-environment "development")
 '(rails-enable-ruby-electric t)
 '(rails-indent-and-complete nil)
 '(rails-number-of-lines-shown-when-opening-log-file 130)
 '(rails-ri-command "ri")
 '(rails-tags-command "ctags-exuberant -e --Ruby-kinds=-cmfF -o TAGS -R .")
 '(rails-text-menu-function nil)
 '(rails-ws:default-server-type "mongrel")
 '(rails-ws:port "3000")
 '(rails-ws:server-name "http://localhost")
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(sh-basic-offset 2)
 '(speedbar-default-position (quote right))
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag t)
 '(sr-speedbar-default-width 30)
 '(yas-fallback-behavior (quote call-other-command))
 '(yas-prompt-functions (quote (yas/x-prompt yas/dropdown-prompt)))
 '(yas-snippet-revival nil)
 '(yas-trigger-symbol " =>")
 '(yas/next-field-key (quote ("<tab>")))
 '(yas/prev-field-key (quote ("<backtab>" "<S-tab>")))
 '(yas/skip-and-clear-key "C-d")
 '(yas/trigger-key "TAB"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((nil (:foreground "#FBDE2D"))))
 '(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
 '(font-lock-operator-face ((nil (:foreground "#FBDE2D"))))
 '(font-lock-variable-name-face ((nil (:foreground "#ff0" :weight semi-bold)))))
