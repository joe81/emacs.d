;;;;;;;;;;;;;;;
;; Packaging ;;
;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'package)

;; MELPA
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)

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

(set-face-attribute 'default nil :height 105)

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


;; Lorem Ipsum
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

;; Color Theme
(load-file "/home/joe/.emacs.d/elpa/blackboard-theme-20161216.656/blackboard-theme.el")

;; linum mode
(require 'linum+)
(global-linum-mode 1)

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

(add-hook 'after-init-hook 'inf-ruby-switch-from-compilation)

;; CoffeeScript
(defun coffee-custom ()
  "coffee-mode-hook"
 (set (make-local-variable 'tab-width) 2))
(add-hook 'coffee-mode-hook '(lambda() (coffee-custom)))

(autoload 'rubydb "rubydb3x" "Ruby debugger" t)

;; (require 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets" ;; personal snippets
;;         "~/.emacs.d/elpa/current_yasnippet/snippets" ;; the default collection
;;         ))

;; add rails-mode snippets
;; (add-hook 'ruby-mode-hook '(lambda ()
;;                              (make-local-variable 'yas-extra-modes)
;;                              (add-to-list 'yas-extra-modes 'rails-mode)
;;                              (yas-minor-mode 1)
;;                              ))
;; (yas-global-mode 1)

;; flycheck
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'json-mode-hook 'flycheck-mode)


;; ruby-tools-mode
;;   switch string to symbol and so on ...
;;   https://github.com/rejeep/ruby-tools.el
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

;; robe
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'slim-mode-hook 'robe-mode)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (eval-after-load 'company
;;   '(push 'company-robe company-backends))

(require 'smartparens-config)
;; Always start smartparens mode in js-mode.
(add-hook 'ruby-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'slim-mode-hook #'smartparens-mode)
(add-hook 'coffee-mode-hook #'smartparens-mode)
(add-hook 'yaml-mode-hook #'smartparens-mode)


;; RuboCop
;; (require 'rubocop)
;; (add-hook 'ruby-mode-hook 'rubocop-mode)

;; Ruby-Hash-Syntax-Switcher
(global-set-key (kbd "M-S") 'ruby-hash-syntax-toggle)

;; Ruby FactoryGirl Switcher
(global-set-key (kbd "C-c C-f") 'ruby-factory-switch-to-buffer)

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

;; ag - search
(setq ag-arguments '("--ignore" "*.scss" "--ignore" "*.html" "--ignore" "tmp" "--ignore" "log" "--ignore" "backups"
                     "--ignore" "public" "--ignore" "assets/website" "--ignore" "assets/newsletter"
                     "--ignore" "TAGS" "--ignore" ".rsync_cache" "--ignore" "coverage"
                     "--ignore" "cache" "--ignore" "data" "--ignore" "cms"
                     "--ignore" "vcr_cassettes" "--ignore" "*.zip"
                     "--ignore" "yarn.lock" "--ignore" "html" "--ignore" "vendor/javascripts"
                     "--smart-case" "--column" "--nogroup" "--"))
(global-set-key (kbd "C-3") 'ag-project) ; Strg - 3

;; find in project
(global-set-key (kbd "C-1") 'fiplr-find-file)

;; list all ruby methods of current buffer
(global-set-key (kbd "C-c o") (lambda () (interactive) (occur "def")))

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
(global-set-key "\C-c\C-q" 'hs-show-all)
(global-set-key "\C-c\C-h" 'hs-hide-block)
(global-set-key "\C-c\C-s" 'hs-show-block)
;; shift rechte Maustaste ...
(global-set-key (kbd "S-<mouse-3>") 'hs-toggle-hiding)

(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "should" "test" "context" "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))


;; git timemachine
(global-set-key [f12] 'git-timemachine)
;; magit-blame
(global-set-key [f11] 'magit-blame-mode)

;; underscore word
(global-set-key (kbd "M--") 'string-inflection-underscore)


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

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\html\\.erb\\'" . web-mode))


;; Cycle between snake case, camel case, etc.
(require 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles

;; universal ctags

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
;; Setup auto update now
(add-hook 'prog-mode-hook
  (lambda ()
    (add-hook 'after-save-hook
              'counsel-etags-virtual-update-tags 'append 'local)))


;;;;;;;;;;;;;;;;;;;;;
;; -- VARIABLES -- ;;
;;;;;;;;;;;;;;;;;;;;;

;; for emacs using ruby 1.9 and new hash syntax
(font-lock-add-keywords
 'ruby-mode
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
 '(blink-cursor-mode nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-auto-complete-chars nil)
 '(company-idle-delay 0.4)
 '(company-insertion-on-trigger (quote (quote company-explicit-action-p)))
 '(company-insertion-triggers nil)
 '(company-tooltip-limit 5)
 '(cua-delete-selection nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(custom-enabled-themes (quote (aalto-dark)))
 '(custom-safe-themes
   (quote
    ("ff8be9ed2696bf7bc999423d909a603cb23a9525bb43135c0d256b0b9377c958" "c07daf599a7d07a18faaf354d269c48d4462ff7dbdbcc3773950a4b37b003d80" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" default)))
 '(ecb-options-version "2.40")
 '(fill-column 90)
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" "cache" ".routes" ".rsync_cache" "tmp" "vcr_cassettes" "cms" "uploads" "vendor" "public" "node_modules"))
     (files
      ("*.zip" "*~" "*.html")))))
 '(flycheck-checkers
   (quote
    (ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-rubocoprc ".TTrubocop.yml")
 '(global-company-mode t)
 '(ido-enable-flex-matching t)
 '(js-indent-level 2)
 '(kill-ring-max 2000)
 '(linum-mode t t)
 '(magit-commit-arguments nil)
 '(markdown-command "/usr/bin/pandoc")
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 13400)
 '(package-selected-packages
   (quote
    (blackboard-theme color-theme-modern rubocop exwm nginx-mode dockerfile-mode highlight-indentation indent-tools json-mode flycheck web-mode markdown-mode markdown-mode+ markdown-preview-mode ghub magit-popup coffee-mode yaml-mode ag move-text sr-speedbar rw-hunspell rvm ruby-interpolation ruby-factory ruby-dev ruby-compilation ruby-block project-local-variables project-explorer javascript highline gitconfig git-blame git flycheck-perl6 fixmee fiplr duplicate-thing company-inf-ruby color-theme)))
 '(rails-always-use-text-menus t)
 '(rails-chm-file nil)
 '(rails-default-environment "development")
 '(rails-enable-ruby-electric t)
 '(rails-indent-and-complete nil)
 '(rails-number-of-lines-shown-when-opening-log-file 130)
 '(rails-ri-command "ri")
 '(rails-tags-command "ctags -e --languages=ruby -o TAGS -R.")
 '(rails-test-command "bin/rails")
 '(rails-text-menu-function nil)
 '(rails-ws:default-server-type "mongrel")
 '(rails-ws:port "3000")
 '(rails-ws:server-name "http://localhost")
 '(ruby-end-insert-newline nil)
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . iso-8859-1)
     (buffer-file-coding-system . utf-8)
     (encoding . utf-8))))
 '(sh-basic-offset 2)
 '(speedbar-default-position (quote right))
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag t)
 '(sr-speedbar-default-width 30)
 '(tool-bar-mode nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-markup-indent-offset 2)
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
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 107 :width normal))))
 '(font-lock-constant-face ((nil (:foreground "#FBDE2D"))))
 '(font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
 '(font-lock-operator-face ((nil (:foreground "#FBDE2D"))))
 '(font-lock-variable-name-face ((nil (:foreground "#FBDE2D" :weight semi-bold)))))
