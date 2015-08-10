;;; bundler-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bundle-gemfile bundle-exec bundle-update bundle-install
;;;;;;  bundle-check bundle-console bundle-open) "bundler" "bundler.el"
;;;;;;  (21960 21290 529280 762000))
;;; Generated autoloads from bundler.el

(autoload 'bundle-open "bundler" "\
Queries for a gem name and opens the location of the gem in dired.

\(fn GEM-NAME)" t nil)

(autoload 'bundle-console "bundler" "\
Run an inferior Ruby process in the context of the current bundle.

\(fn)" t nil)

(autoload 'bundle-check "bundler" "\
Run bundle check for the current bundle.

\(fn)" t nil)

(autoload 'bundle-install "bundler" "\
Run bundle install for the current bundle.

\(fn)" t nil)

(autoload 'bundle-update "bundler" "\
Run bundle update for the current bundle.

\(fn &optional UPDATE-CMD-ARGS)" t nil)

(autoload 'bundle-exec "bundler" "\


\(fn COMMAND)" t nil)

(autoload 'bundle-gemfile "bundler" "\
Set BUNDLE_GEMFILE environment variable.

\(fn &optional GEMFILE)" t nil)

;;;***

;;;### (autoloads nil nil ("bundler-pkg.el") (21960 21290 540909
;;;;;;  786000))

;;;***

(provide 'bundler-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bundler-autoloads.el ends here
