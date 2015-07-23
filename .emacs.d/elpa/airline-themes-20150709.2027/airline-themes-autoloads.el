;;; airline-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "airline-themes" "airline-themes.el" (21927
;;;;;;  46401 545488 122000))
;;; Generated autoloads from airline-themes.el

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(defvar airline-helm-colors t "\
Set helm colors to match the airline theme.

Valid Values: Enabled, Disabled")

(custom-autoload 'airline-helm-colors "airline-themes" t)

(defvar airline-cursor-colors t "\
Set the cursor color based on the current evil state.

Valid Values: Enabled, Disabled")

(custom-autoload 'airline-cursor-colors "airline-themes" t)

(defvar airline-display-directory 'airline-directory-shortened "\
Display the currend directory along with the filename.

Valid Values: Full, Shortened, Disabled")

(custom-autoload 'airline-display-directory "airline-themes" t)

(autoload 'airline-themes-set-modeline "airline-themes" "\
Set the airline mode-line-format

\(fn)" t nil)

(autoload 'airline-themes-set-deftheme "airline-themes" "\
Set appropriate face attributes

\(fn THEME-NAME)" nil nil)

(autoload 'shorten-directory "airline-themes" "\
Show up to `max-length' characters of a directory name `dir'.

\(fn DIR MAX-LENGTH)" nil nil)

;;;***

;;;### (autoloads nil nil ("airline-badwolf-theme.el" "airline-base16-gui-dark-theme.el"
;;;;;;  "airline-base16-gui-light-theme.el" "airline-base16-shell-dark-theme.el"
;;;;;;  "airline-behelit-theme.el" "airline-dark-theme.el" "airline-durant-theme.el"
;;;;;;  "airline-hybridline-theme.el" "airline-kalisi-theme.el" "airline-kolor-theme.el"
;;;;;;  "airline-light-theme.el" "airline-luna-theme.el" "airline-molokai-theme.el"
;;;;;;  "airline-papercolor-theme.el" "airline-powerlineish-theme.el"
;;;;;;  "airline-raven-theme.el" "airline-serene-theme.el" "airline-silver-theme.el"
;;;;;;  "airline-simple-theme.el" "airline-sol-theme.el" "airline-solarized-alternate-gui-theme.el"
;;;;;;  "airline-solarized-gui-theme.el" "airline-themes-pkg.el"
;;;;;;  "airline-ubaryd-theme.el" "airline-understated-theme.el"
;;;;;;  "airline-wombat-theme.el") (21927 46401 949478 6000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; airline-themes-autoloads.el ends here
