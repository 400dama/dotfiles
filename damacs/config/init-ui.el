;;; init-ui.el --- UI Initialization. -*- lexical-binding: t -*-

(require 'cl-lib)

;; doom-theme
(when (require 'doom-themes nil t)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark-high-contrast t))


(provide 'init-ui)
