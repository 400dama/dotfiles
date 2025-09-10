;;; init-marginalia.el --- Marginalia annotations for minibuffer -*- lexical-binding: t -*-

(when (require 'marginalia nil t)
  ;; Align annotations to the right edge of the minibuffer
  (setq marginalia-align 'right
        marginalia-align-offset 0
        ;; Hide remote file annotations; show only TRAMP method (e.g., ssh)
        marginalia-remote-file-regexps '("\\`/\\([^/|:]+\\):"))

  ;; Enable rich annotations in the minibuffer
  (marginalia-mode 1)

  ;; Cycle annotators quickly from the minibuffer
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))

;; Icons in completion using nerd-icons-completion (plays well with Marginalia)
(when (require 'nerd-icons-completion nil t)
  (nerd-icons-completion-mode 1)
  (with-eval-after-load 'marginalia
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(provide 'init-marginalia)
