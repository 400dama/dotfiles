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

(provide 'init-marginalia)
