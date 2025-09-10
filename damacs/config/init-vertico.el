;;; init-vertico.el --- Vertico configuration -*- lexical-binding: t -*-

(when (require 'vertico nil t)
  ;; Basic Vertico behavior
  (setq vertico-cycle t          ; Cycle at list boundaries
        vertico-resize t         ; Grow/shrink minibuffer as needed
        vertico-count 15)        ; Number of candidates to show

  ;; Enable Vertico globally
  (vertico-mode 1)

  ;; File path editing helpers from Vertico's extensions
  (when (require 'vertico-directory nil t)
    ;; Clean up shadowed paths (e.g., /../) while typing
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    ;; Intuitive directory navigation in the minibuffer
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
      (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
      (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)))

  ;; Quick selection using Avy-style labels (built-in extension)
  (when (require 'vertico-quick nil t)
    ;; Exit with selection
    (define-key vertico-map (kbd "C-'") #'vertico-quick-exit)
    ;; Insert candidate, keep session
    (define-key vertico-map (kbd "C-;") #'vertico-quick-insert)))

(provide 'init-vertico)
