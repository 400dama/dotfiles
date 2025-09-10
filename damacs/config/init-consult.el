;;; init-consult.el --- Consult configuration for Vertico stack -*- lexical-binding: t -*-

(when (require 'consult nil t)
  ;; Use Consult for in-region completion so it works nicely with Orderless/Vertico
  (when (fboundp 'consult-completion-in-region)
    (setq completion-in-region-function #'consult-completion-in-region))

  ;; Preview behavior and performance
  (setq consult-preview-key '(:debounce 0.3 any))
  ;; Turn off preview for heavy async greps; toggle on demand with M-.
  (consult-customize consult-ripgrep consult-git-grep consult-grep :preview-key nil)

  ;; Async throttling for snappy, low-CPU searches
  (setq consult-async-min-input 2
        consult-async-input-debounce 0.15
        consult-async-input-throttle 0.2)

  ;; Project integration (built-in project.el)
  ;; Used by consult-buffer and consult-grep when available
  (setq consult-project-function #'consult--default-project-function)

  ;; Narrow / widen keys in the minibuffer
  (setq consult-narrow-key "<"
        consult-widen-key ">")

  ;; Xref UI via Consult
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  ;; Register preview via Consult
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.25
        register-preview-function #'consult-register-format)

  ;; Embark Collect: enable preview of candidates at point
  (with-eval-after-load 'embark
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  ;; Handy keybindings (conservative, avoid overriding C-s/C-r by default)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "M-y")   #'consult-yank-pop)
  (global-set-key (kbd "M-g g") #'consult-goto-line)
  (global-set-key (kbd "M-g M-g") #'consult-goto-line)
  (global-set-key (kbd "M-g i") #'consult-imenu)
  (global-set-key (kbd "M-s l") #'consult-line)
  (global-set-key (kbd "M-s g") #'consult-grep)
  (global-set-key (kbd "M-s r") #'consult-ripgrep)
  (global-set-key (kbd "C-x r b") #'consult-bookmark)

  ;; Remap incremental search to Consult-based line search
  (defun consult-line-from-top ()
    "Search with `consult-line' starting from buffer top."
    (interactive)
    (consult-line nil t))
  (global-set-key [remap isearch-forward]  #'consult-line)
  (global-set-key [remap isearch-backward] #'consult-line-from-top)

  ;; consult-dir: quickly switch between known directories during file prompts
  (when (require 'consult-dir nil t)
    (define-key minibuffer-local-filename-completion-map (kbd "C-x C-d") #'consult-dir)
    (define-key minibuffer-local-filename-completion-map (kbd "C-x C-j") #'consult-dir-jump-file)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
      (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file)))

  ;; Embark + Consult tighter integration
  (with-eval-after-load 'embark
    (require 'embark-consult nil t))

  ;; Note: We use Vertico Quick instead of consult-avy.

  ;; Enable writable grep buffers (edit and apply changes)
  (when (require 'wgrep nil t)
    ;; Optional: apply edits on C-c C-c without asking
    (setq wgrep-auto-save-buffer t))
  )

(provide 'init-consult)
