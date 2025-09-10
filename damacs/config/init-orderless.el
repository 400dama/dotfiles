;;; init-orderless.el --- Orderless + completion styles -*- lexical-binding: t -*-

(when (require 'orderless nil t)
  ;; Use Orderless everywhere with a sane fallback
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Built-in partial-completion improves file path segments like ~/s/f
        completion-category-overrides '((file (styles partial-completion))))

  ;; Treat spaces as separators between orderless components, with easy escaping
  (setq orderless-component-separator #'orderless-escapable-split-on-space)

  ;; Case-insensitive completion for files and buffers only
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t))

(provide 'init-orderless)
