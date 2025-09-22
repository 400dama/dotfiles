;;; init-lsp-bridge.el --- lsp-bridge bootstrap -*- lexical-binding: t -*-

(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)

(setq lsp-bridge-enable-completion-in-minibuffer t)
(setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
(setq lsp-bridge-enable-with-tramp t)
(setq lsp-bridge-enable-org-babel t)
(setq acm-enable-capf t)
(setq acm-enable-quick-access t)
(setq acm-backend-yas-match-by-trigger-keyword t)
(setq acm-enable-tabnine nil)
(setq acm-enable-codeium nil)
(setq acm-enable-lsp-workspace-symbol t)
(setq lsp-bridge-enable-inlay-hint t)
(setq lsp-bridge-semantic-tokens t)
(setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types ["variable"])

(global-lsp-bridge-mode)

(add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("html") . "html_tailwindcss"))
(add-to-list 'lsp-bridge-multi-lang-server-extension-list '(("css") . "css_tailwindcss"))

(setq lsp-bridge-csharp-lsp-server "csharp-ls")
(setq lsp-bridge-nix-lsp-server "nil")


(setq lsp-bridge-get-multi-lang-server-by-project
      (lambda (project-path filepath)
        ;; If typescript file include deno.land url, then use Deno LSP server.
        (save-excursion
          (when (string-equal (file-name-extension filepath) "ts")
            (dolist (buf (buffer-list))
              (when (string-equal (buffer-file-name buf) filepath)
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
                    (return "deno")))))))))
(provide 'init-lsp-bridge)
