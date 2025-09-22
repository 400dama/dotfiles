;;; init-python.el --- Python development setup -*- lexical-binding: t -*-

(defgroup damacs-python nil
  "Python development settings for damacs."
  :group 'languages)

(defcustom damacs-python-format-on-save t
  "Format Python buffers on save if a formatter is available."
  :type 'boolean
  :group 'damacs-python)

(defcustom damacs-python-isort-on-save nil
  "Run isort before formatting when saving."
  :type 'boolean
  :group 'damacs-python)

(defcustom damacs-python-format-prefer 'auto
  "Preferred Python formatter: one of 'auto, 'ruff, or 'black."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Ruff" ruff)
                 (const :tag "Black" black))
  :group 'damacs-python)

(defun damacs--python-format-command ()
  "Return the formatter command list or nil.
The list is like (cmd . args) where args include reading from stdin."
  (pcase damacs-python-format-prefer
    ('ruff (when (executable-find "ruff") (cons "ruff" '("format" "-"))))
    ('black (when (executable-find "black") (cons "black" '("-q" "-"))))
    ('auto (or (and (executable-find "ruff") (cons "ruff" '("format" "-")))
               (and (executable-find "black") (cons "black" '("-q" "-")))))
    (_ nil)))

(defun damacs--python-apply-filter (program args)
  "Replace current buffer by running PROGRAM with ARGS on its contents."
  (let ((tmp (generate-new-buffer " *damacs-python*")))
    (unwind-protect
        (let ((status (apply #'call-process-region (point-min) (point-max)
                              program nil tmp nil args)))
          (when (and (integerp status) (= status 0))
            (let ((pt (point)))
              (erase-buffer)
              (insert-buffer-substring tmp)
              (goto-char (min pt (point-max))))
            t))
      (kill-buffer tmp))))

(defun damacs--python-run-isort ()
  "Sort imports using isort if available."
  (when (executable-find "isort")
    (damacs--python-apply-filter "isort" '("-"))))

(defun damacs--python-run-formatter ()
  "Format buffer using preferred formatter if available."
  (when-let* ((cmd (damacs--python-format-command))
              (prog (car cmd))
              (args (cdr cmd)))
    (damacs--python-apply-filter prog args)))

(defun damacs/python-format-buffer ()
  "Sort imports (optional) and format current Python buffer."
  (interactive)
  (let* ((did-isort (and damacs-python-isort-on-save (damacs--python-run-isort)))
         (did-format (damacs--python-run-formatter)))
    (unless (or did-isort did-format)
      (message "No Python formatter found (install ruff or black)."))))

(defun damacs/python-before-save ()
  (when damacs-python-format-on-save
    (damacs/python-format-buffer)))

(with-eval-after-load 'lsp-bridge
  (setq lsp-bridge-python-lsp-server "basedpyright"
        lsp-bridge-python-multi-lsp-server "basedpyright_ruff"))

(defun damacs/python-setup ()
  "Buffer-local setup for Python editing."
  (setq-local indent-tabs-mode nil
              tab-width 4)
  (when (boundp 'python-indent-offset)
    (setq-local python-indent-offset 4))
  (let* ((venv-root (locate-dominating-file default-directory ".venv"))
         (venv-python (and venv-root (expand-file-name ".venv/bin/python" venv-root))))
    (setq-local python-shell-interpreter
                (cond ((and venv-python (file-executable-p venv-python)) venv-python)
                      ((executable-find "python3"))
                      ((executable-find "python"))
                      (t "python"))))
  (add-hook 'before-save-hook #'damacs/python-before-save nil t)
  (condition-case err
      (when (require 'lsp-bridge nil t)
        ;; Prefer the basedpyright + ruff multi-server setup for diagnostics and completion.
        (lsp-bridge-mode 1))
    (error
     (message "damacs/python-setup: skipping lsp-bridge (%s)" (error-message-string err)))))

;; Prefer tree-sitter mode when available
(when (and (fboundp 'python-ts-mode)
           (boundp 'major-mode-remap-alist))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-f") #'damacs/python-format-buffer)
  (define-key python-mode-map (kbd "C-c C-i") #'damacs--python-run-isort))

;; Hook up for both classic and tree-sitter Python modes
(add-hook 'python-mode-hook #'damacs/python-setup)
(add-hook 'python-ts-mode-hook #'damacs/python-setup)

(provide 'init-python)
