;;; init-ui.el --- UI Initialization. -*- lexical-binding: t -*-

(require 'cl-lib)

;; doom-theme
(when (require 'doom-themes nil t)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark-high-contrast t))


;; doom-modeline
(require 'nerd-icons nil t)
(when (require 'doom-modeline nil t)
  (setq doom-modeline-height 30
        doom-modeline-bar-width 3
        doom-modeline-icon (featurep 'nerd-icons)
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-root
        doom-modeline-major-mode-color-icon t
        doom-modeline-lsp t
        doom-modeline-lsp-icon t
        doom-modeline-time t
        doom-modeline-time-icon nil
        doom-modeline-battery t)
  (setq display-time-format "%H:%M"
	display-time-default-load-average nil
	display-time-interval 60)
  (display-time-mode 1)
  (when (and (fboundp 'display-battery-mode)
             (not (string-match-p "^Power N/A" (battery))))
    (display-battery-mode 1))
  (add-hook 'after-init-hook #'doom-modeline-mode))
(setq inhibit-compacting-font-caches t)

;; 平滑滚动（Emacs 29+）
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-margin 1
      scroll-conservatively 101
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

;; 边框与分隔
(window-divider-mode 1)
(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1)
(set-fringe-mode 8)

;; 光标与高亮
(blink-cursor-mode 0)
(global-hl-line-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'mixed
      cursor-in-non-selected-windows nil)

(setq display-line-numbers-type t)   ; or 'relative / nil
(global-display-line-numbers-mode 1)

;; 不在这些模式显示行号
(dolist (m '(term-mode vterm-mode eshell-mode shell-mode help-mode))
  (add-hook (intern (format "%s-hook" m)) (lambda () (display-line-numbers-mode 0))))

(setq-default truncate-lines t)      ; 默认不自动换行
;; 尾随空格仅在编程/文本模式提示
(dolist (h '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook h (lambda () (setq show-trailing-whitespace t))))

;; 等宽
(defvar damacs-font-size (if (eq system-type 'darwin) 14 12))
(let* ((mono (cl-find-if (lambda (n) (find-font (font-spec :name n)))
                         '("JetBrains Mono" "Fira Code" "Iosevka" "Menlo" "Consolas" "DejaVu Sans Mono")))
       (f (and mono (format "%s-%d" mono damacs-font-size))))
  (when f (add-to-list 'default-frame-alist (cons 'font f))
        (set-face-attribute 'default nil :font f)))

;; CJK
(dolist (cn '("Sarasa Mono SC" "LXGW WenKai Mono" "Noto Sans CJK SC" "PingFang SC"))
  (when (find-font (font-spec :name cn))
    (set-fontset-font t 'han cn nil 'append)))

;; Emoji
(dolist (emo '("Noto Color Emoji" "Apple Color Emoji"))
  (when (find-font (font-spec :name emo))
    (set-fontset-font t 'emoji emo nil 'prepend)))

(setq ring-bell-function #'ignore
      use-dialog-box nil)           ; 不弹 GUI 对话框
(setq frame-title-format '("%b — " mode-name "  " (:eval (abbreviate-file-name default-directory))))

;; 提示只读、不可光标进入
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(provide 'init-ui)
