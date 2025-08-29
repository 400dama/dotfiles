;;; init-whichkey.el --- which-key popup help system -*- lexical-binding: t; -*-
(if (display-graphic-p)
    (when (and (require 'which-key nil t)
               (require 'posframe nil t)
               (require 'which-key-posframe nil t))
      ;; 原 use-package :custom-face
      (set-face-attribute 'which-key-posframe        nil :inherit 'tooltip)
      (set-face-attribute 'which-key-posframe-border nil :background "#504945")

      ;; 原 use-package :init 的变量
      (setq which-key-posframe-border-width 1
            which-key-posframe-poshandler 'posframe-poshandler-frame-center
            which-key-posframe-parameters '((left-fringe  . 5)
                                            (right-fringe . 5)))

      ;; 确保 which-key 打开，再启用 posframe 显示
      (unless (bound-and-true-p which-key-mode)
        (which-key-mode 1))
      (which-key-posframe-mode 1))
  ;; 终端环境：至少开 which-key，使用默认 side-window
  (when (require 'which-key nil t)
    (which-key-mode 1)))

(provide 'init-whichkey)
