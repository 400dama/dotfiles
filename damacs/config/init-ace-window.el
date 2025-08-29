;;; init-ace-window.el --- Main initialization file -*- lexical-binding: t -*-
(when (require 'ace-window nil t)
  ;; 快捷键：官方示例用 M-o
  (global-set-key (kbd "M-o") #'ace-window)  ;; 见 GNU ELPA 说明
  ;; 行为
  (setq aw-scope 'global
        aw-dispatch-always t)

  ;; 提示字符样式（放大更醒目）
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "#859900"
                      :background nil
                      :weight 'bold
                      :height 320)            ;; 放大字号的做法可参考社区讨论
  ;; GUI 下启用 posframe；TTY 自动退化为默认覆盖层
  (when (and (display-graphic-p)
             (require 'posframe nil t)
             (require 'ace-window-posframe nil t))  ;; 来自包内的 ace-window-posframe.el
    (setq aw-posframe-position-handler #'posframe-poshandler-window-center)
    (ace-window-posframe-mode +1)))

(provide 'init-ace-window)
