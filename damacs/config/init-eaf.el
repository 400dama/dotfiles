;;; init-eaf.el --- EAF setup (submodule) -*- lexical-binding: t; -*-
(if (require 'eaf nil t)
    (progn
      ;; 不强制覆盖 eaf-python-command，直接信任来自 exec-path-from-shell 的 PATH
      ;; 若你希望显式指定，可在私有配置里设置：
      ;;   (setq eaf-python-command "python3") ; 将解析到 pixi 的 python

      ;; 常见个性化项（详见 Wiki Customization）
      ;; (setq eaf-browser-enable-adblocker t)
      (setq browse-url-browser-function 'eaf-open-browser)
      ;; (defalias 'browse-web #'eaf-open-browser)

      ;; 按需加载你要用的应用（示例挑常用几项）
      (require 'eaf-browser)
      (require 'eaf-pdf-viewer)
      (require 'eaf-file-manager)
      (require 'eaf-file-browser)
      (require 'eaf-markdown-previewer)
      (require 'eaf-git)
      (require 'eaf-pyqterminal)
      (require 'eaf-org-previewer)
      ;; 还可选：eaf-video-player eaf-image-viewer eaf-rss-reader eaf-git eaf-jupyter 等

      ;; ------- 按键映射示例 -------
      ;; PDF：用 C-n/C-p 翻页（官方推荐用 eaf-bind-key 修改各 app 的映射）。
      (eaf-bind-key scroll_up   "C-n" eaf-pdf-viewer-keybinding)
      (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
      ;; Browser：示例，解除 M-q（避免和你全局 M-q 冲突）
      (eaf-bind-key nil "M-q" eaf-browser-keybinding)

      ;; ------- 方便启动的一些全局快捷键（可改）-------
      (global-set-key (kbd "C-c e b") #'eaf-open-browser)
      (global-set-key (kbd "C-c e p") #'eaf-open)            ; 打开文件 -> 选择 pdf viewer/markdown 等
      (global-set-key (kbd "C-c e t") #'eaf-open-pyqterminal)
      (global-set-key (kbd "C-c e f") #'eaf-open-in-file-manager))
  (message "EAF not found on load-path; skipping EAF init"))


(provide 'init-eaf)
;;; init-eaf.el ends here
