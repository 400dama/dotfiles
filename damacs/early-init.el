;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; 禁用软件包管理器初始化
(setq package-enable-at-startup nil)

;; 提高 GC 阈值，加快启动速度，并禁用文件名处理器以加快加载
(defvar damacs--gc-cons-threshold gc-cons-threshold)
(defvar damacs--gc-cons-percentage gc-cons-percentage)
(defvar damacs--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; 禁用菜单栏、工具栏和滚动条
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 禁用启动画面
(setq inhibit-startup-screen t)

;; 禁用文件对话框
(setq use-file-dialog nil)

;; 禁用对话框
(setq use-dialog-box nil)

;; 禁止自动调整frame大小
(setq frame-inhibit-implied-resize t)

;; 初始化完成后恢复 GC 与文件名处理器
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (or damacs--gc-cons-threshold (* 64 1024 1024))
                  gc-cons-percentage (or damacs--gc-cons-percentage 0.1)
                  file-name-handler-alist damacs--file-name-handler-alist)))

;; 禁用 site-run-file
(setq site-run-file nil)

;; 不要压缩字体缓存文件
(setq inhibit-compacting-font-caches t)

;; 原生编译设置
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil) ; 禁用原生编译警告
  (setq native-comp-deferred-compilation t))          ; 启用延迟编译

;; 设置更大的读取进程输出的初始值
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; 禁用备份文件
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 减少 IO 操作频率
(setq auto-save-interval 1000)
(setq auto-save-timeout 30)

;; 添加 config 目录到 load-path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(provide 'early-init)
