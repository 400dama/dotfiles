;;; init-embark.el --- Embark actions and integration -*- lexical-binding: t -*-

(when (require 'embark nil t)
  ;; Use Embark for prefix help (like describe-bindings, but contextual)
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Convenient global keys
  ;; - C-;: prompt for actions on target at point/minibuffer selection
  ;; - C-.: do-what-I-mean default action
  (global-set-key (kbd "C-;") #'embark-act)
  (global-set-key (kbd "C-.") #'embark-dwim)
  (global-set-key (kbd "C-h B") #'embark-bindings)
  
  ;; Minibuffer helpers
  (define-key minibuffer-local-map (kbd "C-c C-o") #'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-c") #'embark-collect)
  ;; Live-updating collect buffer (aka collect-live)
  (define-key minibuffer-local-map (kbd "C-c C-l") #'embark-live)

  ;; Indicator style: mixed gives a quick echo and a popup after a delay
  (setq embark-indicators
        '(embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(provide 'init-embark)
