(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
;(menu-bar-mode -1)
(set-fringe-mode 10)

(setq auto-save-default nil
      scroll-step 1)

(setq-default tab-width 4)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-clients-clangd-args '("--header-insertion=never" "--header-insertion-decorators=0" "--enable-config" "-j=4")
      lsp-idle-delay 0.1)  ;; clangd is fast
(setq lsp-semantic-tokens-honor-refresh-requests t
      lsp-semantic-tokens-apply-modifiers nil)

(setq lsp-ui-doc-show-with-mouse nil
      lsp-ui-doc-enhanced-markdown nil
      lsp-ui-peek-always-show t)

(with-eval-after-load 'lsp-mode
		      (setq lsp-modeline-diagnostics-scope :workspace ;; :global/:workspace/:file
			    lsp-enable-on-type-formatting nil))
(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(setq projectile-enable-caching t
      projectile-require-project-root t)

(helm-mode 1)
(setq helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)
(global-set-key (kbd "M-x") 'helm-M-x)


(setq-default display-line-numbers 'relative)

(setq evil-want-C-u-scroll t
      evil-want-C-d-scroll t)
(require 'evil)
(evil-mode 1)

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))


(setq evil-normal-state-cursor '(box)
      evil-insert-state-cursor '(bar)
      evil-symbol-word-search 't)
      
(customize-set-variable 'evil-undo-system 'undo-fu)

(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-embrace-enable-evil-surround-integration)

(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'motion (kbd "SPC"))

(evil-define-key 'normal 'global (kbd "<C-tab>") 'other-frame)

(evil-define-key 'normal 'global (kbd "<f7>") 'projectile-compile-project)

(evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key 'normal 'global (kbd "gf") 'lsp-find-declaration)
(evil-define-key 'normal 'global (kbd "go") 'lsp-clangd-find-other-file)

(evil-define-key 'normal 'global (kbd "<leader><SPC>") 'avy-goto-word-1)

(evil-define-key 'normal 'global (kbd "<leader>cn") 'next-error)
(evil-define-key 'normal 'global (kbd "<leader>cp") 'previous-error)

(evil-define-key 'normal 'global (kbd "<leader>gg") 'helm-git-grep)
(evil-define-key 'normal 'global (kbd "<leader>ga") 'helm-git-grep-at-point)
(evil-define-key 'normal 'global (kbd "<leader>gp") 'git-gutter:previous-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gn") 'git-gutter:next-hunk)
(evil-define-key 'normal 'global (kbd "<leader>gd") 'git-gutter:popup-diff)

(evil-define-key 'normal 'global (kbd "<leader>pd") 'lsp-ui-peek-find-definitions)
(evil-define-key 'normal 'global (kbd "<leader>pc") 'lsp-ui-doc-glance)
(evil-define-key 'normal 'global (kbd "<leader>pr") 'lsp-ui-peek-find-references)
(evil-define-key 'normal 'global (kbd "<leader>pi") 'lsp-ui-peek-find-implementation)

;; (evil-define-key 'normal 'global (kbd "<leader>so") 'lsp-ui-imenu)
(evil-define-key 'normal 'global (kbd "<leader>fb") 'helm-mini)
(evil-define-key 'normal 'global (kbd "<leader>ff") 'helm-projectile-find-file)
(evil-define-key 'normal 'global (kbd "<leader>fr") 'lsp-treemacs-references)
;; (evil-define-key 'normal 'global (kbd "<leader>fr") 'lsp-find-references)
(evil-define-key 'normal 'global (kbd "<leader>fo") 'helm-imenu)
(evil-define-key 'normal 'global (kbd "<leader>fs") 'helm-lsp-workspace-symbol)
(evil-define-key 'normal 'global (kbd "<leader>fi") 'lsp-find-implementation)
(evil-define-key 'normal 'global (kbd "<leader>fc") 'lsp-treemacs-call-hierarchy)

(evil-define-key 'normal 'global (kbd "<leader>nd") 'narrow-to-defun)
(evil-define-key 'normal 'global (kbd "<leader>np") 'narrow-to-page)
(evil-define-key 'visual 'global (kbd "<leader>nr") 'narrow-to-region)
(evil-define-key 'normal 'global (kbd "<leader>nw") 'widen)

(evil-define-key 'normal 'global (kbd "<leader>ms") 'magit-status)
(evil-define-key 'normal 'global (kbd "<leader>ml") 'magit-log)

(evil-define-key 'normal 'global (kbd "<leader>ww") 'popper-toggle-latest)
(evil-define-key 'normal 'global (kbd "<leader>wn") 'popper-cycle)
(evil-define-key 'normal 'global (kbd "<leader>wp") 'popper-toggle-type)
(evil-define-key 'motion 'global (kbd "<leader>ww") 'popper-toggle-latest)
(evil-define-key 'motion 'global (kbd "<leader>wn") 'popper-cycle)
(evil-define-key 'motion 'global (kbd "<leader>wp") 'popper-toggle-type)

(require 'evil-exchange)
(evil-exchange-install)

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

(require 'auto-save-buffers-enhanced)
(auto-save-buffers-enhanced-include-only-checkout-path t)
(auto-save-buffers-enhanced t)
(add-hook 'evil-insert-state-entry-hook 'auto-save-buffers-enhanced-toggle-activity)
(add-hook 'evil-insert-state-exit-hook 'auto-save-buffers-enhanced-toggle-activity)

(custom-set-variables
 '(evil-undo-system 'undo-fu)
 '(gdb-many-windows t)
 '(helm-completion-style 'helm)
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(xterm-color elfeed which-key popper git-gutter helm-git-grep compat magit evil-embrace vscode-dark-plus-theme evil-exchange undo-fu helm-lsp evil-surround helm-projectile projectile helm qml-mode cmake-mode treemacs-evil spacemacs-theme rtags-xref nord-theme lsp-ui lsp-treemacs levenshtein gruvbox-theme flycheck-rtags farmhouse-theme dracula-theme doom-themes company color-theme-sanityinc-tomorrow clang-format+ auto-save-buffers-enhanced auto-complete atom-one-dark-theme afternoon-theme))
)
 
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(global-git-gutter-mode +1)

(evil-set-initial-state 'eww 'emacs)
(evil-set-initial-state 'Magit-Blame 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)

(setq popper-reference-buffers
      '(;; "\\*LSP Lookup\\*"
        help-mode
        compilation-mode))
;; (setq popper-group-function #'popper-group-by-projectile)

(popper-mode +1)
(popper-echo-mode +1)

(which-key-mode)

;; compilation buffer coloring
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)
