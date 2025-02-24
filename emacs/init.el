;; Elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
(elpaca-no-symlink-mode)

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Necessary to use the `:elpaca' use-package keyword at the top-level.
(elpaca use-package)
(elpaca-wait)
;; end of Elpaca

(when (eq system-type 'gnu/linux)
  ;; pgtk is only available in Emacs 29+
  ;; without it Emacs fonts don't scale properly on
  ;; HiDPI display
  (if (< emacs-major-version 29)
      (set-frame-font "Cascadia Code 28")
    (set-frame-font "Cascadia Code 13"))

  (when (getenv "WSLENV")
	;; Teach Emacs how to open links in your default Windows browser
	(let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
		  (cmd-args '("/c" "start")))
	  (when (file-exists-p cmd-exe)
		(setq browse-url-generic-program  cmd-exe
			  browse-url-generic-args     cmd-args
			  browse-url-browser-function 'browse-url-generic
			  search-web-default-browser 'browse-url-generic)))))

(setq inhibit-startup-message t) ; Do not show the startup screen
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable menubar
(setq default-frame-alist '((undecorated . t) (fullscreen . maximized)))
(set-fringe-mode 10)
(setq auto-save-default nil
	  scroll-step 1
	  gc-cons-threshold (* 100 1024 1024)
	  read-process-output-max (* 1024 1024))
(setq-default tab-width 4)
(setq-default display-line-numbers 'relative)
(save-place-mode 1)
(global-auto-revert-mode 1)

(with-eval-after-load 'compile
  ;; set cursor to follow compilation output
  (setq compilation-scroll-output t
		compilation-ask-about-save nil))

;; Packages
(use-package general :demand t)
(elpaca-wait)

(general-create-definer global-evil
  :keymaps 'override
  :states '(normal visual motion))

(general-create-definer global-evil-leader
  :keymaps 'override
  :states '(normal visual)
  :prefix "SPC")

(global-evil
  "<f8>" 'recompile
  "<C-tab>" 'evil-switch-to-windows-last-buffer)

(global-evil-leader
  :infix "c"
  "c" 'comment-or-uncomment-region)

(global-evil-leader
  :infix "n"
  "d" 'narrow-to-defun
  "p" 'narrow-to-page
  "r" 'narrow-to-region
  "w" 'widen)

(global-evil-leader
  :infix "d"
  "j" 'next-error
  "k" 'previous-error)

(use-package evil :demand t
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-C-i-jump nil)
  (evil-symbol-word-search t "search by symbol with * and #.")
  :config
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'xref--transient-buffer-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'flycheck-error-list-mode 'emacs)
  (evil-set-initial-state 'elpaca-manager-mode 'emacs)
  (evil-set-initial-state 'elpaca-info-mode 'emacs)
  (evil-set-initial-state 'elpaca-log-mode 'emacs)
  (evil-mode 1))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-args
  :after (evil)
  :bind (:map evil-inner-text-objects-map
			  ("a" . evil-inner-arg)
			  :map evil-outer-text-objects-map
			  ("a" . evil-outer-arg))
  :general
  (global-evil
	"M-l" 'evil-forward-arg
	"M-h" 'evil-backward-arg
	"M-k" 'evil-jump-out-args))

(use-package embrace)

(use-package evil-embrace 
  :after (evil embrace evil-surround)
  :hook (c++-mode . (lambda () (embrace-add-pair-regexp ?C "\\(\\w\\|\\s_\\)+?<" ">" '
														(lambda ()
														  (let ((fname (read-string "Generic type: ")))
															(cons (format "std::%s<" (or fname "")) ">")))
														(embrace-build-help "std::TYPE<" ">"))))
  :general
  (global-evil-leader
	"e" 'embrace-commander)
  :config
  (evil-embrace-enable-evil-surround-integration))

(use-package anzu
  :defer 10
  :config (global-anzu-mode))

(use-package evil-anzu
  :after (evil anzu))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package evil-exchange
  :after (evil)
  :config
  (evil-exchange-install))

(use-package company
  :config
  (setq company-minimum-prefix-length 1))

(use-package which-key
  :config
  (which-key-mode))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package magit
  :general
  (global-evil-leader
	:infix "m"
	"b" 'magit-blame
	"l" 'magit-log
	"s" 'magit-status)
  :config
  (evil-set-initial-state 'Magit-Blame 'emacs))

(use-package helm
  :defer 1
  :general
  (global-evil
	"<f2>" 'helm-semantic-or-imenu)
  (global-evil-leader
	"bb" 'helm-mini
	"ff" 'helm-find-files
	"s" 'helm-google-suggest
	"y" 'helm-show-kill-ring
	"/" 'helm-occur)
  :custom
  (helm-candidate-number-limit 300)
  (helm-move-to-line-cycle-in-source t "cycle to beggining or end afte reaching top/bottom of list")
  (helm-completion-style 'emacs "Necessary to have multiline candidates/text-properties show in completion buffer")
  (helm-google-suggest-search-url "https://www.google.com/search?q=%s")
  :config
  (helm-mode 1))
(global-set-key (kbd "M-x") 'helm-M-x)
(setq
 helm-mode-fuzzy-match t
 helm-completion-in-region-fuzzy-match t
 helm-allow-mouse nil)

(use-package helm-git-grep
  :general
  (global-evil-leader
	"ga" 'helm-git-grep-at-point
	"gg" 'helm-git-grep))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :general
  (global-evil-leader
	"gd" 'git-gutter:popup-diff
	"gj" '(git-gutter:next-hunk :properties (:repeat t :jump t))
	"gk" '(git-gutter:previous-hunk :repeat t :jump t))
  :custom
  (git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package doom-modeline
  :defer 2
  :config
  (column-number-mode 1)
  (doom-modeline-mode)
  :custom
  (doom-modeline-icon t "Show icons in the modeline"))

;; Run M-x nerd-icons-install-fonts to install the necessary fonts.
(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package all-the-icons
  :if (display-graphic-p))

;; https://github.com/magit/forge

(use-package projectile
  :general
  (global-evil
	"<f7>" 'projectile-compile-project)
  :custom
  (projectile-require-project-root t)
  :config
  (projectile-mode))

(use-package helm-projectile
  :general
  (global-evil
	"M-o" 'helm-projectile-find-other-file)
  (global-evil-leader
	"pb" 'helm-projectile-switch-to-buffer
	"pf" 'helm-projectile-find-file
	"pp" 'helm-projectile-switch-project)
  :config
  (helm-projectile-on))

(use-package vterm
  :ensure t
  :general
  (global-evil-leader
	"t" 'vterm)
  :config
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode))

(use-package flycheck
  :commands (flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit "necessary with alternatives to package.el"))

(use-package lsp-mode
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
		 (lambda () (setq lsp-idle-delay 0.1)))
  :general
  (global-evil-leader
	"cr" 'lsp-rename
	"ca" 'lsp-execute-code-action)
  :config
  (setq lsp-modeline-diagnostics-scope :file ; :global/:workspace/:file
		lsp-semantic-tokens-honor-refresh-requests t
		lsp-semantic-tokens-apply-modifiers nil
		lsp-clients-clangd-args '("--header-insertion=never" "--header-insertion-decorators=0" "--enable-config" "-j=4" "--malloc-trim")))
;; TODO: use :bind inside use-package instead of the mapping below
(add-hook 'lsp-mode
          (lambda ()
			(evil-local-set-key 'normal (kbd "gd") 'lsp-find-definition)
			(evil-local-set-key 'normal (kbd "gf") 'lsp-find-declaration)))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode
  :config
  (evil-set-initial-state 'lsp-ui-imenu 'emacs))

(use-package helm-lsp
  :general
  (global-evil-leader
	"cs" 'helm-lsp-workspace-symbol)
  :commands helm-lsp-workspace-symbol)

(use-package ccls
  :config
  (setq lsp-prefer-flymake nil))

(use-package avy
  :general
  (global-evil-leader
	"SPC" 'avy-goto-char-timer))

(use-package firestarter)

(use-package apheleia
  :general
  (global-evil-leader
    "af" 'apheleia-format-buffer
    "at" 'apheleia-mode))

(use-package auto-save-buffers-enhanced
  :hook ((evil-insert-state-entry evil-insert-state-exit) . (lambda () (auto-save-buffers-enhanced-toggle-activity)))
  :config
  (auto-save-buffers-enhanced-include-only-checkout-path t)
  (auto-save-buffers-enhanced t))

(use-package gptel
  :config
  (add-to-list
   'gptel-directives
   '(cli . "You are a command line helper. Generate command line commands that do what is requested, without any additional description or explanation. Generate ONLY the command, I will edit it myself before running.")
   :append))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))
(put 'narrow-to-region 'disabled nil)

(use-package rust-mode
  :hook ((rust-mode) . (lambda () (setq indent-tabs-mode nil)))
  :config
  (setq rust-format-on-save t))

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current))

(use-package web-mode)

(setq display-buffer-alist
	  '(
		((or (derived-mode . compilation-mode)
			 (derived-mode . flycheck-error-list-mode))
		 (display-buffer-reuse-mode-window
		  display-buffer-below-selected)
		 (dedicated . t)
		 )))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(load-file "~/.emacs.d/init-specific.el")

(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)
