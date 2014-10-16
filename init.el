;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(evil
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-surround
    color-theme
    color-theme-solarized
    git-commit-mode
    helm
    helm-projectile
    grizzl
    iflipb
    smooth-scrolling
    powerline-evil
    web-mode
    haml-mode
    slim-mode
    coffee-mode
    magit
    diminish
    editorconfig
    layout-restore)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace t)

(set-default 'truncate-lines t)
(setq-default evil-cross-lines t)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(global-evil-surround-mode 1)

(require 'evil)
(evil-mode 1)

(require 'helm-config)
(helm-mode 1)

(require 'color-theme-solarized)
(color-theme-solarized-dark)
(set-face-attribute 'helm-selection nil :background "#441100")

(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'grizzl)
(setq projectile-enable-caching t)

(require 'iflipb)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq helm-split-window-default-side 'above)

(require 'helm-projectile)
(defun helm-projectile-recentf (&optional arg)
  "Use projectile with Helm instead of ido.
With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump."
  (interactive "P")
  (if (projectile-project-p)
      (projectile-maybe-invalidate-cache arg))
  (let ((helm-ff-transformer-show-only-basename nil)
        (src (if (projectile-project-p)
                 '(helm-source-projectile-recentf-list)
               helm-source-projectile-projects)))
    (helm :sources src
          :buffer "*helm projectile*"
          :prompt (projectile-prepend-project-name (if (projectile-project-p)
                                                       "pattern: "
                                                       "Switch to project: ")))))

; Buffer nagiation keys
(evil-leader/set-key
  "r" 'helm-projectile-recentf
  "R" 'helm-recentf
  "t" 'helm-projectile
  "y" 'switch-to-buffer
  "/" 'iflipb-next-buffer
  "w" 'kill-this-buffer)

(defun my-window-split ()
  "Split the window horizontally and put cursor in the bottom one"
  (interactive)
  (evil-window-split)
  (evil-window-down 1))

(defun my-window-vsplit ()
  "Split the window vertically and put cursor in the right one"
  (interactive)
  (evil-window-vsplit)
  (evil-window-right 1))

(require 'layout-restore)
(global-set-key [?\C-c ?l] 'layout-save-current)
(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)

(defun zoom-win ()
  "Show only the current window, or restore previous window layout."
  (interactive)
  (if layout-configuration-alist
    (progn
      (layout-restore)
      (layout-delete-current))
    (progn
      (layout-save-current)
      (delete-other-windows))))

; Window navigation keys
(evil-leader/set-key
  "v" 'my-window-vsplit
  "s" 'my-window-split
  "q" 'delete-window
  "o" 'zoom-win
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "H" 'evil-window-move-far-left
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "L" 'evil-window-move-far-right)

; Paste from kill ring
(evil-leader/set-key
  ";" 'helm-show-kill-ring)

; Git keys
(evil-leader/set-key
  "gs" 'magit-status
  "gb" 'vc-annotate)

(require 'powerline)
(powerline-evil-vim-color-theme)

(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "helm" '(diminish 'helm-mode))

(show-paren-mode t)

; place backup and auto-save files in my ~/tmp directory
(defvar user-temporary-file-directory "~/tmp")
(setq backup-by-copying t
      backup-directory-alist
        '(("." . "~/tmp"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

; no lock files please (I live dangerously)
(setq create-lockfiles nil)

; remember cursor position when re-opening a file
(setq save-place-file "~/tmp/emacs-saveplace")
(setq-default save-place t)
(require 'saveplace)

(add-to-list 'load-path "~/.emacs.d/lib")

; directory tree view
(evil-leader/set-key
  "p" 'projectile-dired
  "P" 'dired-jump)

; Firefox Reload via MozRepl
(require 'moz)

(defun moz-firefox-reload ()
  "Reload Firefox by sending command through MozRepl"
  (interactive)
  (comint-send-string (inferior-moz-process) "BrowserReload();"))

(evil-leader/set-key
  "fr" 'moz-firefox-reload)

; Two Spaces, No Tabs
(setq-default
  tab-width 2
  indent-tabs-mode nil
  evil-shift-width 2)

; Auto Indent
(defun enable-paste-mode ()
  "Disable auto indent"
  (interactive)
  (define-key global-map (kbd "RET") 'newline))
(defun disable-paste-mode ()
  "Reenable auto indent"
  (interactive)
  (define-key global-map (kbd "RET") 'newline-and-indent))
(disable-paste-mode)

; ERB
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

; CJSX as CoffeeScript
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode))

; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

; Undo/Redo
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '((".*" . "~/tmp/emacs-undo")))

(evil-leader/set-key
  "u" 'undo-tree-visualize)
(global-set-key (kbd "C-r") 'undo-tree-redo)

(add-hook 'ruby-mode-hook
  (lambda () (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)))

(add-hook 'emacs-lisp-mode-hook
  (lambda () (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)))

(global-set-key (kbd "C-k") (lambda () (interactive))) ; no-op

; Nerd Comments
(evilnc-default-hotkeys)

(setq initial-major-mode (quote text-mode))

(defun new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(evil-leader/set-key
  "n" 'new-empty-buffer)

(setq inhibit-startup-screen t)
