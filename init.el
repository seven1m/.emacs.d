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
    color-theme
    color-theme-solarized
    git-commit-mode
    helm
    helm-projectile
    grizzl
    iflipb
    smooth-scrolling
    powerline-evil
    neotree)
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

(set-default 'truncate-lines t)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

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

; Buffer nagiation keys
(evil-leader/set-key
  "r" 'helm-recentf
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

; Window navigation keys
(evil-leader/set-key
  "v" 'my-window-vsplit
  "s" 'my-window-split
  "q" 'delete-window
  "o" 'delete-other-windows
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "H" 'evil-window-move-far-left
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "L" 'evil-window-move-far-right)

(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)

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

; remember cursor position when re-opening a file
(setq save-place-file "~/tmp/emacs-saveplace")
(setq-default save-place t)
(require 'saveplace)

; Project drawer
(require 'neotree)
(evil-leader/set-key
  "p" 'neotree-toggle
  "P" 'neotree-find)

(global-set-key (kbd "C-r") 'undo-tree-redo)

(add-to-list 'load-path "~/.emacs.d/lib")
(require 'moz)

(defun moz-firefox-reload ()
  "Reload Firefox by sending command through MozRepl"
  (interactive)
  (comint-send-string (inferior-moz-process) "BrowserReload();"))

(evil-leader/set-key
  "fr" 'moz-firefox-reload)

(setq-default
  tab-width 2
  indent-tabs-mode nil)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Enable mouse support
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
