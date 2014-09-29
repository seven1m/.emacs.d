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

(require 'color-theme-solarized)
(color-theme-solarized-dark)

(require 'helm-config)
(helm-mode 1)

(projectile-global-mode)
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'grizzl)
(setq projectile-enable-caching t)

(require 'iflipb)

(recentf-mode 1)
(setq recentf-max-menu-items 100)

; Buffer nagiation keys
(evil-leader/set-key
  "r" 'projectile-recentf
  "t" 'projectile-find-file
  "y" 'projectile-switch-to-buffer
  "/" 'iflipb-next-buffer
  "w" 'kill-this-buffer)

; Window navigation keys
(evil-leader/set-key
  "v" 'evil-window-vsplit
  "s" 'evil-window-split
  "q" 'delete-window
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

; Project drawer
(require 'neotree)
(evil-leader/set-key
  "p" 'neotree-toggle
  "P" 'neotree-find)
