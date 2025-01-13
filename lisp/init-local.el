;;; init-local --- Personal settings which have nowhere else to live

;;; Commentary:

;;; Code:

;; Bar cursor
(setq-default cursor-type 'bar)

;; Stop native compiler complaining about everything
(setq native-comp-async-report-warnings-errors 'silent)

;; Background alpha for transparency (does not work on mac :/)
(set-frame-parameter nil 'alpha-background 85) ; current frame
(add-to-list 'default-frame-alist '(alpha-background . 85)) ; future

;; Better go to beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Eglot bindings
(global-set-key (kbd "C-c C-a") #'eglot-code-actions)

;; LSP specific Eglot stuff
(setq-default eglot-workspace-configuration
              `((:pylsp . (:configurationSources ["flake8"]
                                                 :plugins (:pycodestyle (:enabled nil)
                                                                        :mccabe (:enabled nil)
                                                                        :pyflakes (:enabled nil)
                                                                        :flake8 (:enabled t))))))

;; Font
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 130)

(require 'use-package)

(setq use-package-always-ensure t)

(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0))

(use-package swiper
  :config
  (global-set-key (kbd "C-x s") 'swiper))

(use-package org-roam
  :init
  (setq org-roam-directory (file-truename "~/org-roam"))
  :config
  (org-roam-db-autosync-mode t))

(provide 'init-local)
;;; init-local.el ends here
