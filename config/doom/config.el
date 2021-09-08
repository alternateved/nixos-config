;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Enable autosave
(setq auto-save-default t
      make-backup-files t)

;; Saner defaults
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2
      display-line-numbers-type t)

;; Some sane settings in evil mode
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring


;; Custom minimal dashboard
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;; Lots of autocompletion
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-quick-access t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ; make aborting less annoying.

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Which-key improvements
(setq which-key-idle-delay 0.5)                   ; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; Zen improvements
(setq +zen-text-scale 0.7)

;; About me
(setq user-full-name "Tomasz Hołubowicz"
      user-mail-address "alternateved@gmail.com")

;; Font settings
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 16)
      doom-big-font (font-spec :family "Iosevka Nerd Font Mono" :size 23)
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 17))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Dictionary settings
(after! flyspell
  (setq flyspell-lazy-idle-seconds 2))

(after! ispell
  (setq ispell-program-name "aspell"))

;; Themes
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(setq doom-theme 'modus-vivendi)
(setq modus-themes-slanted-constructs nil
      modus-themes-bold-constructs t
      modus-themes-org-blocks 'gray-background)

(defun synchronize-theme ()
  (let* ((light-theme 'modus-operandi)
         (dark-theme 'modus-vivendi)
         (start-time-light-theme 6)
         (end-time-light-theme 18)
         (hour (string-to-number (substring (current-time-string) 11 13)))
         (next-theme (if (member hour (number-sequence start-time-light-theme end-time-light-theme))
                         light-theme dark-theme)))
    (when (not (equal doom-theme next-theme))
      (setq doom-theme next-theme)
      (load-theme next-theme))))

(run-with-timer 0 900 'synchronize-theme)

(use-package! doom-modeline
  :custom (doom-modeline-height 15)
          (doom-modeline-major-mode-icon t))

;; Org-mode settings
(setq org-directory "~/Documents/org/"
      org-agenda-files (list org-directory)
      org-roam-directory "~/Documents/org/roam/")

(after! org
  (add-hook 'org-mode-hook
            (lambda ()
              (doom/reload-font)
              (display-line-numbers-mode 0)
              (visual-line-mode)))
  (setq calendar-week-start-day 1)
  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords '(((sequence
                             "TODO(t)"
                             "NEXT(n)"
                             "WAIT(w)"
                             "MEETING(m)"
                             "|"
                             "DONE(d)"
                             "CANCELLED(c)")))))

(after! org-journal
  (add-hook 'org-journal-mode-hook
            (lambda ()
              (ispell-change-dictionary "polish")))
  (setq org-journal-file-type 'daily
      org-journal-date-format "%A, %d-%m-%Y"
      org-journal-file-format "%d-%m-%Y.org"))

(after! org-capture
  (setq org-capture-templates
        '(("n" "Personal note" entry (file+headline "~/Documents/org/notes.org" "Notes")
           "* %? \n%U" :empty-lines 1)
          ("t" "Personal task" entry (file+headline "~/Documents/org/todo.org" "Tasks")
           "* TODO %? \n%U" :empty-lines 1)
          ("s" "Scheduled task" entry (file+headline "~/Documents/org/todo.org" "Tasks")
           "* TODO %? \nSCHEDULED: %^t\n%U" :empty-lines 1)
          ("d" "Deadline" entry (file+headline "~/Documents/org/todo.org" "Tasks")
           "* TODO %? \n  DEADLINE: %^t" :empty-lines 1)
          ("m" "Meeting" entry (file+headline "~/Documents/org/agenda.org" "Agenda")
           "* MEETING: %? \nSCHEDULED: %^t\n%U" :empty-lines 1)
          ("e" "Event" entry (file+headline "~/Documents/org/event.org" "Events")
           "** %? \n %^T\n%U" :empty-lines 1))))

(use-package! evil-nerd-commenter
  :init (evilnc-default-hotkeys))

;; ebn's hacks
(use-package! evil-collection
  :config
  (defun ebn/evil-collection-vterm-setup ()
    (advice-add 'evil-collection-vterm-append-line :before #'vterm-reset-cursor-point))
  (after! vterm
    (ebn/evil-collection-vterm-setup)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
