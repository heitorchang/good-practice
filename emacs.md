# Emacs

Melpa packages

```
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
```

Run `M-x package-refresh-contents` or `M-x package-list-packages`

# Home directory

To redefine ~, add to `.emacs`

```
;; old value is "c:/Users/neo/AppData/Roaming/"
(setenv "HOME" "c:/Users/neo/")
```

Note that to update `.emacs`, you need to type the old directory's name

# Theme

For default background, change (custom-set-faces '(default ...)) instead of the theme.

`M-x load-theme`

`M-x customize-create-theme`

# Return to point where substitutions was called

After substituting text with `M-%`, type `C-u C-SPC` to return to the initial point.

Load them in .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; ...
 '(custom-enabled-themes (quote (my-tango)))
 ;; ...
)

# view-lossage

C-h l shows a list of recently-typed keys and commands

# yank (paste) and go back to original cursor position

C-y then C-u C-SPC

# No color in terminal

emacs -nw --color=no

# kill-sexp

C-M-k

# Paredit

Set M-z to undo
Use M-o to switch windows

Add to clojure-mode-hook:
(define-key paredit-mode-map (kbd "<C-left>") 'left-word)
(define-key paredit-mode-map (kbd "<C-right>") 'right-word)

C-M-u Move up a level
C-M-d Move down a level

M-n jumps outside to the right of sexp to the top level (forward-paragraph)
M-p jumps outside to the left of sexp to the top level (backward-paragraph)

# Show backtrace on error

M-x toggle-debug-on-error