## helm-recentd.el

Keep directories history you've opened in dired, even if you stop running Emacs!

```elisp
;; Example
(add-to-list 'load-path "~/.emacs.d/elisp/helm")
(add-to-list 'load-path "~/.emacs.d/elisp/helm-recentd")
(require 'helm)
(require 'helm-recentd)

;; You can change the default file name and location. I recommend to attach
;; `user-full-name' to distinguish multiple systems that you are using.
(setq helm-recentd-file
      (convert-standard-filename
       (concat "~/.emacs.d/.helm-recentd" "-" user-full-name)))

;; Sort directory list by 'date or 'frequency
(setq helm-recentd-sort 'date)

(global-set-key (kbd "C-x C-d") 'helm-recentd)
```

### Option for Mac OSX, Linux, Windows
While selecting directory path in helm interface, you can also choose where to open the directory by pressing [TAB] key, the options are, such as Finder, Nautilus, Terminal, or iTerm2, Explorer, Command Prompt, instead of opening in Emacs dired-mode.
