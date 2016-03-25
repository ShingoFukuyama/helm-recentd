;;; helm-recentd.el --- Save directory history to file, and view with helm. Emacs -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: 1.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/helm-recentd
;; Created: Jan 22 2014
;; Keywords: recentd directory dired history helm
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:
;; ----------------------------------------------------------------------
;; ;; Example
;; (add-to-list 'load-path "~/.emacs.d/elisp/helm")
;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-recentd")
;; (require 'helm)
;; (require 'helm-recentd)

;; ;; You can change the default file name and location. I recommend to attach
;; ;; `user-full-name' to distinguish multiple systems that you are using.
;; (setq helm-recentd-file
;;       (convert-standard-filename
;;        (concat "~/.emacs.d/.helm-recentd" "-" user-full-name)))

;; ;; Sort directory list by 'date or 'frequency
;; (setq helm-recentd-sort 'date)

;; (global-set-key (kbd "C-x C-d") 'helm-recentd)
;; ----------------------------------------------------------------------

;;; Code:

(require 'cl-lib)
(require 'helm)

(defgroup helm-recentd nil
  "Group of helm-recentd"
  :prefix "helm-recentd-" :group 'helm)

(defvar helm-recentd-list nil)
(defvar helm-recentd-coding-system 'utf-8)

(defcustom helm-recentd-file
  (convert-standard-filename (concat "~/.helm-recentd" "-" user-full-name))
  "File path for helm-recentd"
  :group 'helm-recentd
  :type 'string)

(defcustom helm-recentd-sort 'date
 "Sort directory list by `date' or `frequency'"
 :type '(choice (const :tag "Sort by date"      'date)
                (const :tag "Sort by frequency" 'frequency))
 :group 'helm-recentd)

(defun helm-recentd--save-to-list ()
  (let* (($dir (expand-file-name default-directory))
         ($cons (assoc $dir helm-recentd-list))
         ($times (or (cadr $cons) 0))
         ($time-stamp (current-time)))
    ;; Delete from helm-recentd-list
    (if $cons (setq helm-recentd-list (delete $cons helm-recentd-list)))
    ;; Update helm-recentd-list
    (add-to-list 'helm-recentd-list
                 (cons (propertize
                        $dir
                        'display
                        (concat $dir " | "
                                (propertize
                                 (format-time-string "%Y/%m/%d %H:%M" $time-stamp)
                                 'face 'font-lock-function-name-face)))
                       (list (1+ $times)
                             (float-time $time-stamp))))
    ;; Sort by `helm-recentd-sort' (frequency, date)
    (if helm-recentd-sort
        (setq helm-recentd-list (cl-sort (copy-sequence helm-recentd-list)
                                         '> :key (lambda ($elms)
                                                   (nth (cl-case helm-recentd-sort
                                                          ('frequency 0)
                                                          ('date 1))
                                                        (cdr $elms)))))))
  (helm-recentd--save-to-file))

(defun helm-recentd--save-to-file ()
  (let ((coding-system-for-write helm-recentd-coding-system))
    (write-region
     (concat "(setq helm-recentd-list '"
             (prin1-to-string helm-recentd-list)
             ")\n")
     nil helm-recentd-file nil 'silent)))

(defun helm-recentd--init-load-file ()
  (load helm-recentd-file t))

(add-hook 'dired-mode-hook 'helm-recentd--save-to-list)
(add-hook 'after-init-hook 'helm-recentd--init-load-file)

;; ----------------------------------------------------------------------
;; helm

(defvar helm-recentd-map
  (let (($map (make-sparse-keymap)))
    (set-keymap-parent $map helm-map)
    (define-key $map (kbd "M-j") 'helm-select-4rd-action)
    (delq nil $map))
  "Keymap for helm-recentd")

(defvar helm-recentd--action-default
  '(("Open in dired" . (lambda (ignored)
                         (let (($dir (helm-recentd--get-target-string)))
                           (if (file-directory-p $dir)
                               (dired $dir)
                             (error "%s is not directory" $dir)))))
    ("Copy path" . (lambda (ignored)
                     (let (($dir (helm-recentd--get-target-string)))
                       (kill-new $dir)
                       (message "Copied: %s" $dir))))))

(defun helm-recentd-open-in-terminal (ignored)
  (start-process "gnome-terminal" nil "gnome-terminal" "--working-directory"
                 (helm-recentd--get-target-string)))

(defvar helm-recentd--action
  (append
   helm-recentd--action-default
   (cond ((executable-find "xdg-open")
          '(("Open in Nautilus"
             . (lambda (ignored)
                 (shell-command (format "xdg-open %s"
                                        (helm-recentd--get-target-string)))))
            ("Open in Terminal" . helm-recentd-open-in-terminal)))
         ((executable-find "open")
          '(("Open in Finder"
             . (lambda (ignored)
                 (shell-command (format "open %s"
                                        (helm-recentd--get-target-string)))))
            ("Open in iTerm2"
             . (lambda (ignored)
                 (shell-command (format "open -a iTerm %s"
                                        (helm-recentd--get-target-string)))))
            ("Open in Terminal" . helm-recentd-open-in-terminal)))
         ((and (executable-find "explorer") (executable-find "start"))
          '(("Open in Explorer"
             . (lambda (ignored)
                 (format "start \"\" \"%s\"" (helm-recentd--get-target-string))))
            ("Open in Command Prompt"
             . (lambda (ignored)
                 (shell-command (format "start cd \"%s\""
                                        (helm-recentd--get-target-string))))))))))

(defun helm-recentd--get-target-string ()
  "Get the directory path you chose from helm directory list"
  (with-current-buffer helm-buffer
    (buffer-substring-no-properties
     (overlay-start helm-selection-overlay)
     (1- (overlay-end helm-selection-overlay)))))

(defun helm-c-source-recentd ()
  `((name . "helm-recentd")
    (candidates . helm-recentd-list)
    (action ,@helm-recentd--action)
    (keymap . ,helm-recentd-map)
    (header-line . "Press [TAB]: Show more options")))

;;;###autoload
(defun helm-recentd (&optional $preinput)
  (interactive)
  "Show all directory that you have ever visited."
  (helm :sources (helm-c-source-recentd)
        :buffer "*helm-recentd*"
        :input (or $preinput "")
        :prompt "Directory name: "
        :candidate-number-limit 255))

(provide 'helm-recentd)

;;; helm-recentd.el ends here
