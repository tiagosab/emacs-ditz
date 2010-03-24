;;; ditz.el --- Emacs interface to Ditz issue tracking system

;; Copyright (C) 2008 Kentaro Kuribayashi

;; Author: Kentaro Kuribayashi <kentarok@gmail.com>
;; Keywords: ditz, todo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README file or the website below:
;; http://github.com/kentaro/emacs-ditz/tree/master

;;; Code:

;; Customizable variables
(defcustom ditz-program "ditz"
  "Ditz command"
  :type 'string
  :group 'ditz)

(defcustom ditz-issue-directory "bugs"
  "Default directory name in which issues are stored.

You must set it some value according with your environment when
you use automatic finding described below."
  :type 'string
  :group 'ditz)

(defcustom ditz-find-issue-directory-automatically-flag nil
  "If non-nil, issue directory will be found automatically in
directories from the current one toward the root. Otherwise, you
must set it from minibuffer."
  :type 'boolean
  :group 'ditz)

;; Constant variables

;; NOTE: If you change the subgroups in these regexes, you may also need to
;; change the code that uses them - both the ditz-extract-thing-at-point calls
;; and the ditz-font-lock-keywords variable.

(defconst ditz-status-regex "^[_>=x]"
  "Regex for issue status.")

(defconst ditz-issue-id-regex "^\\([_>=x]\\|Issue\\) +\\([^:\n]+\\)"
  "Regex for issue id.")

(defconst ditz-release-name-regex "^\\([^\n_>=x][^\n :]*\\) (.*):$"
  "Regex for release name.")

(defconst ditz-unassigned-regex "^\\(Unassigned\\):$"
  "Regex for unassigned issues header.")

(defconst ditz-buffer-issue-id-regex "^Issue \\([^:\n]+\\)$"
  "Regex for issue ID in an issue buffer.")

;; Commands
(defun ditz-init ()
  "Initialize ditz issues."
  (interactive)
  (ditz-call-process "init" nil "pop"))

(defun ditz-html ()
  "Generate html files of issues."
  (interactive)
  (ditz-call-process "html" nil "display"))

(defun ditz-add-release ()
  "Add a new release."
  (interactive)
  (ditz-call-process "add-release" nil "pop"))

(defun ditz-add ()
  "Add a new issue."
  (interactive)
  (ditz-call-process "add" nil "pop"))

(defun ditz-status ()
  "Show status of issues."
  (interactive)
  (ditz-call-process "status" nil "display"))

(defun ditz-todo ()
  "Show current todo."
  (interactive)
  (ditz-call-process "todo" nil "pop"))

(defun ditz-log ()
  "Show log of recent activities."
  (interactive)
  (ditz-call-process "log" nil "pop"))

(defun ditz-show ()
  "Show issue details."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(ditz-call-process "show" issue-id "switch")
      (error "Issue id not found"))))

(defun ditz-assign ()
  "Assign issue to a release."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(ditz-call-process "assign" issue-id "switch")
      (error "Issue id not found"))))

(defun ditz-start-stop ()
  "Start or stop working on issue."
  (interactive)
  (let ((issue-id (ditz-find-issue))
	(status (ditz-issue-status)))
    (if (and issue-id status)
	(ditz-call-process
	 (cond ((memq status '(unstarted paused closed)) "start")
	       ((memq status '(in-progress)) "stop")
	       (t (error "Can't interpret status marker")))
	 issue-id)
      (error "Issue id not found"))))

(defun ditz-edit ()
  "Edit issue details."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(ditz-call-process "edit" issue-id "switch")
      (error "Issue id not found"))))

(defun ditz-comment ()
  "Comment on issue."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(ditz-call-process "comment" issue-id "switch")
      (error "Issue id not found"))))

(defun ditz-close ()
  "Close an issue."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(ditz-call-process "close" issue-id "switch")
      (error "Issue id not found"))))

(defun ditz-drop ()
  "Drop an issue."
  (interactive)
  (let ((issue-id (ditz-find-issue)))
    (if issue-id
	(when (yes-or-no-p (concat "Drop " issue-id " "))
	  (ditz-call-process "drop" issue-id "switch"))
      (error "Issue id not found"))))

(defun ditz-release ()
  "Mark issues as released."
  (interactive)
  (let ((release-name nil))
    (setq release-name (ditz-extract-thing-at-point ditz-release-name-regex 1))
    (if release-name
	(ditz-call-process "release" release-name "switch")
      (error "Release name not found"))))

(defun ditz-extract-thing-at-point (regex n)
  (save-excursion
    (let ((line (buffer-substring-no-properties (progn (beginning-of-line) (point))
						(progn (end-of-line) (point)))))
      (when (string-match regex line)
	(match-string n line)))))

(defun ditz-find-issue ()
  (or (ditz-extract-thing-at-point ditz-issue-id-regex 2) ;; status line
      (save-excursion
	(goto-char (point-min))
	(ditz-extract-thing-at-point ditz-issue-id-regex 2)))) ;; header

(defun ditz-reload ()
  (interactive)
  (cond ((string= (buffer-name) "*ditz-todo*")
	 (ditz-call-process "todo" nil "switch"))
	((string= (buffer-name) "*ditz-status*")
	 (ditz-call-process "status" nil "switch"))
	((string= (buffer-name) "*ditz-log*")
	 (ditz-call-process "log" nil "switch"))))

(defun ditz-close-buffer ()
  "Close ditz buffer."
  (interactive)
  (quit-window))

(defun ditz-call-process (command &optional arg popup-flag)
  "Call ditz process asynchronously according with sub-commands."
  (let* ((buffer (get-buffer-create (concat "*ditz-" command "*")))
	 (proc (get-buffer-process buffer)))

    (if (and proc (eq (process-status proc) 'run))
	(when (y-or-n-p (format "A %s process is running; kill it?"
				(process-name proc)))
	  (interrupt-process proc)
	  (sit-for 1)
	  (delete-process proc))

      (with-current-buffer buffer
	(erase-buffer)
	(buffer-disable-undo (current-buffer)))

      (make-comint-in-buffer "ditz-call-process"
			     buffer shell-file-name nil shell-command-switch
			     (ditz-build-command command arg))

      (cond ((or (eq major-mode 'ditz-mode)
		 (string= popup-flag "switch"))
	     (switch-to-buffer buffer))
	    ((string= popup-flag "pop")
	     (pop-to-buffer buffer))
	    ((string= popup-flag "display")
	     (display-buffer buffer))
	    (t
	     (set-buffer buffer)))

      (set-process-sentinel
       (get-buffer-process buffer)
       '(lambda (process signal)
	  (when (string= signal "finished\n")
	    (with-current-buffer (process-buffer process)
	      (ditz-mode)
	      (goto-char (point-min)))))))))

(defvar ditz-last-visited-issue-directory nil)

(defun ditz-build-command (command arg)
  (let (issue-directory current-directory)

    ;; Reserve current directory to come back later It's needed when
    ;; automatically finding directory.
    (when buffer-file-name
      (setq current-directory (file-name-directory (buffer-file-name))))

    (cond ((eq major-mode 'ditz-mode)
	   (setq issue-directory ditz-last-visited-issue-directory))
	  ((and (not (string= command "init"))
		ditz-find-issue-directory-automatically-flag
		(catch 'loop
		  (while t
		    (cond ((file-exists-p ditz-issue-directory)
			   (throw 'loop t))
			  ((string= "/" default-directory)
			   (throw 'loop nil))
			  (t
			   (cd ".."))))))
	   (setq issue-directory
		 (concat default-directory ditz-issue-directory)))
	  (t
	   (setq issue-directory
		 (read-file-name "Issue dir: "
				 (or ditz-last-visited-issue-directory
				     default-directory)))))

    ;; Restore default directory if needed.
    (when current-directory
      (setq default-directory current-directory))

    (setq ditz-last-visited-issue-directory issue-directory)
    (mapconcat 'identity
	       (list ditz-program
		     "-i" (shell-quote-argument (expand-file-name issue-directory))
		     command (and arg (shell-quote-argument arg))) " ")))

(defun ditz-issue-status ()
  "Return symbol indicating issue's status."
  (let ((marker (ditz-extract-thing-at-point ditz-status-regex 0)))
    (if marker
	(cdr (assq (string-to-char marker)
		   '((?_ . unstarted)
		     (?> . in-progress)
		     (?= . paused)
		     (?x . closed))))
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward "^\\s-*Status: \\(.*\\)\\s-*$")
	    (intern (subst-char-in-string ?\ ?\- (match-string 1))))))))

;; define ditz-view-issue-{next,prev}
(easy-mmode-define-navigation ditz-view-issue ditz-issue-id-regex "issue")

;; Hooks
(defvar ditz-mode-hook nil
  "*Hooks for Ditz major mode")

;; Keymap
(defvar ditz-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n")    'ditz-view-issue-next)
    (define-key map (kbd "p")    'ditz-view-issue-prev)
    (define-key map (kbd "s")    'ditz-show)
    (define-key map (kbd "\C-m") 'ditz-show)
    (define-key map (kbd "A")    'ditz-add)
    (define-key map (kbd "a")    'ditz-assign)
    (define-key map (kbd "D")    'ditz-drop)
    (define-key map (kbd "e")    'ditz-edit)
    (define-key map (kbd "c")    'ditz-close)
    (define-key map (kbd "r")    'ditz-release)
    (define-key map (kbd "g")    'ditz-reload)
    (define-key map (kbd "q")    'ditz-close-buffer)
    map)
  "*Keymap for Ditz major mode")


;; Face
(defface ditz-issue-id-face
  '((((class color) (background light))
     (:foreground "blue" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "PowderBlue" :underline t :weight bold)))
  "Face definition for issue id")

(defface ditz-release-name-face
  '((((class color) (background light))
     (:foreground "red" :underline t :weight bold))
    (((class color) (background dark))
     (:foreground "red" :underline t :weight bold)))
  "Face definition for release name")

(defvar ditz-issue-id-face 'ditz-issue-id-face)
(defvar ditz-release-name-face 'ditz-release-name-face)
(defvar ditz-font-lock-keywords
  `((,ditz-issue-id-regex (2 ditz-issue-id-face t))
    (,ditz-release-name-regex (1 ditz-release-name-face t))
    (,ditz-unassigned-regex (1 ditz-release-name-face t))))

;; Ditz major mode
(define-derived-mode ditz-mode fundamental-mode "Ditz"
  "Major mode for interacting with the Ditz issue tracker."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ditz-mode)
  (setq mode-name "Ditz")
  (use-local-map ditz-mode-map)
  (set (make-local-variable 'font-lock-defaults)  '(ditz-font-lock-keywords))
  (font-lock-mode 1)
  (run-hooks 'ditz-mode-hook))

(provide 'ditz)
;;; ditz.el ends here