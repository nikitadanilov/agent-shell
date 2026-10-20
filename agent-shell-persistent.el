;;; agent-shell-persistent.el --- Remember selected agent shells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Mark shells with `agent-shell-persistent-mode' and restore them with
;; `agent-shell-restore-persistent'.  Only session identifiers, buffer names,
;; directories and agent configuration identifiers are saved, not credentials
;; or conversation contents.  Agents must support session loading or resuming.

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(defvar agent-shell--state)
(declare-function agent-shell--resolved-agent-configs "agent-shell" ())
(declare-function agent-shell--start "agent-shell" (&rest args))
(declare-function agent-shell-buffers "agent-shell" ())
(declare-function agent-shell-subscribe-to "agent-shell" (&rest args))
(declare-function agent-shell-unsubscribe "agent-shell" (&rest args))
(declare-function shell-maker-set-buffer-name "shell-maker" (buffer new-name))

(defcustom agent-shell-persistent-default nil
  "Whether new agent shells are persistent by default.
For example, setting this to t remembers every newly started shell.
Existing shells are unaffected; use `agent-shell-persistent-mode' in them."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-persistent-file
  (locate-user-emacs-file "agent-shell/persistent-sessions.el")
  "File containing the shells remembered by `agent-shell-persistent-mode'.
This is a data file, not an Emacs Lisp file to load or evaluate.
Use a different file for each concurrently running Emacs instance."
  :type 'file
  :group 'agent-shell)

(defvar-local agent-shell-persistent--entry nil
  "Last saved record for the current shell.
For example, ((:session-id . \"abc\") (:buffer-name . \"Work\")
              (:directory . \"/tmp/\") (:agent-config . claude-code)).")

(defvar-local agent-shell-persistent--subscription nil
  "Subscription tracking session initialisation in the current shell.")

(defvar-local agent-shell-persistent--restoring nil
  "Record whose restoration is still in progress in the current shell.")

(defvar agent-shell-persistent-mode)

(defun agent-shell-persistent--valid-entry-p (entry)
  "Return non-nil if ENTRY is a persistent shell record."
  (and (proper-list-p entry)
       (= (length entry) 4)
       (seq-every-p #'consp entry)
       (seq-every-p (lambda (key)
                     (let ((value (map-elt entry key)))
                       (and (stringp value) (not (string-empty-p value)))))
                   '(:session-id :buffer-name :directory))
       (file-name-absolute-p (map-elt entry :directory))
       (map-elt entry :agent-config)
       (symbolp (map-elt entry :agent-config))))

(defun agent-shell-persistent--read ()
  "Read saved shell records without evaluating their contents.
Signal an error for unreadable or malformed files rather than overwrite them."
  (when (file-exists-p agent-shell-persistent-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents agent-shell-persistent-file)
          (let* ((read-circle nil)
                 (data (read (current-buffer)))
                 (entries (map-elt data :shells)))
            (skip-chars-forward " \t\r\n")
            (unless (and (eobp)
                         (proper-list-p data)
                         (= (length data) 2)
                         (seq-every-p #'consp data)
                         (map-contains-key data :shells)
                         (equal (map-elt data :version) 1)
                         (proper-list-p entries)
                         (seq-every-p #'agent-shell-persistent--valid-entry-p entries))
              (error "Invalid persistent shell data"))
            entries))
      (error (user-error "Cannot read persistent shells from %s; file left unchanged"
                         agent-shell-persistent-file)))))

(defun agent-shell-persistent--write (entries)
  "Atomically replace the persistence file with ENTRIES.
The replacement file is readable and writable only by its owner."
  (let* ((file (expand-file-name agent-shell-persistent-file))
         (directory (file-name-directory file))
         (temporary-file nil))
    (make-directory directory t)
    (unwind-protect
        (progn
          (setq temporary-file (make-temp-file (expand-file-name ".persistent-" directory)))
          (set-file-modes temporary-file #o600)
          (let ((print-length nil)
                (print-level nil)
                (coding-system-for-write 'utf-8-unix))
            (with-temp-file temporary-file
              (prin1 (list (cons :version 1) (cons :shells entries)) (current-buffer))
              (insert "\n")))
          (rename-file temporary-file file t))
      (when (and temporary-file (file-exists-p temporary-file))
        (delete-file temporary-file)))))

(defun agent-shell-persistent--same-session-p (left right)
  "Return non-nil if LEFT and RIGHT identify the same saved session.
Names may differ; agent, directory and session identifier must agree."
  (and left right
       (seq-every-p (lambda (key)
                     (equal (map-elt left key) (map-elt right key)))
                   '(:agent-config :directory :session-id))))

(defun agent-shell-persistent--current-entry ()
  "Return a record for the current shell, or nil before it has a session."
  (when-let* ((id (map-nested-elt agent-shell--state '(:session :id))))
    (list (cons :session-id id)
          (cons :buffer-name (buffer-name))
          (cons :directory (file-name-as-directory (expand-file-name default-directory)))
          (cons :agent-config (map-nested-elt agent-shell--state '(:agent-config :identifier))))))

(defun agent-shell-persistent--save ()
  "Save the current persistent shell if its record has changed."
  (when-let* ((agent-shell-persistent-mode)
              (entry (agent-shell-persistent--current-entry))
              ((not (equal entry agent-shell-persistent--entry))))
    (unless (agent-shell-persistent--valid-entry-p entry)
      (user-error "Shell needs a session and an agent configuration identifier"))
    (agent-shell-persistent--write
     (cons entry
           (seq-remove
            (lambda (saved)
              (or (agent-shell-persistent--same-session-p saved entry)
                  (agent-shell-persistent--same-session-p saved agent-shell-persistent--entry)))
            (agent-shell-persistent--read))))
    (setq agent-shell-persistent--entry entry)))

(defun agent-shell-persistent--maybe-save ()
  "Save after a buffer rename without interrupting buffer management."
  (condition-case err
      (agent-shell-persistent--save)
    (error (display-warning 'agent-shell (error-message-string err) :warning))))

(defun agent-shell-persistent--on-event (event)
  "Save a session once EVENT reports it has been initialised."
  (pcase (map-elt event :event)
    ((or 'init-session 'init-finished)
     (setq agent-shell-persistent--restoring nil)
     (agent-shell-persistent--maybe-save))
    ('error (setq agent-shell-persistent--restoring nil))))

(defun agent-shell-persistent--detach ()
  "Stop tracking the current shell without forgetting its saved record."
  (remove-hook 'buffer-list-update-hook #'agent-shell-persistent--maybe-save t)
  (when agent-shell-persistent--subscription
    (agent-shell-unsubscribe :subscription agent-shell-persistent--subscription)
    (setq agent-shell-persistent--subscription nil)))

;;;###autoload
(define-minor-mode agent-shell-persistent-mode
  "Remember this agent shell across Emacs restarts.
Toggle interactively, or pass a positive ARG to enable and a negative ARG
to disable.  For example, (agent-shell-persistent-mode 1) remembers this shell.
Renaming the buffer updates its record.  Killing it retains the record;
disabling this mode explicitly forgets it.  If no session exists yet,
the record is saved once initialisation succeeds.
Use `agent-shell-restore-persistent' to reopen remembered shells."
  :lighter " Persistent"
  :group 'agent-shell
  (unless (derived-mode-p 'agent-shell-mode)
    (setq agent-shell-persistent-mode nil)
    (user-error "Not in an agent shell"))
  (if agent-shell-persistent-mode
      (condition-case err
          (progn
            (unless agent-shell-persistent--subscription
              (setq agent-shell-persistent--subscription
                    (agent-shell-subscribe-to
                     :shell-buffer (current-buffer)
                     :on-event #'agent-shell-persistent--on-event)))
            (agent-shell-persistent--save)
            (add-hook 'buffer-list-update-hook #'agent-shell-persistent--maybe-save nil t))
        (error
         (agent-shell-persistent--detach)
         (setq agent-shell-persistent-mode nil)
         (signal (car err) (cdr err))))
    (condition-case err
        (when agent-shell-persistent--entry
          (agent-shell-persistent--write
           (seq-remove (lambda (saved)
                         (agent-shell-persistent--same-session-p saved agent-shell-persistent--entry))
                       (agent-shell-persistent--read))))
      (error
       (setq agent-shell-persistent-mode t)
       (signal (car err) (cdr err))))
    (agent-shell-persistent--detach)
    (setq agent-shell-persistent--entry nil
          agent-shell-persistent--restoring nil)))

(defun agent-shell-persistent--setup ()
  "Apply `agent-shell-persistent-default' to a newly started shell."
  (when agent-shell-persistent-default
    (condition-case err
        (agent-shell-persistent-mode 1)
      (error (display-warning 'agent-shell (error-message-string err) :warning)))))

(add-hook 'agent-shell-mode-hook #'agent-shell-persistent--setup)

(defun agent-shell-persistent--find-buffer (entry)
  "Find a live shell already running or restoring ENTRY."
  (seq-find
   (lambda (buffer)
     (with-current-buffer buffer
       (or (agent-shell-persistent--same-session-p entry (agent-shell-persistent--current-entry))
           (agent-shell-persistent--same-session-p entry agent-shell-persistent--restoring))))
   (agent-shell-buffers)))

(defun agent-shell-persistent--restore (entry configs)
  "Restore one saved ENTRY using CONFIGS, or reuse its existing shell."
  (let ((existing (agent-shell-persistent--find-buffer entry)))
    (if existing
        (with-current-buffer existing
          (agent-shell-persistent-mode 1)
          existing)
      (let ((config (seq-find (lambda (config)
                                (eq (map-elt config :identifier) (map-elt entry :agent-config)))
                              configs))
            (directory (map-elt entry :directory)))
        (unless config
          (user-error "Agent configuration is unavailable"))
        (unless (file-directory-p directory)
          (user-error "Working directory is unavailable"))
        (let* ((agent-shell-persistent-default nil)
               (buffer (agent-shell--start :config config
                                          :cwd directory
                                          :session-id (map-elt entry :session-id)
                                          :resume-session-only t
                                          :session-strategy 'new
                                          :new-session t
                                          :no-focus t)))
          (with-current-buffer buffer
            (setq agent-shell-persistent--entry entry
                  agent-shell-persistent--restoring
                  (unless (map-nested-elt agent-shell--state '(:session :id)) entry))
            (shell-maker-set-buffer-name buffer (map-elt entry :buffer-name))
            (agent-shell-persistent-mode 1))
          buffer)))))

;;;###autoload
(defun agent-shell-restore-persistent ()
  "Restore all remembered agent shells in the background.
For example, run this after restarting Emacs to reopen persistent shells.
Already open or restoring sessions are reused.  Missing directories and
agent configurations are reported without forgetting their records.
An agent must support loading or resuming the saved session; failure never
substitutes a fresh conversation.  Return the list of shell buffers."
  (interactive)
  (require 'agent-shell)
  (let ((entries (agent-shell-persistent--read))
        (configs (agent-shell--resolved-agent-configs))
        buffers)
    (dolist (entry entries)
      (condition-case err
          (push (agent-shell-persistent--restore entry configs) buffers)
        (error
         (display-warning 'agent-shell
                          (format "Could not restore %s: %s"
                                  (map-elt entry :buffer-name) (error-message-string err))
                          :warning))))
    (when (called-interactively-p 'interactive)
      (message "Opened or reused %d of %d persistent shells; initialisation may still be in progress"
               (length buffers) (length entries)))
    (nreverse buffers)))

(provide 'agent-shell-persistent)

;;; agent-shell-persistent.el ends here