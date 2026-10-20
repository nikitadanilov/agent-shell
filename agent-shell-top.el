;;; agent-shell-top.el --- Live overview of agent shells -*- lexical-binding: t; -*-

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
;; `agent-shell-top' displays all agent shells and refreshes every second.
;; RET visits a shell, g refreshes immediately and q closes the overview.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'timer)

(declare-function agent-shell-buffers "agent-shell" ())
(declare-function agent-shell-status "agent-shell" (&key shell-buffer))

(defvar agent-shell-top--buffer nil
  "Buffer used by `agent-shell-top'.")

(defvar-local agent-shell-top--timer nil
  "Timer refreshing the current agent shell overview.")

(defun agent-shell-top--entries ()
  "Return rows containing each live shell's name, status and directory.
Each row has the form (BUFFER [NAME STATUS DIRECTORY]), with BUFFER
as its stable identifier.  STATUS is \"busy\", \"blocked\" or \"ready\"."
  (seq-map
   (lambda (buffer)
     (list buffer
           (vector (buffer-name buffer)
                   (symbol-name (agent-shell-status :shell-buffer buffer))
                   (buffer-local-value 'default-directory buffer))))
   (seq-filter #'buffer-live-p (agent-shell-buffers))))

(defun agent-shell-top-refresh (&rest _ignored)
  "Refresh the overview, keeping point on the same shell when possible.
Arguments are ignored, for use as `revert-buffer-function'."
  (interactive)
  (setq tabulated-list-entries (agent-shell-top--entries))
  (tabulated-list-print t))

(defun agent-shell-top--refresh-buffer (buffer)
  "Refresh BUFFER if it is still a live agent shell overview."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-top-mode)
        (agent-shell-top-refresh)))))

(defun agent-shell-top--cancel-timer ()
  "Cancel the current overview's refresh timer."
  (when agent-shell-top--timer
    (cancel-timer agent-shell-top--timer)
    (setq agent-shell-top--timer nil)))

(defun agent-shell-top-visit ()
  "Visit the live agent shell on the current row."
  (interactive)
  (let ((buffer (tabulated-list-get-id)))
    (unless (buffer-live-p buffer)
      (user-error "No live agent shell on this line"))
    (pop-to-buffer buffer)))

(defun agent-shell-top-quit ()
  "Quit the overview and kill its buffer, stopping automatic refresh."
  (interactive)
  (quit-window t))

(defvar agent-shell-top-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-top-visit)
    (define-key map (kbd "g") #'agent-shell-top-refresh)
    (define-key map (kbd "q") #'agent-shell-top-quit)
    map)
  "Keymap for `agent-shell-top-mode'.")

(define-derived-mode agent-shell-top-mode tabulated-list-mode "Agent Shell Top"
  "Major mode for a live overview of all agent shells.
\\<agent-shell-top-mode-map>
Use \\[agent-shell-top-visit] to visit a shell, \\[agent-shell-top-refresh]
to refresh immediately and \\[agent-shell-top-quit] to quit."
  (setq-local tabulated-list-format
              [("Buffer" 36 t) ("Status" 8 t) ("Working directory" 0 t)])
  (setq-local tabulated-list-sort-key (cons "Buffer" nil))
  (setq-local tabulated-list-entries nil)
  (setq-local revert-buffer-function #'agent-shell-top-refresh)
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'agent-shell-top--cancel-timer nil t)
  (add-hook 'change-major-mode-hook #'agent-shell-top--cancel-timer nil t)
  (setq agent-shell-top--timer
        (run-at-time 1 1 #'agent-shell-top--refresh-buffer (current-buffer))))

;;;###autoload
(defun agent-shell-top ()
  "Display all agent shells with their status and working directory.
Reuse the existing overview buffer and refresh it every second."
  (interactive)
  (require 'agent-shell)
  (unless (and (buffer-live-p agent-shell-top--buffer)
               (with-current-buffer agent-shell-top--buffer
                 (derived-mode-p 'agent-shell-top-mode)))
    (setq agent-shell-top--buffer (generate-new-buffer "*agent-shell-top*")))
  (with-current-buffer agent-shell-top--buffer
    (unless (derived-mode-p 'agent-shell-top-mode)
      (agent-shell-top-mode))
    (agent-shell-top-refresh))
  (pop-to-buffer agent-shell-top--buffer))

(provide 'agent-shell-top)

;;; agent-shell-top.el ends here