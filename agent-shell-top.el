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

(defconst agent-shell-top--default-format
  [("Buffer" 36 t) ("Status" 8 t) ("Working directory" 0 t)]
  "Column layout of the overview.
Widths are upper bounds; `agent-shell-top--format' trims them to the
contents.  The last column has width 0 and fills the remaining space.")

(defun agent-shell-top--format (entries)
  "Return the column layout for ENTRIES, trimmed to their widths.
ENTRIES has the form of `tabulated-list-entries'.  Each column is no
wider than in `agent-shell-top--default-format' and no narrower than
its label plus the sort indicator.  For example, with entries whose
longest buffer name is 20 characters wide, the \"Buffer\" column is 20
wide instead of 36; with no entries it is 8 wide, enough for \"Buffer\"
and the indicator; and with a 50-character name it stays at 36."
  (let ((widths (make-vector (length agent-shell-top--default-format) 0)))
    (dolist (entry entries)
      (seq-do-indexed (lambda (cell index)
                        (aset widths index (max (aref widths index)
                                                (string-width cell))))
                      (cadr entry)))
    (vconcat
     (seq-map-indexed
      (lambda (column index)
        (pcase-let ((`(,label ,width . ,props) column))
          (if (zerop width)
              column
            (cons label (cons (max (+ (length label) 2)
                                   (min width (aref widths index)))
                              props)))))
      agent-shell-top--default-format))))

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
  (let* ((entries (agent-shell-top--entries))
         (format (agent-shell-top--format entries)))
    (setq tabulated-list-entries entries)
    (unless (equal format tabulated-list-format)
      (setq tabulated-list-format format)
      (tabulated-list-init-header))
    (tabulated-list-print t)))

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
  (setq-local tabulated-list-format agent-shell-top--default-format)
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