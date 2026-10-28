;;; agent-shell-top-tests.el --- Tests for agent-shell-top -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; These tests use temporary buffers and stub the public shell APIs.
;; No agents are started.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'agent-shell)
(require 'agent-shell-top)

(defmacro agent-shell-top-tests--with-shells (&rest body)
  "Run BODY in an overview of three temporary shell buffers.
Bind `shells' to the enumerated buffers and `statuses' to their status
alist.  Bind `top-buffer' to the original overview.  Changes to these
values are visible to the stubbed public APIs on subsequent refreshes."
  (declare (indent 0) (debug t))
  `(let* ((buffers (seq-map #'generate-new-buffer
                            '(" *agent-shell-top-test-a*"
                              " *agent-shell-top-test-b*"
                              " *agent-shell-top-test-c*")))
          (shells (copy-sequence buffers))
          (statuses (seq-mapn #'cons buffers '(busy blocked ready)))
          (agent-shell-top--buffer nil)
          top-buffer)
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () shells))
                   ((symbol-function 'agent-shell-status)
                    (cl-function
                     (lambda (&key shell-buffer)
                       (should (buffer-live-p shell-buffer))
                       (or (map-elt statuses shell-buffer)
                           (ert-fail "Unexpected shell buffer")))))
                   ((symbol-function 'pop-to-buffer)
                    (lambda (buffer &rest _ignored) buffer)))
           (setq top-buffer (agent-shell-top))
           (with-current-buffer top-buffer
             ,@body))
       (seq-do (lambda (buffer)
                 (when (buffer-live-p buffer)
                   (kill-buffer buffer)))
               (append buffers (list top-buffer agent-shell-top--buffer))))))

(ert-deftest agent-shell-top-displays-all-shells ()
  (agent-shell-top-tests--with-shells
    (seq-do (lambda (buffer)
              (with-current-buffer buffer
                (setq-local default-directory
                            (format "/tmp/project-%s/" (map-elt statuses buffer)))))
            shells)
    (agent-shell-top-refresh)
    (should (derived-mode-p 'tabulated-list-mode))
    (should buffer-read-only)
    (should (= (length tabulated-list-entries) 3))
    (seq-do (lambda (buffer)
              (should (equal (seq-first (map-elt tabulated-list-entries buffer))
                             (vector (buffer-name buffer)
                                     (symbol-name (map-elt statuses buffer))
                                     (buffer-local-value 'default-directory buffer)))))
            shells)))

(ert-deftest agent-shell-top-refresh-updates-status-and-directory ()
  (agent-shell-top-tests--with-shells
    (let ((shell (seq-first shells)))
      (map-put! statuses shell 'ready)
      (with-current-buffer shell
        (setq-local default-directory "/tmp/another-project/"))
      (call-interactively (key-binding (kbd "g")))
      (should (equal (seq-first (map-elt tabulated-list-entries shell))
                     (vector (buffer-name shell) "ready" "/tmp/another-project/")))
      (map-put! statuses shell 'blocked)
      (revert-buffer nil t)
      (should (equal (seq-elt (seq-first (map-elt tabulated-list-entries shell)) 1)
                     "blocked")))))

(ert-deftest agent-shell-top-trims-column-widths-to-contents ()
  (agent-shell-top-tests--with-shells
    (let* ((shell (seq-first shells))
           ;; All test buffer names are this wide, below the default 36.
           (name-width (length (buffer-name shell)))
           (width (lambda (name)
                    (nth 1 (seq-find (lambda (column) (equal (car column) name))
                                     tabulated-list-format)))))
      (should (< name-width 36))
      (should (= (funcall width "Buffer") name-width))
      (should (= (funcall width "Status") 8))
      (should (= (funcall width "Working directory") 0))
      (goto-char (point-min))
      (should (equal (seq-elt (tabulated-list-get-entry) 1)
                     (buffer-substring-no-properties
                      (+ (line-beginning-position) name-width 1)
                      (+ (line-beginning-position) name-width 1
                         (length (seq-elt (tabulated-list-get-entry) 1))))))
      (with-current-buffer shell
        (rename-buffer (make-string 50 ?x) t))
      (agent-shell-top-refresh)
      (should (= (funcall width "Buffer") 36))
      (setq shells nil)
      (agent-shell-top-refresh)
      (should (= (funcall width "Buffer") (+ (length "Buffer") 2)))
      (should (equal (seq-elt tabulated-list-format 0) '("Buffer" 8 t))))))

(ert-deftest agent-shell-top-refresh-preserves-selection-after-rename ()
  (agent-shell-top-tests--with-shells
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 3)
    (let ((shell (tabulated-list-get-id)))
      (with-current-buffer shell
        (rename-buffer " *agent-shell-top-test-z*" t))
      (setq shells (reverse shells))
      (agent-shell-top-refresh)
      (should (eq (tabulated-list-get-id) shell))
      (should (= (current-column) 3))
      (should (= (line-number-at-pos) 3))
      (should (equal (seq-elt (tabulated-list-get-entry) 0)
                     (buffer-name shell))))))

(ert-deftest agent-shell-top-refresh-preserves-sort-and-selection ()
  (agent-shell-top-tests--with-shells
    (goto-char (point-min))
    (let ((shell (tabulated-list-get-id)))
      (tabulated-list-sort 1)
      (should (equal tabulated-list-sort-key '("Status")))
      (should (eq (tabulated-list-get-id) shell))
      (map-put! statuses shell 'blocked)
      (agent-shell-top-refresh)
      (should (equal tabulated-list-sort-key '("Status")))
      (should (eq (tabulated-list-get-id) shell))
      (should (equal (seq-elt (tabulated-list-get-entry) 1) "blocked")))))

(ert-deftest agent-shell-top-refresh-removes-killed-shells ()
  (agent-shell-top-tests--with-shells
    (let ((shell (seq-first shells)))
      (kill-buffer shell)
      (agent-shell-top-refresh)
      (should (= (length tabulated-list-entries) 2))
      (should-not (map-elt tabulated-list-entries shell))
      (should (buffer-live-p (tabulated-list-get-id))))
    (seq-do (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            shells)
    (agent-shell-top-refresh)
    (should-not tabulated-list-entries)
    (should (= (buffer-size) 0))
    (should-not (tabulated-list-get-id))
    (should-error (agent-shell-top-visit) :type 'user-error)))

(ert-deftest agent-shell-top-refresh-adds-new-shells ()
  (agent-shell-top-tests--with-shells
    (let ((shell (generate-new-buffer " *agent-shell-top-test-new*")))
      (unwind-protect
          (progn
            (push shell shells)
            (setf (map-elt statuses shell) 'ready)
            (agent-shell-top--refresh-buffer top-buffer)
            (should (= (length tabulated-list-entries) 4))
            (should (map-elt tabulated-list-entries shell)))
        (kill-buffer shell)))))

(ert-deftest agent-shell-top-visit-uses-buffer-identity ()
  (agent-shell-top-tests--with-shells
    (goto-char (point-min))
    (let ((shell (tabulated-list-get-id))
          visited)
      (with-current-buffer shell
        (rename-buffer " *agent-shell-top-test-renamed*" t))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buffer &rest _ignored) (setq visited buffer))))
        (call-interactively (key-binding (kbd "RET"))))
      (should (eq visited shell)))))

(ert-deftest agent-shell-top-visit-rejects-stale-rows ()
  (agent-shell-top-tests--with-shells
    (goto-char (point-min))
    (let* ((shell (tabulated-list-get-id))
           (name (buffer-name shell)))
      (kill-buffer shell)
      (should-error (agent-shell-top-visit) :type 'user-error)
      (should-not (get-buffer name)))))

(ert-deftest agent-shell-top-reuses-renamed-overview-and-timer ()
  (agent-shell-top-tests--with-shells
    (goto-char (point-min))
    (forward-line 1)
    (let ((timer agent-shell-top--timer)
          (shell (tabulated-list-get-id)))
      (rename-buffer " *agent-shell-top-test-overview*" t)
      (dotimes (_ 3)
        (should (eq (agent-shell-top) top-buffer))
        (should (eq agent-shell-top--timer timer))
        (should (eq (tabulated-list-get-id) shell)))
      (should (timerp timer))
      (should (memq timer timer-list))
      (should-not (memq timer timer-idle-list)))))

(ert-deftest agent-shell-top-schedules-one-second-regular-timer ()
  (let ((buffer (generate-new-buffer " *agent-shell-top-test-timer*"))
        scheduled cancelled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (time repeat function &rest arguments)
                 (setq scheduled (list time repeat function arguments))
                 'test-timer))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _ignored) (ert-fail "Idle timer used")))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer))))
      (unwind-protect
          (with-current-buffer buffer
            (agent-shell-top-mode)
            (should (equal scheduled
                           (list 1 1 #'agent-shell-top--refresh-buffer
                                 (list buffer)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    (should (eq cancelled 'test-timer))))

(ert-deftest agent-shell-top-scheduled-timer-refreshes-overview ()
  (agent-shell-top-tests--with-shells
    (let ((shell (seq-first shells))
          (deadline (+ (float-time) 5)))
      (should (equal (seq-elt (seq-first (map-elt tabulated-list-entries shell)) 1)
                     "busy"))
      (setf (map-elt statuses shell) 'ready)
      (with-current-buffer shell
        (setq-local default-directory "/tmp/timer-project/"))
      (while (and (not (equal (seq-elt (seq-first (map-elt tabulated-list-entries
                                                        shell))
                                     1)
                             "ready"))
                  (< (float-time) deadline))
        (sleep-for 0.05))
      (should (equal (seq-first (map-elt tabulated-list-entries shell))
                     (vector (buffer-name shell) "ready" "/tmp/timer-project/")))
      (should (memq agent-shell-top--timer timer-list)))))

(ert-deftest agent-shell-top-timer-refreshes-only-its-buffer ()
  (agent-shell-top-tests--with-shells
    (map-put! statuses (seq-first shells) 'ready)
    (with-temp-buffer
      (insert "Unrelated buffer")
      (let ((buffer (current-buffer)))
        (agent-shell-top--refresh-buffer top-buffer)
        (should (eq (current-buffer) buffer))
        (should (equal (buffer-string) "Unrelated buffer"))))
    (should (equal (seq-elt (seq-first (map-elt tabulated-list-entries
                                             (seq-first shells)))
                           1)
                   "ready"))))

(ert-deftest agent-shell-top-kill-cancels-timer-and-allows-reopening ()
  (agent-shell-top-tests--with-shells
    (let ((timer agent-shell-top--timer))
      (kill-buffer top-buffer)
      (should-not (memq timer timer-list))
      (agent-shell-top--refresh-buffer top-buffer)
      (let ((replacement (agent-shell-top)))
        (should (buffer-live-p replacement))
        (should-not (eq replacement top-buffer))
        (with-current-buffer replacement
          (should (memq agent-shell-top--timer timer-list))
          (should-not (eq agent-shell-top--timer timer)))))))

(ert-deftest agent-shell-top-changing-mode-cancels-timer ()
  (agent-shell-top-tests--with-shells
    (let ((timer agent-shell-top--timer))
      (fundamental-mode)
      (should-not (memq timer timer-list))
      (should-not agent-shell-top--timer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Repurposed buffer"))
      (agent-shell-top--refresh-buffer top-buffer)
      (should (equal (buffer-string) "Repurposed buffer"))
      (should-not (eq (agent-shell-top) top-buffer))
      (should (equal (buffer-string) "Repurposed buffer")))))

(ert-deftest agent-shell-top-reinitializing-mode-replaces-timer ()
  (agent-shell-top-tests--with-shells
    (let ((timer agent-shell-top--timer))
      (agent-shell-top-mode)
      (should-not (memq timer timer-list))
      (should (memq agent-shell-top--timer timer-list))
      (should-not (eq agent-shell-top--timer timer))
      (should (eq (agent-shell-top) top-buffer)))))

(ert-deftest agent-shell-top-quit-kills-overview-and-cancels-timer ()
  (agent-shell-top-tests--with-shells
    (let ((timer agent-shell-top--timer))
      (save-window-excursion
        (switch-to-buffer top-buffer)
        (call-interactively (key-binding (kbd "q"))))
      (should-not (buffer-live-p top-buffer))
      (should-not (memq timer timer-list)))))

(provide 'agent-shell-top-tests)

;;; agent-shell-top-tests.el ends here