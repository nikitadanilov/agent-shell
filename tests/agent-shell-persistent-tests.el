;;; agent-shell-persistent-tests.el --- Persistence tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Persistence and restoration regressions without agents or ACP connections.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'agent-shell)
(require 'agent-shell-persistent)

(defvar agent-shell-persistent-tests--directory nil)
(defvar agent-shell-persistent-tests--buffers nil)
(defvar agent-shell-persistent-tests--starts nil)
(defvar agent-shell-persistent-tests--configs nil)

(cl-defun agent-shell-persistent-tests--buffer
    (&key session-id config directory (name "*persistent test*"))
  "Create an isolated shell named NAME with SESSION-ID, CONFIG and DIRECTORY."
  (let ((buffer (generate-new-buffer name)))
    (push buffer agent-shell-persistent-tests--buffers)
    (with-current-buffer buffer
      (setq major-mode 'agent-shell-mode)
      (setq default-directory (or directory agent-shell-persistent-tests--directory))
      (setq-local agent-shell--state
                  (agent-shell--make-state
                   :buffer buffer
                   :agent-config (copy-tree
                                  (or config
                                      (car agent-shell-persistent-tests--configs)))))
      (map-put! (map-elt agent-shell--state :session) :id session-id))
    buffer))

(cl-defun agent-shell-persistent-tests--entry
    (&key (session-id "saved-session") (name "*persistent saved*")
          (directory agent-shell-persistent-tests--directory)
          (identifier 'persistent-test-agent))
  "Make a record with SESSION-ID, NAME, DIRECTORY and configuration IDENTIFIER."
  (list (cons :session-id session-id)
        (cons :buffer-name name)
        (cons :directory directory)
        (cons :agent-config identifier)))

(defun agent-shell-persistent-tests--seed (entries)
  "Write ENTRIES as version-one data independently of the persistence writer."
  (with-temp-file agent-shell-persistent-file
    (prin1 (list (cons :version 1) (cons :shells entries)) (current-buffer))
    (insert "\n")))

(defun agent-shell-persistent-tests--file-text ()
  "Return the persistence file's text for byte-preserving comparisons."
  (with-temp-buffer
    (insert-file-contents-literally agent-shell-persistent-file)
    (buffer-string)))

(defun agent-shell-persistent-tests--start (&rest args)
  "Record startup ARGS and return a shell whose initialisation is pending."
  (push args agent-shell-persistent-tests--starts)
  (should-not agent-shell-persistent-default)
  (agent-shell-persistent-tests--buffer :config (plist-get args :config)
                                      :directory (plist-get args :cwd)))

(defmacro agent-shell-persistent-tests--with-isolation (&rest body)
  "Run BODY with disposable buffers and a disposable persistence directory."
  (declare (indent 0) (debug t))
  `(let* ((agent-shell-persistent-tests--directory
           (file-name-as-directory (make-temp-file "agent-shell-persistent-test-" t)))
          (agent-shell-persistent-file
           (expand-file-name "sessions.el" agent-shell-persistent-tests--directory))
          (agent-shell-persistent-tests--buffers nil)
          (agent-shell-persistent-tests--starts nil)
          (agent-shell-persistent-tests--configs
           '(((:identifier . persistent-test-agent) (:buffer-name . "Test agent"))))
          (agent-shell-persistent-default nil)
          (agent-shell-persistent-mode-hook nil)
          (agent-shell--subscription-counter 0)
          (buffer-list-update-hook nil)
          (kill-buffer-hook nil)
          (kill-buffer-query-functions nil))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-buffers)
                    (lambda () (seq-filter #'buffer-live-p
                                           agent-shell-persistent-tests--buffers)))
                   ((symbol-function 'agent-shell--resolved-agent-configs)
                    (lambda () agent-shell-persistent-tests--configs))
                   ((symbol-function 'agent-shell--start)
                    #'agent-shell-persistent-tests--start)
                   ((symbol-function 'agent-shell--send-request)
                    (lambda (&rest _) (ert-fail "Unexpected ACP request")))
                   ((symbol-function 'agent-shell--sync-system-sleep) #'ignore)
                   ((symbol-function 'agent-shell--uninhibit-sleep) #'ignore))
           ,@body)
       (dolist (buffer agent-shell-persistent-tests--buffers)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq-local buffer-list-update-hook nil)
             (kill-buffer buffer))))
       (delete-directory agent-shell-persistent-tests--directory t))))

(ert-deftest agent-shell-persistent-toggle-saves-only-whitelisted-fields-test ()
  "Enabling persistence saves identifiers and location, not arbitrary state."
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "session-1")
      (insert "Conversation text which must not be persisted")
      (map-put! agent-shell--state :client 'test-client)
      (map-put! (map-elt agent-shell--state :session) :title "Unsaved title")
      (setf (map-elt (map-elt agent-shell--state :agent-config) :environment)
            '(("PERSISTENCE_TEST_ONLY" . "fixture-value")))
      (should-not (file-exists-p agent-shell-persistent-file))
      (agent-shell-persistent-mode 'toggle)
      (should agent-shell-persistent-mode)
      (should
       (equal (read (agent-shell-persistent-tests--file-text))
              (list (cons :version 1)
                    (cons :shells
                          (list (agent-shell-persistent-tests--entry
                                 :session-id "session-1" :name (buffer-name))))))))))

(ert-deftest agent-shell-persistent-defers-saving-until-init-session-test ()
  "An opted-in shell without an ID is saved by the real event subscription."
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer)
      (agent-shell-persistent-mode 1)
      (should-not (file-exists-p agent-shell-persistent-file))
      (agent-shell--emit-event :event 'init-started)
      (should-not (file-exists-p agent-shell-persistent-file))
      (map-put! (map-elt agent-shell--state :session) :id "event-session")
      (agent-shell--emit-event :event 'init-session)
      (should (equal (agent-shell-persistent--read)
                     (list (agent-shell-persistent-tests--entry
                            :session-id "event-session" :name (buffer-name)))))
      (map-put! (map-elt agent-shell--state :session) :id "replacement-session")
      (agent-shell--emit-event :event 'init-session)
      (should (equal (agent-shell-persistent--read)
                     (list (agent-shell-persistent-tests--entry
                            :session-id "replacement-session" :name (buffer-name))))))))

(ert-deftest agent-shell-persistent-rename-buffer-updates-record-test ()
  "A real buffer rename updates the record without manually invoking hooks."
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "rename-session")
      (agent-shell-persistent-mode 1)
      (let ((new-name (generate-new-buffer-name "*persistent renamed*")))
        (rename-buffer new-name)
        (should (equal (agent-shell-persistent--read)
                       (list (agent-shell-persistent-tests--entry
                              :session-id "rename-session" :name new-name))))))))

(ert-deftest agent-shell-persistent-kill-retains-record-test ()
  "Killing a persistent buffer does not forget its saved session."
  (agent-shell-persistent-tests--with-isolation
    (let ((buffer (agent-shell-persistent-tests--buffer :session-id "killed-session")))
      (with-current-buffer buffer
        (agent-shell-persistent-mode 1))
      (let ((saved (agent-shell-persistent-tests--file-text)))
        (kill-buffer buffer)
        (should-not (buffer-live-p buffer))
        (should (equal saved (agent-shell-persistent-tests--file-text)))
        (should (= 1 (length (agent-shell-persistent--read))))))))

(ert-deftest agent-shell-persistent-disable-forgets-and-unsubscribes-test ()
  "Disabling forgets only this record and removes only its subscription."
  (agent-shell-persistent-tests--with-isolation
    (let ((unrelated (agent-shell-persistent-tests--entry :session-id "unrelated")))
      (agent-shell-persistent-tests--seed (list unrelated))
      (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "disable-session")
        (let ((other-token (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                                    :on-event #'ignore)))
          (agent-shell-persistent-mode 1)
          (should (= 2 (length (map-elt agent-shell--state :event-subscriptions))))
          (agent-shell-persistent-mode 'toggle)
          (should-not agent-shell-persistent-mode)
          (should-not agent-shell-persistent--subscription)
          (should-not agent-shell-persistent--entry)
          (should-not (memq #'agent-shell-persistent--maybe-save buffer-list-update-hook))
          (should (equal (mapcar (lambda (sub) (map-elt sub :token))
                                (map-elt agent-shell--state :event-subscriptions))
                         (list other-token)))
          (rename-buffer (generate-new-buffer-name "*disabled persistent*"))
          (agent-shell--emit-event :event 'init-session)
          (should (equal (agent-shell-persistent--read) (list unrelated))))))))

(ert-deftest agent-shell-persistent-default-and-idempotent-enabling-test ()
  "Defaults affect new shells and repeated enabling installs one subscription."
  (agent-shell-persistent-tests--with-isolation
    (should (memq #'agent-shell-persistent--setup agent-shell-mode-hook))
    (let ((older (agent-shell-persistent-tests--buffer :session-id "older"))
          (agent-shell-mode-hook '(agent-shell-persistent--setup)))
      (with-current-buffer older
        (run-hooks 'agent-shell-mode-hook)
        (should-not agent-shell-persistent-mode))
      (should-not (file-exists-p agent-shell-persistent-file))
      (let ((agent-shell-persistent-default t))
        (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "newer")
          (run-hooks 'agent-shell-mode-hook)
          (should agent-shell-persistent-mode)
          (let ((token agent-shell-persistent--subscription))
            (run-hooks 'agent-shell-mode-hook)
            (agent-shell-persistent-mode 1)
            (should (equal token agent-shell-persistent--subscription))
            (should (= 1 (length (map-elt agent-shell--state :event-subscriptions)))))
          (should (= 1 (seq-count
                        (lambda (hook) (eq hook #'agent-shell-persistent--maybe-save))
                        buffer-list-update-hook)))
          (should (= 1 (length (agent-shell-persistent--read)))))
        (with-current-buffer older
          (should-not agent-shell-persistent-mode))))))

(ert-deftest agent-shell-persistent-unchanged-events-do-not-write-test ()
  "Repeated initialisation and buffer-list events must not rewrite the file."
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "unchanged")
      (agent-shell-persistent-mode 1)
      (let ((writes 0)
            (saved (agent-shell-persistent-tests--file-text)))
        (cl-letf (((symbol-function 'agent-shell-persistent--write)
                   (lambda (_entries) (setq writes (1+ writes)))))
          (dotimes (_ 3)
            (agent-shell--emit-event :event 'init-session)
            (agent-shell--emit-event :event 'init-finished)
            (run-hooks 'buffer-list-update-hook)
            (agent-shell-persistent-mode 1)))
        (should (= 0 writes))
        (should (equal saved (agent-shell-persistent-tests--file-text)))))))

(ert-deftest agent-shell-persistent-key-distinguishes-session-config-and-directory-test ()
  "Changing a name replaces one record, not sessions sharing part of its key."
  (agent-shell-persistent-tests--with-isolation
    (let* ((other-directory (expand-file-name "other/" agent-shell-persistent-tests--directory))
           (unrelated (agent-shell-persistent-tests--entry :session-id "unrelated"))
           (primary (agent-shell-persistent-tests--buffer :session-id "shared"))
           (others (list (agent-shell-persistent-tests--buffer :session-id "different")
                         (agent-shell-persistent-tests--buffer
                          :session-id "shared" :config '((:identifier . other-test-agent)))
                         (agent-shell-persistent-tests--buffer
                          :session-id "shared" :directory other-directory))))
      (make-directory other-directory)
      (agent-shell-persistent-tests--seed (list unrelated))
      (dolist (buffer (cons primary others))
        (with-current-buffer buffer (agent-shell-persistent-mode 1)))
      (should (= 5 (length (agent-shell-persistent--read))))
      (let ((others-saved (cons unrelated
                               (mapcar (lambda (buffer)
                                         (buffer-local-value 'agent-shell-persistent--entry buffer))
                                       others))))
        (with-current-buffer primary
          (rename-buffer (generate-new-buffer-name "*persistent key renamed*"))
          (should (= 5 (length (agent-shell-persistent--read))))
          (agent-shell-persistent-mode -1))
        (should (= 4 (length (agent-shell-persistent--read))))
        (dolist (entry others-saved)
          (should (member entry (agent-shell-persistent--read))))))))

(ert-deftest agent-shell-persistent-rejects-invalid-files-without-overwriting-test ()
  "Malformed records, trailing data and unsupported versions remain untouched."
  (agent-shell-persistent-tests--with-isolation
    (let ((entry (agent-shell-persistent-tests--entry)))
      (dolist (text (list "(" "nil" "((:version . 1) (:shells . broken))"
                          "((:version . 2) (:shells))"
                          "((:version . 1) (:shells . (((:session-id . 42)))))"
                          (concat (prin1-to-string
                                   (list (cons :version 1) (cons :shells (list entry))))
                                  "\n(:trailing . data)")))
        (ert-info ((format "Rejected data: %s" text))
          (with-temp-file agent-shell-persistent-file (insert text))
          (should-error (agent-shell-persistent--read) :type 'user-error)
          (should-error (agent-shell-restore-persistent) :type 'user-error)
          (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "not-saved")
            (should-error (agent-shell-persistent-mode 1) :type 'user-error)
            (should-not agent-shell-persistent-mode)
            (should-not agent-shell-persistent--subscription)
            (should-not (map-elt agent-shell--state :event-subscriptions)))
          (should (equal text (agent-shell-persistent-tests--file-text)))
          (should-not agent-shell-persistent-tests--starts))))))

(ert-deftest agent-shell-persistent-rejects-missing-shells-field-test ()
  "A missing record list is malformed, not an empty list safe to overwrite."
  (agent-shell-persistent-tests--with-isolation
    (let ((text "((:version . 1))"))
      (with-temp-file agent-shell-persistent-file (insert text))
      (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "not-saved")
        (should-error (agent-shell-persistent-mode 1) :type 'user-error))
      (should (equal text (agent-shell-persistent-tests--file-text))))))

(ert-deftest agent-shell-persistent-rejects-invalid-entry-fields-test ()
  "Reject invalid field types, missing fields and fields outside the whitelist."
  (agent-shell-persistent-tests--with-isolation
    (dolist (change '((:session-id . "") (:session-id . 42)
                      (:buffer-name . "") (:directory . "relative/")
                      (:agent-config . nil) (:agent-config . "not-a-symbol")))
      (let ((entry (agent-shell-persistent-tests--entry)))
        (map-put! entry (car change) (cdr change))
        (agent-shell-persistent-tests--seed (list entry))
        (should-error (agent-shell-persistent--read) :type 'user-error)))
    (dolist (entry (list (cdr (agent-shell-persistent-tests--entry))
                         (cons '(:extra . "not allowed")
                               (agent-shell-persistent-tests--entry))))
      (agent-shell-persistent-tests--seed (list entry))
      (should-error (agent-shell-persistent--read) :type 'user-error))))

(ert-deftest agent-shell-persistent-file-permissions-test ()
  "New and replaced persistence files are readable and writable only by owner."
  (skip-unless (memq system-type '(gnu gnu/linux gnu/kfreebsd darwin berkeley-unix)))
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer :session-id "permissions")
      (agent-shell-persistent-mode 1)
      (should (= #o600 (logand #o777 (file-modes agent-shell-persistent-file))))
      (set-file-modes agent-shell-persistent-file #o644)
      (rename-buffer (generate-new-buffer-name "*persistent permissions*"))
      (should (= #o600 (logand #o777 (file-modes agent-shell-persistent-file)))))))

(ert-deftest agent-shell-persistent-atomic-write-failure-preserves-file-test ()
  "A failed final rename preserves the old file and removes the temporary file."
  (agent-shell-persistent-tests--with-isolation
    (let ((old (agent-shell-persistent-tests--entry :session-id "old"))
          (replacement (agent-shell-persistent-tests--entry :session-id "replacement"))
          temporary-file)
      (agent-shell-persistent-tests--seed (list old))
      (let ((saved (agent-shell-persistent-tests--file-text)))
        (cl-letf (((symbol-function 'rename-file)
                   (lambda (source destination &optional overwrite)
                     (setq temporary-file source)
                     (should (equal (file-name-directory source)
                                    (file-name-directory agent-shell-persistent-file)))
                     (should (equal destination agent-shell-persistent-file))
                     (should overwrite)
                     (should (file-exists-p source))
                     (signal 'file-error '("Simulated atomic rename failure")))))
          (should-error (agent-shell-persistent--write (list replacement)) :type 'file-error))
        (should temporary-file)
        (should-not (file-exists-p temporary-file))
        (should (equal saved (agent-shell-persistent-tests--file-text)))
        (should (equal (directory-files agent-shell-persistent-tests--directory
                                        nil directory-files-no-dot-files-regexp)
                       '("sessions.el")))))))

(ert-deftest agent-shell-persistent-restore-passes-saved-startup-options-test ()
  "Restoration uses the saved config, ID, CWD and name without stealing focus."
  (agent-shell-persistent-tests--with-isolation
    (let ((entry (agent-shell-persistent-tests--entry
                  :name (generate-new-buffer-name "*persistent original name*")))
          (origin (current-buffer))
          (window (selected-window))
          (shown (window-buffer))
          (agent-shell-persistent-default t))
      (agent-shell-persistent-tests--seed (list entry))
      (let ((restored (agent-shell-restore-persistent)))
        (should (= 1 (length restored)))
        (should (= 1 (length agent-shell-persistent-tests--starts)))
        (let ((args (car agent-shell-persistent-tests--starts)))
          (should (eq (plist-get args :config) (car agent-shell-persistent-tests--configs)))
          (should (equal (plist-get args :cwd) (map-elt entry :directory)))
          (should (equal (plist-get args :session-id) (map-elt entry :session-id)))
          (should (eq (plist-get args :session-strategy) 'new))
          (should (eq (plist-get args :new-session) t))
          (should (eq (plist-get args :no-focus) t))
          (should (eq (plist-get args :resume-session-only) t)))
        (should (eq origin (current-buffer)))
        (should (eq window (selected-window)))
        (should (eq shown (window-buffer)))
        (with-current-buffer (car restored)
          (should (equal (buffer-name) (map-elt entry :buffer-name)))
          (should (equal shell-maker--buffer-name-override (map-elt entry :buffer-name)))
          (should (equal default-directory (map-elt entry :directory)))
          (should agent-shell-persistent-mode)
          (should (equal agent-shell-persistent--restoring entry))
          (should-not (map-nested-elt agent-shell--state '(:session :id)))
          (map-put! (map-elt agent-shell--state :session) :id (map-elt entry :session-id))
          (agent-shell--emit-event :event 'init-session)
          (should-not agent-shell-persistent--restoring)
          (should (equal (agent-shell-persistent--read) (list entry))))))))

(ert-deftest agent-shell-persistent-restore-reuses-active-session-test ()
  "A matching active shell is reused even when its current name differs."
  (agent-shell-persistent-tests--with-isolation
    (let* ((entry (agent-shell-persistent-tests--entry))
           (active (agent-shell-persistent-tests--buffer
                    :session-id (map-elt entry :session-id))))
      (agent-shell-persistent-tests--seed (list entry))
      (dotimes (_ 2)
        (should (equal (agent-shell-restore-persistent) (list active))))
      (should-not agent-shell-persistent-tests--starts)
      (with-current-buffer active
        (should agent-shell-persistent-mode)
        (should (= 1 (length (map-elt agent-shell--state :event-subscriptions)))))
      (should (equal (agent-shell-persistent--read)
                     (list (agent-shell-persistent-tests--entry :name (buffer-name active))))))))

(ert-deftest agent-shell-persistent-restore-reuses-in-progress-session-test ()
  "A second restore does not start a duplicate while an ID is still pending."
  (agent-shell-persistent-tests--with-isolation
    (let ((entry (agent-shell-persistent-tests--entry)))
      (agent-shell-persistent-tests--seed (list entry))
      (let ((first (agent-shell-restore-persistent)))
        (should (= 1 (length first)))
        (should (equal (agent-shell-restore-persistent) first))
        (should (= 1 (length agent-shell-persistent-tests--starts)))
        (with-current-buffer (car first)
          (should-not (map-nested-elt agent-shell--state '(:session :id)))
          (should (equal agent-shell-persistent--restoring entry))
          (should (= 1 (length (map-elt agent-shell--state :event-subscriptions)))))
        (should (equal (agent-shell-persistent--read) (list entry)))))))

(ert-deftest agent-shell-persistent-restore-continues-after-unavailable-entry-test ()
  "Missing configs, missing directories and startup errors retain all records."
  (dolist (failure '(config directory start))
    (agent-shell-persistent-tests--with-isolation
      (let ((bad (agent-shell-persistent-tests--entry :session-id "bad" :name "*bad saved*"))
            (good (agent-shell-persistent-tests--entry :session-id "good" :name "*good saved*"))
            attempts warnings)
        (pcase failure
          ('config (map-put! bad :agent-config 'unavailable-test-agent))
          ('directory (map-put! bad :directory
                               (expand-file-name "missing/" agent-shell-persistent-tests--directory))))
        (agent-shell-persistent-tests--seed (list bad good))
        (let ((saved (agent-shell-persistent-tests--file-text)))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings)))
                    ((symbol-function 'agent-shell--start)
                     (lambda (&rest args)
                       (push (plist-get args :session-id) attempts)
                       (if (and (eq failure 'start)
                                (equal (plist-get args :session-id) "bad"))
                           (error "Simulated startup failure")
                         (apply #'agent-shell-persistent-tests--start args)))))
            (let ((restored (agent-shell-restore-persistent)))
              (should (= 1 (length restored)))
              (should (equal (buffer-name (car restored)) (map-elt good :buffer-name)))))
          (should (= 1 (length warnings)))
          (should (string-match-p "bad saved" (nth 1 (car warnings))))
          (should (equal (nreverse attempts)
                         (if (eq failure 'start) '("bad" "good") '("good"))))
          (should (= 1 (length agent-shell-persistent-tests--starts)))
          (should (equal saved (agent-shell-persistent-tests--file-text))))))))

(ert-deftest agent-shell-persistent-error-event-permits-restore-retry-test ()
  "An asynchronous error releases the in-progress marker without forgetting."
  (agent-shell-persistent-tests--with-isolation
    (let ((entry (agent-shell-persistent-tests--entry)))
      (agent-shell-persistent-tests--seed (list entry))
      (let ((failed (car (agent-shell-restore-persistent)))
            (saved (agent-shell-persistent-tests--file-text)))
        (should (buffer-live-p failed))
        (with-current-buffer failed
          (should agent-shell-persistent--restoring)
          (agent-shell--emit-event :event 'error :data '((:message . "Test failure")))
          (should-not agent-shell-persistent--restoring))
        (should (equal saved (agent-shell-persistent-tests--file-text)))
        (let ((retried (car (agent-shell-restore-persistent))))
          (should (buffer-live-p retried))
          (should-not (eq failed retried))
          (should (= 2 (length agent-shell-persistent-tests--starts)))
          (should (equal (buffer-local-value 'agent-shell-persistent--restoring retried) entry)))
        (should (equal saved (agent-shell-persistent-tests--file-text)))))))

(defmacro agent-shell-persistent-tests--without-core-ui (&rest body)
  "Run BODY through the real error handler without shell output or timers."
  (declare (indent 0) (debug t))
  `(let ((comint-last-prompt nil)
         (agent-shell-path-resolver-function nil)
         (agent-shell-session-restore-verbosity 'full))
     (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
               ((symbol-function 'agent-shell--update-fragment) #'ignore)
               ((symbol-function 'agent-shell--finish-output) #'ignore)
               ((symbol-function 'agent-shell-heartbeat-stop) #'ignore)
               ((symbol-function 'shell-maker-busy) #'ignore)
               ((symbol-function 'agent-shell--mcp-servers) (lambda () [])))
       ,@body)))

(ert-deftest agent-shell-persistent-core-unsupported-resume-does-not-fallback-test ()
  "Strict restoration emits an error when neither resume nor load is supported."
  (agent-shell-persistent-tests--with-isolation
    (with-current-buffer (agent-shell-persistent-tests--buffer)
      (map-put! agent-shell--state :resume-session-id "saved-session")
      (map-put! agent-shell--state :resume-session-only t)
      (let (events fallback initialised)
        (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                  :on-event (lambda (event) (push (map-elt event :event) events)))
        (agent-shell-persistent-tests--without-core-ui
          (cl-letf (((symbol-function 'agent-shell--initiate-new-session)
                     (lambda (&rest _) (push 'new fallback)))
                    ((symbol-function 'agent-shell--initiate-session-list-and-load)
                     (lambda (&rest _) (push 'list fallback))))
            (agent-shell--initiate-session
             :shell-buffer (current-buffer)
             :on-session-init (lambda () (setq initialised t)))))
        (should (equal events '(error)))
        (should-not fallback)
        (should-not initialised)
        (should-not (map-nested-elt agent-shell--state '(:session :id)))
        (should (equal (map-elt agent-shell--state :resume-session-id) "saved-session"))))))

(ert-deftest agent-shell-persistent-core-failed-load-or-resume-does-not-fallback-test ()
  "Strict restoration must not create or select a different session on failure."
  (dolist (capability '(:supports-session-load :supports-session-resume))
    (agent-shell-persistent-tests--with-isolation
      (with-current-buffer (agent-shell-persistent-tests--buffer)
        (map-put! agent-shell--state :resume-session-id "saved-session")
        (map-put! agent-shell--state :resume-session-only t)
        (map-put! agent-shell--state capability t)
        (let (events fallback initialised request on-failure)
          (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                    :on-event (lambda (event) (push (map-elt event :event) events)))
          (agent-shell-persistent-tests--without-core-ui
            (cl-letf (((symbol-function 'agent-shell--initiate-new-session)
                       (lambda (&rest _) (push 'new fallback)))
                      ((symbol-function 'agent-shell--initiate-session-list-and-load)
                       (lambda (&rest _) (push 'list fallback)))
                      ((symbol-function 'agent-shell--send-request)
                       (lambda (&rest args)
                         (setq request (plist-get args :request)
                               on-failure (plist-get args :on-failure)))))
              (agent-shell--initiate-session
               :shell-buffer (current-buffer)
               :on-session-init (lambda () (setq initialised t)))
              (should (equal (map-elt request :method)
                             (if (eq capability :supports-session-load)
                                 "session/load" "session/resume")))
              (should (equal (map-nested-elt request '(:params sessionId)) "saved-session"))
              (should (equal (map-nested-elt request '(:params cwd))
                             (directory-file-name default-directory)))
              (when (eq capability :supports-session-load)
                (should (map-elt agent-shell--state :pending-restore)))
              (should (functionp on-failure))
              (funcall on-failure '((code . -32603) (message . "Test load failure")) nil)))
          (should (equal (nreverse events) '(session-selected error)))
          (should-not fallback)
          (should-not initialised)
          (should-not (map-elt agent-shell--state :pending-restore))
          (should-not (map-nested-elt agent-shell--state '(:session :id)))
          (should (equal (map-elt agent-shell--state :resume-session-id) "saved-session")))))))

(provide 'agent-shell-persistent-tests)

;;; agent-shell-persistent-tests.el ends here