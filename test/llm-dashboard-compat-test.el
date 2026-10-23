;;; llm-dashboard-compat-test.el --- Emacs 30/31 integration tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'llm-dashboard)

;; Load the optional modules without starting their background scans.  No
;; native terminal or agent process is needed for these integration tests.
(cl-letf (((symbol-function 'run-with-timer) #'ignore))
  (require 'llm-dashboard-schedule)
  (require 'llm-dashboard-recurring)
  (require 'llm-dashboard-usage))

(ert-deftest llm-dashboard-test-no-legacy-claude-api ()
  "Loading all modules exposes only the canonical dashboard API."
  (dolist (feature '(claude-dashboard claude-dashboard-schedule
                    claude-dashboard-recurring claude-dashboard-usage))
    (should-not (featurep feature))
    (should-not (locate-library (symbol-name feature))))
  (should-not (apropos-internal "\\`claude-dashboard\\(?:-\\|\\'\\)"
                               #'fboundp))
  (should-not (apropos-internal "\\`claude-dashboard\\(?:-\\|\\'\\)"
                               #'boundp))
  ;; Removing the legacy namespace must not remove the Claude backend.
  (should (equal "claude" (llm-dashboard--backend-prop :program 'claude)))
  (dolist (slot '(:session-id-fn :session-name-fn :transcript-path-fn
                 :transcript-walk-fn :list-sessions-fn :rename-fn :interrupt-fn))
    (should (fboundp (llm-dashboard--backend-prop slot 'claude)))))

(ert-deftest llm-dashboard-test-optional-module-bindings ()
  "Optional modules attach to llm-dashboard without the legacy feature."
  (dolist (binding '(("S" . llm-dashboard-schedule-send)
                     ("J" . llm-dashboard-schedule-launch)
                     ("L" . llm-dashboard-list-pending-sends)
                     ("r" . llm-dashboard-list-recurring)
                     ("U" . llm-dashboard-usage-list)))
    (should (commandp (cdr binding)))
    (should (eq (lookup-key llm-dashboard-mode-map (car binding))
                (cdr binding)))))

(ert-deftest llm-dashboard-test-resize-hook-window-and-frame-contexts ()
  "Resize hooks target the dashboard regardless of the current buffer.
Emacs 30 and 31 differ in the current buffer used for local window hooks."
  (save-window-excursion
    (let ((llm-dashboard--refresh-timer t)
          (window (selected-window))
          refreshed)
      (with-temp-buffer
        (llm-dashboard-mode)
        (let ((dashboard (current-buffer)))
          (should (memq #'llm-dashboard--on-window-size-change
                        window-size-change-functions))
          (set-window-buffer window dashboard)
          (cl-letf (((symbol-function 'llm-dashboard-refresh)
                     (lambda () (push (current-buffer) refreshed))))
            ;; Buffer-local hooks receive a window; global hooks a frame.
            (dolist (argument (list window (window-frame window)))
              (setq-local llm-dashboard--last-rendered-width nil)
              (setq refreshed nil)
              (with-temp-buffer
                (llm-dashboard--on-window-size-change argument))
              (should (equal (list dashboard) refreshed))
              (should (= llm-dashboard--last-rendered-width
                         (window-text-width window)))
              ;; An unchanged width must not cause another render.
              (with-temp-buffer
                (llm-dashboard--on-window-size-change argument))
              (should (equal (list dashboard) refreshed)))))))))

(ert-deftest llm-dashboard-test-resize-hook-ignores-dead-window ()
  "A stale window argument does not attempt to access its former buffer."
  (save-window-excursion
    (let ((window (split-window)))
      (delete-window window)
      (cl-letf (((symbol-function 'llm-dashboard-refresh)
                 (lambda () (ert-fail "Unexpected refresh"))))
        (should-not (llm-dashboard--on-window-size-change window))))))

(ert-deftest llm-dashboard-test-refresh-requests-coalesce ()
  "Repeated UI hooks queue one refresh, and the callback releases it."
  (let ((llm-dashboard--queued-refresh-timer nil)
        (timer (timer-create))
        (queued 0)
        (refreshed 0))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (delay repeat function)
                 (should (= delay 0))
                 (should-not repeat)
                 (should (eq function #'llm-dashboard--run-queued-refresh))
                 (cl-incf queued)
                 timer))
              ((symbol-function 'llm-dashboard--refresh-if-visible)
               (lambda () (cl-incf refreshed))))
      (llm-dashboard--queue-refresh)
      (llm-dashboard--queue-refresh)
      (should (= queued 1))
      (llm-dashboard--run-queued-refresh)
      (should (= refreshed 1))
      (should-not llm-dashboard--queued-refresh-timer)
      (llm-dashboard--queue-refresh)
      (should (= queued 2)))))

(ert-deftest llm-dashboard-test-sqlite-unavailable ()
  "Builds without SQLite keep the optional opencode adapter harmless."
  (let ((llm-dashboard--opencode-unavailable-warned nil)
        ran)
    (cl-letf (((symbol-function 'sqlite-available-p) (lambda () nil))
              ((symbol-function 'message) #'ignore))
      (should-not
       (llm-dashboard--with-opencode-db db
         (setq ran t)))
      (should-not ran)
      (should llm-dashboard--opencode-unavailable-warned))))

(ert-deftest llm-dashboard-test-sqlite-read-and-close ()
  "The database wrapper reads and closes using APIs shared by Emacs 30/31."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (let ((path (make-temp-file "llm-dashboard-sqlite-" nil ".db"))
        connection)
    (unwind-protect
        (progn
          (let ((db (sqlite-open path)))
            (unwind-protect
                (sqlite-execute db "CREATE TABLE sample (value TEXT)")
              (sqlite-close db)))
          (cl-letf (((symbol-function 'llm-dashboard--opencode-db-path)
                     (lambda () path)))
            (should (equal '((0))
                           (llm-dashboard--with-opencode-db db
                             (setq connection db)
                             (sqlite-select db "SELECT count(*) FROM sample"))))
            (should-error (sqlite-select connection "SELECT 1"))
            ;; The connection must also close after a query error.
            (should-not (llm-dashboard--with-opencode-db db
                          (setq connection db)
                          (sqlite-select db "SELECT * FROM missing_table")))
            (should-error (sqlite-select connection "SELECT 1"))))
      (delete-file path))))

(provide 'llm-dashboard-compat-test)
;;; llm-dashboard-compat-test.el ends here
