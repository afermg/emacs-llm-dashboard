;;; run-tests.el --- Compile and test in batch on Emacs 30/31 -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -L <dependency-dir> ... -l test/run-tests.el
;; Use dependencies built by the selected Emacs (or their .el sources), not
;; .elc/.eln files compiled with a different Emacs major version.

(require 'bytecomp)
(require 'cl-lib)
(require 'ert)

(let* ((root (file-name-directory
              (directory-file-name (file-name-directory load-file-name))))
       (compiled-dir (make-temp-file "llm-dashboard-bytecode-" t))
       (load-path (cons root load-path))
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (file)
          (expand-file-name (concat (file-name-nondirectory file) "c")
                            compiled-dir)))
       (exit-code 1))
  (unwind-protect
      (progn
        (message "Checking llm-dashboard on Emacs %s" emacs-version)
        (dolist (file '("llm-dashboard.el" "llm-dashboard-schedule.el"
                        "llm-dashboard-recurring.el" "llm-dashboard-usage.el"))
          (unless (byte-compile-file (expand-file-name file root))
            (error "Byte compilation failed: %s" file)))
        ;; Compilation loads dependencies for their macros.  Reload every
        ;; compiled module explicitly so tests exercise bytecode, not the
        ;; source definitions already loaded during compilation.
        (let ((load-path (cons compiled-dir load-path)))
          (cl-letf (((symbol-function 'run-with-timer) #'ignore))
            (dolist (file '("llm-dashboard.elc" "llm-dashboard-schedule.elc"
                            "llm-dashboard-recurring.elc" "llm-dashboard-usage.elc"))
              (load (expand-file-name file compiled-dir) nil t)))
          (load (expand-file-name "test/llm-dashboard-test.el" root) nil t)
          (load (expand-file-name "test/llm-dashboard-compat-test.el" root) nil t)
          (let ((stats (ert-run-tests-batch t)))
            (setq exit-code
                  (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))))
    (delete-directory compiled-dir t))
  (kill-emacs exit-code))

;;; run-tests.el ends here
