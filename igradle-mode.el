
(require 's)
(require 'find-lisp)
(require 'hydra)
(require 'find-file-in-project)

(defcustom igradle-exec nil
  "String representation of path to gradle."
  :group 'gradle
  :type 'string)

(defvar igradle-project-root nil
  "String representation of path to the project root.")

(defvar igradle-target-build nil
  "String representation of path to target gradle.build.")

(defvar igradle-build-files nil
  "List of available build files.")

;;; Code:
;;; -------------------------------------------------------------
;;; Internal Functions

(defun igradle-use-wrapper ()
  (setq igradle-exec (concat igradle-project-root "gradlew")))

(defun igradle-locate-project-root ()
  "Set default project root."
  (setq igradle-project-root
	(locate-dominating-file default-directory ".igradle")))

(defun igradle-locate-build-files ()
  "Locate available gradle build files. Try to set a default
target build file if none is selected."
  (unless igradle-target-build
    (setq igradle-target-build
	  (concat (or igradle-project-root
		      (igradle-locate-project-root)) "build.gradle"))
  (setq igradle-build-files (find-lisp-find-files igradle-project-root "build.gradle"))))

(defun igradle-execute (tasks &optional quiet)
  "Run gradle with TASKS.
Locate project root and build file if unknown.  If QUIET is nil
output will be opened in a new window.

If igradle-target-build is nil, igradle-locate-project-root will
automatically try to set a default build file."
  (unless igradle-target-build (igradle-locate-build-files))
  (unless igradle-exec (igradle-use-wrapper))
  (let ((output
	 (shell-command-to-string
	  (s-join " " (list igradle-exec "-b " igradle-target-build tasks)))))
    (if quiet (message "Done!")
      (igradle-temp-buffer output))))

(defun igradle-temp-buffer (content)
  "Display CONTENT in a new buffer & window with a hydra."
  (with-temp-buffer-window
   "*Gradle View*" nil nil
   (with-current-buffer "*Gradle View*"
     (insert content))
   (setq other-window-scroll-buffer t)
   (igradle-menu-read/body)))

(defun igradle-restore-temp-buffer ()
  "Try to restore the frame to its original state."
  (quit-restore-window (get-buffer-window "*Gradle View*"))
  (setq other-window-scroll-buffer nil)
  (igradle-menu/body))

(defmacro deftask (name tasks)
  "Generate an interactive function called NAME executing TASKS."
  (let ((funsymbol (intern (concat "igradle-" name))))
    `(defun ,funsymbol ()
       (interactive)
       (igradle-execute ,tasks))))

;;; -------------------------------------------------------------
;;; Interactive Functions

(deftask "clean" "clean")
(deftask "build" "build")
(deftask "jar" "jar")
(deftask "test" "test")
(deftask "list-tasks" "-q tasks")
(deftask "describe-project" "-q project")

(defun igradle-run (tasks)
  "Execute gradle command with TASKS supplied by user input."
  (interactive "sTask(s) to run: ")
  (igradle-execute tasks))

(defun igradle-describe-task (task)
  "Display descriptive information about TASK."
  (interactive "sTask: ")
  (igradle-execute (concat "-q help --task " task)))

(defun igradle-describe-build ()
  "Display the selected build file in the message buffer."
  (interactive)
  (message (concat "Target Build: " igradle-target-build)))

(defun igradle-select-build ()
  "Interactively select target build file."
  (interactive)
  (unless igradle-build-files
    (igradle-locate-build-files))
  (ffip-completing-read
   (format "Build file in '%s':" igradle-project-root)
   igradle-build-files
   (lambda (file)
     (setq igradle-target-build file))))

;;; -------------------------------------------------------------
;;; Keybindings

(defvar igradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g e") 'igradle-run)
    (define-key map (kbd "C-c C-g c") 'igradle-clean)
    (define-key map (kbd "C-c C-g b") 'igradle-build)
    (define-key map (kbd "C-c C-g j") 'igradle-jar)
    (define-key map (kbd "C-c C-g t") 'igradle-test)
    (define-key map (kbd "C-c C-g s") 'igradle-select-build)
    (define-key map (kbd "C-c C-c") 'igradle-menu/body)
    map)
  "Keymap for igradle-mode.")

;;; -------------------------------------------------------------
;;; Hydra UI

(defhydra igradle-menu-read (:color amaranth :hint nil)
  "\n
  _p_ Scroll Up    _n_ Scroll Down
  \n"
  ("p" (scroll-other-window-down 5))
  ("n" (scroll-other-window 5))
  ("b" (igradle-restore-temp-buffer) "Back" :exit t)
  ("q" nil "Quit"))

(defhydra igradle-menu (:color pink :hint nil)
  "\n
  Build File: %`igradle-target-build
  Project Root: %`igradle-project-root
  -------------------------------------------
  \n"
  ("c" igradle-clean "Clean" :column "Compile")
  ("b" igradle-build "Build")
  ("d" igradle-describe-project "Describe Project" :column "Help" :exit t)
  ("t" igradle-list-tasks "List Available Tasks" :exit t)
  ("s" igradle-select-build "Select Build File" :column "Menu")
  ("q" nil "quit"))

(define-minor-mode igradle-mode
  "Interactive minor-mode for gradle projects."
  :lighter " igradle"
  :keymap 'igradle-mode-map)

(provide 'igradle-mode)
;;; igradle-mode.el ends here
