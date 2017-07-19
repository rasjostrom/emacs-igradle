
(require 's)
(require 'hydra)

(defcustom igradle-exec "/home/rasjostrom/.sdkman/candidates/gradle/4.0/bin/gradle"
  "String representation of path to gradle."
  :group 'gradle
  :type 'string)

(defcustom igradle-project-root nil
  "String representation of path to the project root."
  :group 'gradle
  :type 'string)

(defcustom igradle-build-file nil
  "String representation of path to target gradle.build."
  :group 'gradle
  :type 'string)

;;; -------------------------------------------------------------
;;; Internal Functions

;; FIXME: Not reliable enough
(defun igradle-locate-project-root ()
  "Set the default build root by finding the closest build file
moving up the file hierarchy."
  (setq igradle-project-root
	(locate-dominating-file default-directory "build.gradle"))
  (setq igradle-build-file
	(concat igradle-project-root "build.gradle")))

(defun igradle-execute (tasks &optional quiet)
  "Run gradle with TASKS. Locate project root and build file if
unknown. Unless QUIET is non-nil, output will be opened in a new
window."
  (unless igradle-project-root
    (igradle-locate-project-root))
  (let ((output
	 (shell-command-to-string
	  (s-join " " (list igradle-exec "-b " igradle-build-file tasks)))))
    (if quiet
	(message "Done!")
      (igradle-temp-buffer output))))

(defun igradle-temp-buffer (content)
  "Displays CONTENT in a new buffer & window that is compatible
with hydra (Help Mode is not)."
  (with-temp-buffer-window
   "*Gradle View*" nil nil
   (with-current-buffer "*Gradle View*"
     (insert content))
   (setq other-window-scroll-buffer t)
   (igradle-menu-read/body)))

(defun igradle-restore-temp-buffer (&optional exit)
  "Try to restore the frame to its original state."
  (quit-restore-window (get-buffer-window "*Gradle View*"))
  (setq other-window-scroll-buffer nil)
  (igradle-menu/body))

(defmacro deftask (name tasks)
  "Generate interactive functions for gradle tasks."
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
(deftask "describe-project" "project")

(defun igradle-run (tasks)
  "Execute gradle command with TASKS supplied by user input."
  (interactive "sTask(s) to run: ")
  (igradle-execute tasks))

(defun igradle-describe-task (task)
  "Display descriptive information about a task."
  (interactive "sTask: ")
  (igradle-execute (concat "-q help --task " task)))

(defun igradle-which-build? ()
  "Display the selected build file in the message buffer."
  (interactive)
  (message (concat "TARGET BUILD FILE: " igradle-build-file)))

(defun igradle-mode-restart ()
  (interactive)
  (igradle-mode 0)
  (igradle-mode 1))

;;; -------------------------------------------------------------
;;; Keybindings

(defvar igradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g e") 'igradle-execute)
    (define-key map (kbd "C-c C-g c") 'igradle-clean)
    (define-key map (kbd "C-c C-g b") 'igradle-build)
    (define-key map (kbd "C-c C-g j") 'igradle-jar)
    (define-key map (kbd "C-c C-g t") 'igradle-test)
    (define-key map (kbd "C-c C-c") 'igradle-menu/body)
    map)
  "Keymap for the gradle minor mode.")

(define-minor-mode igradle-mode
  "Interactive minor-mode for gradle projects."
  :lighter " igradle"
  :keymap 'igradle-mode-map)

;;; -------------------------------------------------------------
;;; Hydra UI

(defhydra igradle-menu-read (:color amaranth :hint nil)
  "
  _p_ Scroll Up    _n_ Scroll Down
  "
  ("p" (scroll-other-window-down 5))
  ("n" (scroll-other-window 5))
  ("b" (igradle-restore-temp-buffer) "Back" :exit t)
  ("q" nil "Quit"))

(defhydra igradle-menu-project (:color amaranth :hint nil)
  "


  _s_ Set Build File            %`igradle-build-file


  "
  ("s" igradle-set-build)
  ("b" igradle-menu/body "Back" :color blue)
  ("q" nil "Quit"))

(defhydra igradle-menu (:color pink :hint nil)
  "

  Build File: %`igradle-build-file
  Project Root: %`igradle-project-root
  -------------------------------------------

  "
  ("c" igradle-clean "Clean" :column "Compile")
  ("b" igradle-build "Build")
  ("d" igradle-describe-project "Describe Project" :column "Help" :exit t)
  ("t" igradle-list-tasks "List Available Tasks" :exit t)
  ("p" igradle-menu-project/body "Project" :exit t :column "Menu")
  ("q" nil "quit"))

(provide 'igradle-mode)
