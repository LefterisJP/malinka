;;; malinka.el --- A C/C++ project configuration package for Emacs
;;

;; Copyright © 2014 Lefteris Karapetsas <lefteris@refu.co>
;;
;; Author: Lefteris Karapetsas <lefteris@refu.co>
;; URL: https://github.com/LefterisJP/malinka
;; Keywords: c c++ project-management
;; Version: 0.1.0
;; Package-Requires: ((s "1.9.0") (dash "2.4.0") (f "0.11.0") (cl-lib "0.3") (rtags "0.0") (projectile "0.11.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;; Malinka is a project management Emacs package for C/C++
;;
;; It uses rtags to help the user jump around the code easily and without the
;; mistaken tag jumping that other taggers frequently have with C/C++ code.
;; The main functionality of malinka is to properly populate and communicate the
;; compiler commands to the rtags daemons depending on the project you are working
;; on.
;;
;; Optionally and if you also have flycheck with the clang syntax-checker activated
;; malinka will communicate to flycheck's clang syntax checker the appropriate
;; cpp-defines and include paths so that flycheck can do its syntax checking.
;;
;; The way to define a project is by using `malinka-define-project' and to provide
;; the basic attributes that a project needs.  For more information you can read
;; the function's docstring and the readme file.  For a quick introduction you can
;; visit this blog post http://blog.refu.co/?p=1311

;;; Code:

(eval-when-compile
(require 'cl))

(require 'projectile)
(require 's)
(require 'dash)
(require 'f)
(require 'json)
(require 'rtags)



;;; --- Customizable variables ---
(defcustom malinka-completion-system nil
  "The completion system to use.

Inspired by flycheck's choice of completion system.
Docstrings are also taken from there.

`ido'
     Use IDO.

     IDO is a built-in alternative completion system, without
     good flex matching and a powerful UI.  You may want to
     install flx-ido (see URL `https://github.com/lewang/flx') to
     improve the flex matching in IDO.

nil
     Use the standard unfancy `completing-read'.

     `completing-read' has a very simple and primitive UI, and
     does not offer flex matching.  This is the default setting,
     though, to match Emacs' defaults.  With this system, you may
     want enable option `icomplete-mode' to improve the display
     of completion candidates at least."
  :group 'malinka
  :type '(choice (const :tag "IDO" ido)
                 (const :tag "Completing read" nil))
  :package-version '(malinka . "0.1"))


(defvar malinka-current-project-name nil)
(defvar malinka-projects-map '())
(defvar malinka-macro-cppflags '() "The current project's cpp flags.")
(defvar malinka-include-dirs '()
  "The current project's compiler include directories.")
(defvar malinka-ignored-directories '(".git" ".hg")
  "These directories will be ignored during file search.")

; --- Helper Macros ---

(defmacro malinka-generate-project-name-getter (attribute)
"Generate getter function for ATTRIBUTE given a project name."
  `(defun ,(intern (format "malinka-project-name-get-%s" (symbol-name attribute)))
     (name)
     (let ((project-map (assoc name malinka-projects-map)))
       (when 'project-map
         (cdr (assoc ',attribute (cdr project-map)))))))

(defmacro malinka-generate-project-map-getter (attribute)
"Generate getter function for ATTRIBUTE given a project map."
  `(defun ,(intern (format "malinka-project-map-get-%s" (symbol-name attribute)))
     (map)
       (when 'map
         (cdr (assoc ',attribute (cdr map))))))

; Instead of generating getters for everything, just use these 2 macros
(defmacro malinka-project-name-get (attribute name)
"Get the value of list ATTRIBUTE for project NAME."
  `(let ((project-map (assoc ,name malinka-projects-map)))
     (malinka-project-map-get ,attribute project-map)))

(defmacro malinka-project-name-get-single (attribute name)
"Get the value of list ATTRIBUTE for project NAME."
  `(let ((project-map (assoc ,name malinka-projects-map)))
     (malinka-project-map-get-single ,attribute project-map)))

(defmacro malinka-project-map-get (attribute map)
"Get the value of list ATTRIBUTE for project-map MAP."
  `(when ,map
     (cdr (assoc ',attribute (cdr ,map)))))

(defmacro malinka-project-map-get-single (attribute map)
"Get the value of single ATTRIBUTE for project-map MAP."
  `(when ,map
     (assoc ',attribute (cdr ,map))))


(defmacro malinka-error (fmt &rest args)
"Issue an error, by passing FMT and ARGS to (error)."
`(error (concat "Malinka-error: " ,fmt) ,@args))

;;; --- Utility functions ---

(defun malinka-process-relative-dirs (input-list project-root)
  "Process the INPUT-LIST and return relative dirs to PROJECT-ROOT."
  (--map-when
   (or (s-starts-with? "../" it) (s-starts-with? "./" it))
   (s-prepend project-root it) input-list))

;;; --- Predicate functions ---

(defun malinka-string-list-p (obj)
  "Determine if OBJ is a list of strings.
Copied from flycheck.el and not used directly to not introduce dependency"
  (and (listp obj) (-all? #'stringp obj)))

(defun malinka-configure-project-p (name)
  "Check if project NAME should be configured.

Returns true if the project with NAME exists in the project map and
if it has the same name check predicate then it also checks that it's not
the current project."
  (let ((project-map (assoc name malinka-projects-map)))
    (when project-map
      ; if we need to check for the name, do that, else return true
      (if (cdr (assoc 'same-name-check (cdr project-map)))
          (not (string= malinka-current-project-name name))
        t))))

(defun malinka-file-p (file)
"Return non-nil only if the FILE is related to C/C++."
(or (f-ext? file "c")
    (f-ext? file "cpp")
    (f-ext? file "cc")
    (f-ext? file "tcc")))

(defun malinka-file-dir-p (file)
"Return true if FILE is a file or directory of interest.

File of interest means that it's a C/C++ file and directory of interest
is basically any directory except known ignored directories"
(if (f-file? file)
    (malinka-file-p file)
  (not (-contains? 'malinka-ignored-directories (f-filename file)))))

;;; --- Elisp internal API
(defun* malinka-define-project (&key (name "Project Name")
                                 (compiler-executable "/usr/bin/gcc")
                                 (cpp-defines '())
                                 (compiler-flags '())
                                 (include-dirs '())
                                 (root-directory nil)
                                 (same-name-check t))
"Define a c/c++ project named NAME.

NAME should be the same as the file-name of the root-directory of the project

In COMPILER-EXECUTABLE provide the path to the executable of the compiler that
you will be using in order to compile the project

Provide a list of strings in CPP-DEFINES for the cpp defines used by
the project.

In COMPILER-FLAGS provide arguments that should be passed to the compiler

Provide a list of strings in INCLUDE-DIRS for the include paths used by
this project.

Provide the ROOT-DIRECTORY of the project

If SAME-NAME-CHECK is non nil then even if a file of the same project is visited
the c/c++ configuration will be regenerated. If `same-name-check' is non nil
 then even if a file of the same project is visited the c/c++ configuration
 will be regenerated. That is useful in situations where multiple versions
  of a project exist in different directories.

Having `same-name-check' nil in those cases allows us to simultaneously develop on
the different versions of the project which reside in the different directories and
keep different defines and include directories for each. Be default it is set to
true.

The project is added to the global `malinka-projects-map'
"
(unless (stringp name) (malinka-error "Provided non-string for project name"))
(unless (or (not compiler-executable) (f-executable? compiler-executable))
  (malinka-error "Can't find compiler executable \"%s\"" compiler-executable))
(when root-directory
  (unless (f-directory? root-directory)
    (malinka-error
     "Provided root directory of project \"%s\" does not exist" name)))
(unless (malinka-string-list-p cpp-defines)
  (malinka-error
   "Provided non all-string list for project \"%s\" defines" name))
(unless (malinka-string-list-p compiler-flags)
  (malinka-error
   "Provided non all-string list for project \"%s\" compiler flags" name))
  (unless (malinka-string-list-p include-dirs)
  (malinka-error
   "Provided non all-string list for project \"%s\" includes" name))

  (add-to-list 'malinka-projects-map
               `(,name . ((name . ,name)
                          (compiler-executable . ,compiler-executable)
                          (compiler-flags . ,compiler-flags)
                          (cpp-defines . ,cpp-defines)
                          (include-dirs . ,include-dirs)
                          (root-directory . ,root-directory)
                          (same-name-check . ,same-name-check)))))

(defun* malinka-update-project-map  (map &key
                                         (cpp-defines '())
                                         (compiler-flags '())
                                         (include-dirs '()))
  (let* ((new-cpp-defines
          (cons cpp-defines (malinka-project-map-get cpp-defines map)))
         (new-include-dirs
          (cons include-dirs (malinka-project-map-get include-dirs map)))
         (new-compiler-flags
          (cons compiler-flags (malinka-project-map-get compiler-flags map)))
         (name (malinka-project-map-get name map))
         (compiler-executable (malinka-project-map-get compiler-executable map))
         (root-directory (malinka-project-map-get root-directory map))
         (same-name-check (malinka-project-map-get same-name-check map)))
  (add-to-list 'malinka-projects-map
               `(,name . ((name . ,name)
                          (compiler-executable . ,compiler-executable)
                          (compiler-flags . ,new-compiler-flags)
                          (cpp-defines . ,new-cpp-defines)
                          (include-dirs . ,new-include-dirs)
                          (root-directory . ,root-directory)
                          (same-name-check . ,same-name-check))))))


(defun malinka-defined-project-names ()
  "Return all defined project names sorted alphabetically."
  (let ((projects (-map
                   (lambda (it) (cdr (assoc 'name (cdr it))))
                   malinka-projects-map)))
    (sort projects #'string<)))

(defun malinka-project-detect-root ()
  "Attempts to detect the project root for the current buffer.

Basically uses projectile's root searching utilities.
No need to reinvent the wheel."
  (let* ((dir (file-truename default-directory))
         (found-dir (--reduce-from
                     (or acc (funcall it dir)) nil
                     projectile-project-root-files-functions)))
    (when found-dir (file-truename found-dir))))


(defun malinka-project-detect-name ()
"Detect the name of the project of the current buffer."
  (let ((dir (malinka-project-detect-root)))
    (when dir
      (malinka-project-name-from-root dir))))

(defun malinka-project-name-from-root (root-dir)
  "Deduce project name from ROOT-DIR."
  (when root-dir
    (file-name-nondirectory (directory-file-name root-dir))))




;;; --- rtags integration ---
(defun malinka-rtags-invoke-with (&rest args)
  "Invoke rc (rtags executable) with ARGS as arguments.

Returns the output of the command as a string or nil in case of error"
  (when (malinka-rtags-assert-rdm-runs)
    (let* ((rc (rtags-executable-find "rc"))
           (cmd (s-join " " (cons rc args))))
      (when rc
        (shell-command-to-string cmd)))))


(defun malinka-rtags-file-indexed-p (filename)
  "Ask rtags if it knows about FILENAME."
  (let ((output (s-trim (malinka-rtags-invoke-with "--is-indexed" filename))))
    (string-equal output "indexed")))

(defun malinka-rtags-assert-rdm-runs ()
  "Assert that the rtags daemon is running."
  ; if the process has been messed with by outside sources clean it up
  (let ((status (if rtags-process (process-status rtags-process) nil)))
    (when (or (not status) (memq status '(exit signal closed failed)))
      (when rtags-process
        (delete-process rtags-process))
      (setq rtags-process nil)
      (when (get-buffer "*rdm*")
        (kill-buffer "*rdm*"))))
  (if (rtags-start-process-maybe)
      t
      ;else
      (malinka-error "Could not find rtags daemon in the system")))


;;; --- Functions related to creating the compilation database ---

(defun malinka-json-escape-paths (str)
"Escape any directory separators in STR, for json encoding."
  (s-replace "\\\/" "/" str))

(defun malinka-project-command-from-map (project-map file)
"Form the compile command for a PROJECT-MAP and a specific FILE."
  (let ((cpp-defines (malinka-project-map-get cpp-defines project-map))
        (compiler-flags (malinka-project-map-get compiler-flags project-map))
        (include-dirs (malinka-project-map-get include-dirs project-map))
        (executable (malinka-project-map-get compiler-executable project-map)))
    (s-concat executable
              " "
              (s-join " " compiler-flags)
              " "
              (s-join " " (--map (s-prepend "-D" it) cpp-defines))
              " "
              (s-join " " (--map (s-prepend "-I" it) include-dirs))
              " -c -o "
              (s-append ".o " (f-no-ext file))
              file)))


(defun malinka-create-json-representation (files-list project-map given-root-dir)
"Return the json representation that should go into the compilation DB.

The contents are defined by reading all the relevant files from the
FILES-LIST and by getting the cpp-defines and the root-directory
from the PROJECT-MAP.

If there is a GIVEN-ROOT-DIR then this is used instead of the one taken
from the project map"
(let ((root-dir
        (if given-root-dir
           given-root-dir
         (f-canonical (cdr (assoc 'root-directory (cdr project-map)))))))
  ; build an association list with all the data for each file
  ; json-encode does not seem to work for a list of dicts, so we
  ; have to build it manually
  (let ((json-list (-map
                    (lambda (item)
                      (let* ((command-string
                              (malinka-project-command-from-map
                               project-map item)))
                        (json-encode-alist
                         `((directory . ,root-dir)
                           (command . ,command-string)
                           (file . ,item))))) files-list)))
    (format "[\n%s\n]" (s-join
                        ",\n" (-map 'malinka-json-escape-paths json-list))))))

(defun malinka-project-compiledb-create (project-map root-dir)
  "Create a json compile database for the PROJECT-MAP.

Creates the configuration for NAME with ROOT-DIR.
For more information on the compilations database please refer here:
http://clang.llvm.org/docs/JSONCompilationDatabase.html"
  (let* ((project-files
          (--filter (not (f-dir? it))
                    (f--entries root-dir (malinka-file-dir-p it) t)))
         (db-file-name (f-join root-dir "compile_commands.json"))
         (json-string
          (malinka-create-json-representation
           project-files project-map root-dir)))
    (f-touch db-file-name)
    (f-write-text  json-string 'utf-8 db-file-name)
    (message (format "malinka: Created %s" db-file-name))))

(defun malinka-project-map-update-compiledb (project-map root-dir)
  "Update the compilation database for PROJECT-MAP and ROOT-DIR."
  (malinka-project-compiledb-create project-map root-dir)
  (when (malinka-rtags-assert-rdm-runs)
    (with-temp-buffer
      (rtags-call-rc "-W" root-dir)
      (rtags-call-rc "-J" root-dir))))



; --- Minibuffer utilities ---
(defvar malinka-read-project-history nil
  "`completing-read' history of `malinka-read-project'.")

(defun malinka-default-project ()
"Select a default project if possible.  If not return nil."
(let ((name (malinka-project-detect-name)))
  (when (-contains? (malinka-defined-project-names) name)
    name)))



(defun malinka-read-project (prompt &optional default)
"Select a malinka project from minibuffer with PROMPT.

If DEFAULT is provided then this is shown as the default
choice of a prompt.

Returns the project as string or nil if not found."
(let* ((candidates (malinka-defined-project-names))
       (input (pcase malinka-completion-system
                (`ido (ido-completing-read prompt candidates nil
                                           'require-match default
                                           'malinka-read-project-history
                                           default))
                (_ (completing-read prompt candidates nil 'require-match
                                    default 'malinka-read-project-history
                                    default)))))
  (if (string= input "")
      (user-error "No project name entered")
    input)))


; --- Interactive functions ---

;;;###autoload
(defun malinka-project-configure (name given-root-dir)
  "Configure a project by querying for both NAME and GIVEN-ROOT-DIR.

If multiple projects with the same name in different directories may
exist then it's nice to provide the ROOT-DIR of the project to configure"
  (interactive
   (let* ((project-name
           (malinka-read-project "Project: " (malinka-default-project)))
          (project-root-dir (malinka-project-name-get root-directory project-name))
          (given-dir (if project-root-dir project-root-dir
                       (read-directory-name "Project root: "))))
     (list project-name given-dir)))
  (let ((root-dir (f-canonical given-root-dir))
        (project-map (assoc name malinka-projects-map)))
    (if project-map
        (malinka-project-map-update-compiledb project-map root-dir)
      ;; else - given project NAME not found
      (message "malinka: Project %s is not known. Use malinka-define-project to fix this" name))))

;;;###autoload
(defun malinka-project-add-file (file-name project-name)
  "Add FILE-NAME to PROJECT-NAME.

Adds the file to the project map and also makes sure that rtags
indexes the file."
  (interactive (list
                (read-file-name "File name: " nil (buffer-file-name) t)
                (malinka-read-project "Project: " (malinka-default-project))))
  (unless (malinka-rtags-file-indexed-p file-name)
    (let* ((map (assoc project-name malinka-projects-map))
           (cmd (malinka-project-command-from-map map file-name)))
      (malinka-rtags-invoke-with "--compile" cmd "--curent-file" file-name))))

;;;###autoload
(defun malinka-project-add-include-dir (include-dir project-name given-root-dir)
  "Add INCLUDE-DIR to PROJECT-NAME with the GIVEN-ROOT-DIR.

Adds the include directory to the project map and also makes sure that rtags
knows about this additional include directory."
  (interactive
   (let* ((include (read-directory-name "Directory name: "))
          (project-name
           (malinka-read-project "Project: " (malinka-default-project)))
          (project-root-dir (malinka-project-name-get root-directory project-name))
          (given-dir (if project-root-dir project-root-dir
                       (read-directory-name "Project root: "))))
     (list include project-name given-dir)))
  (let ((map (assoc project-name malinka-projects-map)))
    (malinka-update-project-map map :include-dirs include-dir)
    (malinka-project-map-update-compiledb map given-root-dir)
    (malinka-update-flycheck-include-dirs map)))


;;;###autoload
(defun malinka-project-add-cpp-define (define project-name given-root-dir)
  "Add DEFINE to PROJECT-NAME with the GIVEN-ROOT-DIR.

Adds the preprocessor define to the project map and also makes sure that rtags
knows about this additional include directory."
  (interactive
   (let* ((define (read-string "Define: "))
          (project-name
           (malinka-read-project "Project: " (malinka-default-project)))
          (project-root-dir (malinka-project-name-get root-directory project-name))
          (given-dir (if project-root-dir project-root-dir
                       (read-directory-name "Project root: "))))
     (list define project-name given-dir)))
  (let ((map (assoc project-name malinka-projects-map)))
    (malinka-update-project-map map :cpp-defines define)
    (malinka-project-map-update-compiledb map given-root-dir)
    (malinka-update-flycheck-cpp-defines map)))


;;; --- Interface with flycheck if existing ---

(defun malinka-update-flycheck-cpp-defines (map)
"If MAP's project is current, update the flycheck clang definitions."
  (let ((name (malinka-project-map-get name map))
         (defines (malinka-project-map-get cpp-defines map)))
    (when (string= malinka-current-project-name name)
      (setq flycheck-clang-definitions defines))))

(defun malinka-update-flycheck-include-dirs (map)
"If MAP's project is current, update the flycheck clang include path."
  (let ((name (malinka-project-map-get name map))
         (includes (malinka-project-map-get cpp-defines map)))
    (when (string= malinka-current-project-name name)
      (setq flycheck-clang-include-path includes))))

(eval-after-load 'flycheck
  (progn
    (defun malinka-flycheck-clang-interface()
      "Configure flycheck clang's syntax checker according to what we know."
      (with-current-buffer (current-buffer)
        (let* ((project-root (malinka-project-detect-root))
               (project-name (malinka-project-name-from-root project-root)))
          (when (malinka-configure-project-p project-name)
            (setq malinka-current-project-name project-name)
            (let ((includes-res
                   (malinka-process-relative-dirs
                    (malinka-project-name-get include-dirs project-name)
                    project-root))
                  (cppflags-res (malinka-project-name-get cpp-defines project-name)))
              (if includes-res
                  (setq malinka-include-dirs includes-res)
                (setq malinka-include-dirs '()))
              (if cppflags-res
                  (setq malinka-macro-cppflags cppflags-res)
                (setq malinka-macro-cppflags '()))))
          ;; whether we switched project or not assert modules are configured
          (setq flycheck-clang-definitions malinka-macro-cppflags)
          (setq flycheck-clang-include-path malinka-include-dirs))))

    (add-hook 'flycheck-before-syntax-check-hook 'malinka-flycheck-clang-interface)))

(provide 'malinka)
;;; malinka.el ends here
