;;; malinka.el --- A C/C++ project configuration package for Emacs
;;

;; Copyright © 2014 Lefteris Karapetsas <lefteris@refu.co>
;;
;; Author: Lefteris Karapetsas <lefteris@refu.co>
;; URL: https://github.com/LefterisJP/malinka
;; Keywords: c c++ project-management
;; Version: 0.2.0
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
(defgroup malinka nil
  "An Emacs c/c++ project manager"
  :group 'tools ;; Emacs -> Programming -> tools
  :prefix "malinka-"
  :link '(url-link :tag "Github" "https://github.com/LefterisJP/malinka"))

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
  :package-version '(malinka . "0.1.0"))

(defcustom malinka-ignored-directories '(".git" ".hg")
  "A list of directories to ignore for file searching."
  :group 'malinka
  :type '(repeat (string :tag "Ignored directory"))
  :safe #'malinka-string-list-p
  :package-version '(malinka . "0.2.0"))

(defcustom malinka-supported-compilers '("gcc" "cc" "g++" "clang")
"A list of compiler executable names that are recognized and supported by malinka."
  :group 'malinka
  :type '(repeat (string :tag "Supported compilers"))
  :safe #'malinka-string-list-p
  :package-version '(malinka . "0.2.0"))

(defcustom malinka-supported-file-types '("c" "cc" "cpp" "tcc")
"File extensions that malinka will treat as related source files."
  :group 'malinka
  :type '(repeat (string :tag "Supported file types"))
  :safe #'malinka-string-list-p
  :package-version '(malinka . "0.2.0"))

(defcustom malinka-files-list-populator 'build-and-recursive
"Decides how malinka will populate the files list of a project.

`recursive'
     Populates the files-list of a project by recursively searching
     inside the root-directory of the project and gathering all files
     whose extension is a member of `malinka-supported-file-types'.

`build-cmd'
     Populates the files-list of a project by using the `build-cmd'
     argument and trying to determine the files by parsing the
     make commands.

`build-and-recursive'
     Populates the files-list of a project by combining the behaviour
     of both `recursive' and `build-cmd.' This is the default."
  :group 'malinka
  :type '(choice (const :tag "Recursive file search" recursive)
                 (const :tag "Build command file search" build-cmd)
                 (const :tag "Build command and recursive file search"
                        build-and-recursive))
  :package-version '(malinka . "0.2.0"))

;;; --- Global project variables ---

(defvar malinka-current-project-name nil)
(defvar malinka-projects-map '())
(defvar malinka-macro-cppflags '() "The current project's cpp flags.")
(defvar malinka-include-dirs '()
  "The current project's compiler include directories.")

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

(defun malinka-file-make-absolute (root-dir file)
  "Use ROOT-DIR to turn FILE into its absolute path version."
  (f-join root-dir file))
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
  (-contains? malinka-supported-file-types (f-ext file)))

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
                                 (same-name-check t)
                                 (build-cmd nil))
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

A user can provide a `build-cmd' such as 'make -f target_makefile' for malinka
to attempt to parse that and get all project attributes.

The project is added to the global `malinka-projects-map'
"
(unless (stringp name) (malinka-error "Provided non-string for project name"))
(unless (or (not build-cmd) (stringp build-cmd))
  (malinka-error "Provided non-string for the project's build command"))
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

  ;; to avoid some association list problems
  ;; delete it if it already exists before redefining
  (when (assoc name malinka-projects-map)
      (malinka-delete-project name))

  (add-to-list 'malinka-projects-map
               `(,name . ((name . ,name)
                          (compiler-executable . ,compiler-executable)
                          (compiler-flags . ,compiler-flags)
                          (cpp-defines . ,cpp-defines)
                          (include-dirs . ,include-dirs)
                          (root-directory . ,root-directory)
                          (same-name-check . ,same-name-check)
                          (build-cmd . ,build-cmd)))))

(defun malinka-delete-project (name)
  "Delete project NAME from the projects map."
  (setq malinka-projects-map (assq-delete-all name malinka-projects-map)))


(defun malinka-list-add-list-or-elem (list elem)
  "Add element to LIST.

ELEM can be either a single element or another list"
  (if (listp elem)
      (append elem list)
      (cons elem list)))

(defun* malinka-update-project-map  (map &key
                                         (cpp-defines '())
                                         (compiler-flags '())
                                         (include-dirs '()))
  (let* ((new-cpp-defines
          (malinka-list-add-list-or-elem
           cpp-defines (malinka-project-map-get cpp-defines map)))
         (new-include-dirs
          (malinka-list-add-list-or-elem
           include-dirs (malinka-project-map-get include-dirs map)))
         (new-compiler-flags
          (malinka-list-add-list-or-elem
           compiler-flags (malinka-project-map-get compiler-flags map)))
         (name (malinka-project-map-get name map))
         (compiler-executable (malinka-project-map-get compiler-executable map))
         (root-directory (malinka-project-map-get root-directory map))
         (same-name-check (malinka-project-map-get same-name-check map))
         (makefile (malinka-project-map-get makefile map))
         (build-cmd (malinka-project-map-get build-cmd map)))
    ;; first delete the project from the project list
    (malinka-delete-project name)
    ;; then add the updated project map to malinka projects
    (setq malinka-projects-map
          (add-to-list 'malinka-projects-map
                 `(,name . ((name . ,name)
                            (compiler-executable . ,compiler-executable)
                            (compiler-flags . ,new-compiler-flags)
                            (cpp-defines . ,new-cpp-defines)
                            (include-dirs . ,new-include-dirs)
                            (root-directory . ,root-directory)
                            (same-name-check . ,same-name-check)
                            (makefile . ,makefile)
                            (build-cmd . ,build-cmd)))))
    ;; return the updated project map
    (assoc name malinka-projects-map)))


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

(defun malinka-json-format-escapes (str)
  "Unescapes/escape special characters before signing off a json encoded STR."
  (s-replace "\\\/" "/" str))

(defun malinka-project-command-form-defines (cpp-defines)
  "Form the CPP-DEFINES part of the build command."
  (s-join " "
          (--map (s-prepend "-D" (malinka-json-format-escapes it)) cpp-defines)))

(defun malinka-project-command-form-includes (include-dirs)
  "Form the INCLUDE-DIRS part of the build command."
  (s-join " "
          (--map (s-prepend "-I" (malinka-json-format-escapes it)) include-dirs)))

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
              (malinka-project-command-form-defines cpp-defines)
              " "
              (malinka-project-command-form-includes include-dirs)
              " -c -o "
              (s-append ".o " (f-no-ext file))
              file)))

(defun malinka-project-create-json-list (project-map
                                         files-list
                                         root-dir)
"Create the json association list for this project.

Read the PROJECT-MAP and use the FILES-LIST and all the attributes
of a project to create the commands.
Finally ROOT-DIR determines the root directory to write to the file."
    (-map
     (lambda (item)
       (let* ((command-string
               (malinka-project-command-from-map
                project-map item)))
         (json-encode-alist
          `((directory . ,root-dir)
            (command . ,command-string)
            (file . ,item))))) files-list))

(defun malinka-create-json-representation (files-list
                                           project-map
                                           given-root-dir)
"Return the json representation that should go into the compilation DB.

The contents are defined by reading all the relevant files from the
FILES-LIST and by getting the cpp-defines and the root-directory
from the PROJECT-MAP.

If there is a GIVEN-ROOT-DIR then this is used instead of the one taken
from the project map"
(let* ((root-dir
        (if given-root-dir
            given-root-dir
          (f-canonical (cdr (assoc 'root-directory (cdr project-map))))))
       (processed-list (malinka-buildcmd-process project-map root-dir files-list))
       (updated-files-list (if processed-list (nth 3 processed-list) files-list))
       (updated-map
        (if processed-list
            (malinka-update-project-map project-map
                                        :cpp-defines (car processed-list)
                                        :include-dirs (nth 1 processed-list)
                                        :compiler-flags (nth 2 processed-list))
             ;;else
             project-map)))

  ; build an association list with all the data for each file
  ; json-encode does not seem to work for a list of dicts, so we
  ; have to build it manually
  (let ((json-list (malinka-project-create-json-list updated-map
                                                     updated-files-list
                                                     root-dir)))
    (format "[\n%s\n]" (s-join
                        ",\n" (-map 'malinka-json-format-escapes json-list))))))

(defun malinka-project-recursive-file-search (root-dir)
  "Find all c/c++ files under ROOT-DIR.

If `malinka-files-list-populator' is `build-cmd' then no search is
performed."
  (unless (eq malinka-files-list-populator 'build-cmd))
    (--filter (not (f-dir? it))
              (f--entries root-dir (malinka-file-dir-p it) t)))

(defun malinka-compiledb-write (str name)
  "Create and write STR to db file NAME."
  (f-touch name)
  (f-write-text  str 'utf-8 name)
  (message (format "malinka: Created %s" name)))

(defun malinka-project-compiledb-create (project-map root-dir)
  "Create a json compile database for the PROJECT-MAP.

Creates the configuration for NAME with ROOT-DIR.
For more information on the compilations database please refer here:
http://clang.llvm.org/docs/JSONCompilationDatabase.html"
  (let* ((project-files (malinka-project-recursive-file-search root-dir))
         (db-file-name (f-join root-dir "compile_commands.json"))
         (json-string
          (malinka-create-json-representation
           project-files project-map root-dir)))
    (malinka-compiledb-write json-string db-file-name)))

(defun malinka-project-map-update-compiledb (project-map root-dir)
  "Update the compilation database for PROJECT-MAP and ROOT-DIR."
  (when (malinka-rtags-assert-rdm-runs)
    (malinka-project-compiledb-create project-map root-dir)
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


; --- Makefile/Build command reading ---
(defun malinka-buildcmd-line-get-file (line)
"Process a LINE of a build command and determine the file being compiled."
  (let* ((words (s-split " " line))
         (last-word (car (last words))))
    (if (malinka-file-p last-word)
        last-word
      ;else
      (progn
        (malinka-error "Could not determine the file compiled by:\n%s"
                     line)
        nil))))


(defun malinka-buildcmd-line-begins-with-compile (line)
  "Determine wether LINE begins with a compile command.
If it does return the list of 'words' contained in the line.
If not return nil."
  (let* ((words (s-split " " line))
         (first-word (car words)))
    (if (-contains? malinka-supported-compilers first-word)
        words
      nil)))

(defun malinka-add-if-not-existing (list elem ind &rest vars)
"Add to LIST the ELEM at IND if not existing.

Return VARS list if existing and if not existing returns the VARS
list with ELEM appended to the IND sublist."
  (if (-contains? list elem)
      vars
    ;;else
    (-replace-at ind (cons elem (nth ind vars)) vars)))

(defun malinka-buildcmd-ignore-argument-p (arg)
  "Return true if ARG of the build command should be ignored."
  (or
   ;; ignore object files
   (s-ends-with? ".o" arg)
   ;; ignore -o argument
   (equal "-o" arg)
   ;; ignore -c argument
   (equal "-c" arg)))

(defun malinka-buildcmd-process-word (input-list word)
  "Read the INPUT-LIST and process the WORD of a compile command."
  (let ((cpp-defines (car input-list))
        (include-dirs (nth 1 input-list))
        (compiler-flags (nth 2 input-list))
        (files-list (nth 3 input-list))
        (root-dir (nth 4 input-list)))
    (cond
     ((s-starts-with? "-D" word)
      (let ((cpp-define (s-chop-prefix "-D" word)))
        (malinka-add-if-not-existing cpp-defines cpp-define
                                     0 cpp-defines include-dirs
                                     compiler-flags files-list root-dir)))

     ((s-starts-with? "-I" word)
      (let ((include-dir
             ;; TODO: think about include dirs and absolute or not
             ;; (malinka-file-make-absolute root-dir (s-chop-prefix "-I" word))))
             (s-chop-prefix "-I" word)))
        (malinka-add-if-not-existing include-dirs include-dir
                                     1 cpp-defines include-dirs
                                     compiler-flags files-list root-dir)))


     ((malinka-file-p word)
      (let ((file (malinka-file-make-absolute root-dir word)))
        ;; If it's a source file and config allows add it to the file-list
        (unless (eq malinka-files-list-populator 'recursive)
          (malinka-add-if-not-existing files-list file
                                       3 cpp-defines include-dirs
                                       compiler-flags files-list root-dir))))

     ((malinka-buildcmd-ignore-argument-p word)
      input-list)

     ((-contains? malinka-supported-compilers word)
      ;; If it's a compiler argument (should be first)
      ;; then this should help us determine the compiler.
      ;; TODO: but .. for now ignore
      input-list)

     (:else
      ;; All other choices would be compiler flags
      (malinka-add-if-not-existing compiler-flags word
                                   2 cpp-defines include-dirs
                                   compiler-flags files-list root-dir)))))

(defun malinka-buildcmd-line-link-cmd-p (words)
  "Return t if the line comprised of WORDS is a linking related cmd.

The logic is that if no source file is found in the command then it's a linking
command.  Maybe a better way could be devised ..."
  (not (-any? 'malinka-file-p words)))


(defun malinka-buildcmd-process-line (input-list line)
  "Read the INPUT-LIST and if LINE is a compile command, process it."
  (let ((words (malinka-buildcmd-line-begins-with-compile line)))
    (if (and words (not (malinka-buildcmd-line-link-cmd-p words)))
        (-reduce-from 'malinka-buildcmd-process-word input-list words)
      ;; else return unchanged
      input-list)))

(defun malinka-build-cmd-to-str (build-cmd root-dir)
"Turn the BUILD-CMD of project at ROOT-DIR into a string."
(s-replace "\\\"" "\"" (shell-command-to-string (format "cd %s && %s -n" root-dir build-cmd))))

(defun malinka-buildcmd-process (project-map root-dir &optional files-list)
  "Read the build commands from a project's makefile.

Read the PROJECT-MAP and ROOT-DIR combination thas has a makefile
and returns a list of lists.  Optionally if we want to have a starting
FILES-LIST it can be provided as an argument.

This function shoud only be called if we know the project-map contains
a build command.  If it does not then NIL is returned.

The returned list has the form:
'(CPP-DEFINES INCLUDE-DIRS COMPILER-FLAGS FILES-LIST)"
  (let ((build-cmd (malinka-project-map-get build-cmd project-map)))
    (when build-cmd
      (let* ((cmd-output (malinka-build-cmd-to-str build-cmd root-dir))
             ;; get the compile commands from the output
             (lines (s-lines cmd-output)))
        (-reduce-from
         'malinka-buildcmd-process-line
         `(,(malinka-project-map-get cpp-defines project-map)
           ,(malinka-project-map-get include-dirs project-map)
           ,(malinka-project-map-get compiler-flags project-map)
           ,files-list
           ,root-dir)
         lines)))))


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
