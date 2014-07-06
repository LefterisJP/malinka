;;; test-malinka.el --- Unit tests for malinka.el
;;

;;

;;; Commentary:


;;; Code:
(require 'ert)
(require 'malinka)

(require 'dash)

;; -- Functions used only in testing
(defvar malinka-test/root-dir (f-dirname (f-this-file)))

(defun malinka-test-fail-explain/contained-lists-not-equal (a b)
"Explain why A's and B's contained lists are not equal."
  (let* ((zipped (-zip-with '(lambda (l1 l2)
                               (if (-same-items? l1 l2) t `(,l1 ,l2)))
                             a b))
         (mismatch (--first (not (equal it t)) zipped)))
  (format "Lists %s and %s are not equal"
          (nth 0 mismatch) (nth 1 mismatch))))

(defun malinka-test-contained-lists-equal? (result-list exp-list)
"Test if RESULT-LIST's and EXP-LIST's contained lists are not equal."
  (--all? (equal it t)
          (-zip-with '-same-items? exp-list result-list)))
(put 'malinka-test-contained-lists-equal 'ert-explainer
     'malinka-test-fail-explain/contained-lists-not-equal)

(defun malinka-test-assert-contained-lists-equal (result-list exp-list)
"Assert that RESULT-LIST's and EXP-LIST's contained lists are equal."
  (should (malinka-test-contained-lists-equal? exp-list result-list)))


(defun malinka-test-form-build-cmd (dir command file)
  "Form a build command for DIR COMMAND and FILE."
  (format "{\"directory\":\"%s\", \"command\":\"%s\", \"file\":\"%s\"}"
          dir command file))

(defun malinka-test-turn-to-absolute (file)
  "Turn FILE into absolute path."
    (f-join malinka-test/root-dir file))


;; -- Test Malinka's utility functions
(ert-deftest malinka-test/list-add-list-or-elem/add-list ()
  (should (equal (malinka-list-add-list-or-elem '(1 2 3) '(4 5 6))
                 '(4 5 6 1 2 3))))
(ert-deftest malinka-test/list-add-list-or-elem/add-elem ()
  (should (equal (malinka-list-add-list-or-elem '(1 2 3) 5)
                 '(5 1 2 3))))

(ert-deftest malinka-test/add-if-not-existing/simple ()
  (should (equal
           (malinka-add-if-not-existing
            '(1 2 3) 4 0 '(1 2 3) '("foo" "boo") '(1200 200))
           '((4 1 2 3) ("foo" "boo") (1200 200)))))

;; -- Test Malinka's makefile parsing related functions
(defmacro malinka-test/setup-buildcmd-test-project (&rest commands)
"Setup the environment for the test-project and execute COMMANDS.

This macro exposes the project map as 'map' and the root directory
as 'root-dir' to the COMMANDS.  Afterwards it cleans up so the next
test can start with a clean project-map."
  `(progn
     (setq malinka-files-list-populator 'build-cmd)
     (malinka-define-project
      :name "test_project"
      :root-directory ,(malinka-test-turn-to-absolute "test_project/")
      :makecmd "make -f test_makefile")
     (let* ((map (assoc "test_project" malinka-projects-map))
            (root-dir (malinka-project-map-get root-directory map)))
       (progn ,@commands)
       (malinka-delete-project "test_project"))))


(ert-deftest malinka-test/parse-makefile ()
  (malinka-test/setup-buildcmd-test-project
   (let ((result (malinka-buildcmd-process map root-dir)))
     (malinka-test-assert-contained-lists-equal
      result
      `(("_GNU_SOURCE" "_FILE_OFFSET_BITS=64")
        ("/nice/include/path/" "/good/include/path")
        ("-g" "-Wall")
        (,(malinka-test-turn-to-absolute "test_project/foo.c")
         ,(malinka-test-turn-to-absolute "test_project/boo.c")))))))


(ert-deftest malinka-test/create-json-representation ()
  (malinka-test/setup-buildcmd-test-project
   (let ((json (malinka-create-json-representation nil map root-dir)))
     (should (equal
              json
              (format "[\n%s,\n%s\n]"
                      (malinka-test-form-build-cmd
                       (malinka-test-turn-to-absolute "test_project/")
                       (format "/usr/bin/gcc -c -o %s.o %s.c"
                               (malinka-test-turn-to-absolute "test_project/boo")
                               (malinka-test-turn-to-absolute "test_project/boo"))

                       (malinka-test-turn-to-absolute "test_project/boo.c"))
                      (malinka-test-form-build-cmd
                       (malinka-test-turn-to-absolute "test_project/")
                       (format "/usr/bin/gcc -c -o %s.o %s.c"
                               (malinka-test-turn-to-absolute "test_project/foo")
                               (malinka-test-turn-to-absolute "test_project/foo"))
                       (malinka-test-turn-to-absolute "test_project/foo.c"))))))))


;; -- Test some customization attributes
(ert-deftest malinka-test/build-and-recursive-file-list ()
  (malinka-test/setup-buildcmd-test-project
   (setq malinka-files-list-populator 'build-and-recursive)
   (let ((result (malinka-buildcmd-process
                  map
                  root-dir
                  (malinka-project-recursive-file-search root-dir))))
     (malinka-test-assert-contained-lists-equal
      result
      `(("_GNU_SOURCE" "_FILE_OFFSET_BITS=64")
        ("/nice/include/path/" "/good/include/path")
        ("-g" "-Wall")
        (,(malinka-test-turn-to-absolute "test_project/foo.c")
         ,(malinka-test-turn-to-absolute "test_project/boo.c")
         ,(malinka-test-turn-to-absolute "test_project/goo.c")))))))


(provide 'test-malinka)
;;; test-malinka.el ends here
