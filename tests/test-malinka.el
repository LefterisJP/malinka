;;; test-malinka.el --- Unit tests for malinka.el
;;

;;

;;; Commentary:


;;; Code:
(require 'ert)
(require 'malinka)

(require 'dash)

;; -- Functions used only in testing
(defun malinka-test-fail-explain/contained-lists-not-equal (a b)
  (let* ((zipped (-zip-with '(lambda (l1 l2)
                               (if (-same-items? l1 l2) t `(,l1 ,l2)))
                             a b))
         (mismatch (--first (not (equal it t)) zipped)))
  (format "Lists %s and %s are not equal"
          (nth 0 mismatch) (nth 1 mismatch))))

(defun malinka-test-contained-lists-equal (result-list exp-list)
  (--all? (equal it t)
          (-zip-with '-same-items? exp-list result-list)))
(put 'malinka-test-contained-lists-equal 'ert-explainer
     'malinka-test-fail-explain/contained-lists-not-equal)

(defun malinka-test-assert-contained-lists-equal (result-list exp-list)
  (should (malinka-test-contained-lists-equal exp-list result-list)))



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
(ert-deftest malinka-test/parse-makefile ()
  (malinka-define-project
   :name "test_project"
   :root-directory "./test_project"
   :makecmd "make -f test_makefile")
  (let* ((map (assoc "test_project" malinka-projects-map))
         (root-dir "./test_project/")
         (result (malinka-buildcmd-process map root-dir)))
    (malinka-test-assert-contained-lists-equal
     result
     '(("_GNU_SOURCE" "_FILE_OFFSET_BITS=64")
       ("/nice/include/path/" "/good/include/path")
       ("-g" "-Wall" "-lm")))))

(provide 'test-malinka)
;;; test-malinka.el ends here
