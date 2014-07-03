;;; test-malinka.el --- Unit tests for malinka.el
;;

;; Copyright © 2014 Lefteris Karapetsas <lefteris@refu.co>
;;

;;; Commentary:


;;; Code:
(require 'ert)
(require 'malinka)

;; -- Testing utility functions
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

;; -- Testing makefile parsing related functions
(ert-deftest malinka-test/parse-makefile ()
  (malinka-define-project
   :name "test_project"
   :root-directory "./test_project"
   :makecmd "make -f test_makefile")
  (let* ((map (assoc "test_project" malinka-projects-map))
         (root-dir "./test_project/")
         (result (malinka-buildcmd-process map "test_project" root-dir)))
    ;;TODO
    ;; (should (equal result
    ;;                '(
    ))

(provide 'test-malinka)
;;; test-malinka.el ends here
