;;; leetcode-tests.el --- Test for leetcode.el       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Test for leetcode.el
;;; Code:

(require 'leetcode)

(ert-deftest leetcode-test-slugify-title ()
  (should (equal (leetcode--slugify-title "Two Sum") "two-sum"))
  (should (equal (leetcode--slugify-title "Reverse Nodes in k-Group") "reverse-nodes-in-k-group"))
  (should (equal (leetcode--slugify-title "Pow(x, n)") "powx-n"))
  (should (equal (leetcode--slugify-title "Implement strStr()") "implement-strstr"))
  (should (equal (leetcode--slugify-title "String to Integer (atoi)") "string-to-integer-atoi")))

(ert-deftest leetcode-test-fetch-csrftoken ()
  (should (equal (leetcode--csrf-token) (leetcode--csrf-token))))

(ert-deftest leetcode-test-login ()
  (let ((leetcode-account
         (with-temp-buffer
           (insert-file-contents "test-account")
           (car (split-string (string-trim (buffer-string)) "\n"))))
        (leetcode-password
         (with-temp-buffer
           (insert-file-contents "test-account")
           (cadr (split-string (string-trim (buffer-string)) "\n")))))
    (setq leetcode--user nil)
    (setq leetcode--problems nil)
    (should (equal (leetcode--login-p) nil))
    (deferred:sync! (leetcode--login leetcode-account leetcode-password))
    (deferred:sync! (leetcode-problems-refresh))
    (should (equal (not (leetcode--login-p)) nil))
    (setq leetcode--user nil)
    (setq leetcode--problems nil)
    (kill-buffer (get-buffer leetcode--buffer-name))))

(ert-deftest leetcode-test-make-tabulated-headers ()
  (should (equal (leetcode--make-tabulated-headers
                  '(" " "#" "Problem" "Acceptance" "Difficulty")
                  '(["✓" "1" "   Two Sum" "43.3%" "easy"]
                    ["✓" "2" "   Add Two Numbers" "30.9%" "medium"]
                    ["✓" "3"    "Longest Substring Without Repeating Characters" "28.2%" "medium"]
                    ["✓" "4"    "Median of Two Sorted Arrays" "26.0%" "hard"]
                    ["✓" "5"    "Longest Palindromic Substring" "27.0%" "medium"]
                    [" " "6"    "ZigZag Conversion" "31.3%" "medium"]))
                 [(" " 1 nil) ("#" 1 nil) ("Problem" 46 nil) ("Acceptance" 10 nil) ("Difficulty" 10 nil)])))

(ert-deftest leetcode-test-get-code-buffer-name ()
  (let ((leetcode-prefer-language "c"))
    (should (equal (leetcode--get-code-buffer-name "String to Integer (atoi)")
                   "string-to-integer-atoi.c")))
  (let ((leetcode-prefer-language "java"))
    (should (equal (leetcode--get-code-buffer-name "Pow(x, n)")
                   "powx-n.java")))
  (let ((leetcode-prefer-language "python3"))
    (should (equal (leetcode--get-code-buffer-name "Reverse Nodes in k-Group")
                   "reverse-nodes-in-k-group.py"))))

(provide 'leetcode-tests)
;;; leetcode-tests.el ends here
