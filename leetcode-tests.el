(require 'leetcode)

(ert-deftest leetcode-test-slugify-title ()
  (should (equal (leetcode--slugify-title "Two Sum") "two-sum"))
  (should (equal (leetcode--slugify-title "Reverse Nodes in k-Group") "reverse-nodes-in-k-group"))
  (should (equal (leetcode--slugify-title "Pow(x, n)") "powx-n"))
  (should (equal (leetcode--slugify-title "Implement strStr()") "implement-strstr"))
  (should (equal (leetcode--slugify-title "String to Integer (atoi)") "string-to-integer-atoi")))

(ert-deftest leetcode-test-fetch-csrftoken ()
  (should (equal (leetcode--csrf-token) (leetcode--csrf-token))))
