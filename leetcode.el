;;; leetcode.el --- An emacs leetcode client.          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools

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

;;

;;; Code:


(require 'json)
(require 'url-cookie)

(require 'shr)
(require 'furl)
(require 'dash)
(require 'graphql)


(defcustom leetcode-account ""
  "leetcode login account."
  :type 'string
  :group 'leetcode)

(defcustom leetcode-password ""
  "leetcode login password."
  :type 'string
  :group 'leetcode)

(defvar leetcode--user nil
  "User object.
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number
")

(defvar leetcode--problems nil
  "Problems info with a list of problem object.
:num      Number
:tag      String
:problems List

    :status     String
    :id         Number
    :title      String
    :acceptance String
    :difficulty Number {1,2,3}
    :paid-only  Boolean {t|nil}
")

(defvar leetcode--check-mark "✓")
(defvar leetcode--problem-buffer-name "*leetcode:problem*")

;;; Login
;; URL
(defconst leetcode--domain "leetcode.com")
(defconst leetcode--base-url "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--base-url "/accounts/login"))

;; Cookie
(defconst leetcode--csrftoken "csrftoken")
(defconst leetcode--cookie-localpart-separator "/")

;; Header
(defconst leetcode--Referer          `("Referer" . ,leetcode--url-login))
(defconst leetcode--User-Agent       '("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken      "X-CSRFToken")
(defconst leetcode--login-csrf-key   "csrfmiddlewaretoken")
(defconst leetcode--login-account    "login")
(defconst leetcode--login-password   "password")

;; API
(defconst leetcode--api-root (concat leetcode--base-url "/api"))
(defconst leetcode--api-all-problems (concat leetcode--api-root "/problems/all/"))
(defconst leetcode--api-graphql (concat leetcode--base-url "/graphql"))

(defun leetcode--gen-csrf-token ()
  "Knock knock, please generate the csrf token."
  (url-retrieve-synchronously leetcode--url-login))

(defun leetcode--csrf-token ()
  (let (token)
    (catch 'break
      (dolist (cur (url-cookie-retrieve leetcode--domain leetcode--cookie-localpart-separator t))
        (when (string-equal leetcode--csrftoken (aref cur 1))
          (setq token (aref cur 2))
          (throw 'break "Found the csrf token"))))
    (unless token
      (leetcode--gen-csrf-token)
      (setq token (leetcode--csrf-token)))
    token))

(defun leetcode-login (account password)
  (interactive (unless (and (boundp 'account) (boundp 'password))
                 (list (read-string "account: ")
                       (read-passwd "password: "))))
  (let* ((csrftoken (leetcode--csrf-token))
         (url-request-method "POST")
         (url-request-extra-headers (list leetcode--Referer
                                          leetcode--User-Agent
                                          leetcode--X-Requested-With
                                          (cons leetcode--X-CSRFToken csrftoken)))
         (furl-request-data (list (cons leetcode--login-csrf-key csrftoken)
                                  (cons leetcode--login-account account)
                                  (cons leetcode--login-password password)))
         (furl-request-files t))
    (furl-retrieve-synchronously leetcode--url-login)))

(defun leetcode--fetch-user-and-problems ()
  "leetcode set `leetcode--user' and `leetcode--problems', if user isn't login,
only `leetcode--problems' will be set."
  (let ((url-request-method "GET")
        (url-request-extra-headers (list leetcode--Referer
                                         leetcode--User-Agent
                                         leetcode--X-Requested-With)))
    (with-current-buffer (url-retrieve-synchronously leetcode--api-all-problems)
      (let ((result (json-read-object)))
        ;; user
        (setq leetcode--user (plist-put leetcode--user :username (alist-get 'user_name result)))
        (setq leetcode--user (plist-put leetcode--user :solved (alist-get 'num_solved result)))
        (setq leetcode--user (plist-put leetcode--user :easy (alist-get 'ac_easy result)))
        (setq leetcode--user (plist-put leetcode--user :medium (alist-get 'ac_medium result)))
        (setq leetcode--user (plist-put leetcode--user :hard (alist-get 'ac_hard result)))
        ;; problem list
        (setq leetcode--problems (plist-put leetcode--problems :num (alist-get 'num_total result)))
        (setq leetcode--problems (plist-put leetcode--problems :tag "all"))
        (setq leetcode--problems
              (plist-put leetcode--problems :problems
                         (let ((raw-vec (alist-get 'stat_status_pairs result))
                               (len (plist-get leetcode--problems :num))
                               problems)
                           (dolist (i (number-sequence 0 (1- len)))
                             (let* ((cur (aref raw-vec i))
                                    (stat (alist-get 'stat cur))
                                    (status (alist-get 'status cur))
                                    (difficulty (alist-get 'level (alist-get 'difficulty cur)))
                                    (paid-only (eq (alist-get 'paid_only cur) t))
                                    (total-submitted (alist-get 'total_submitted stat))
                                    (total-acs (alist-get 'total_acs stat)))
                               (push
                                (list
                                 :status status
                                 :id (- len i)
                                 :title (alist-get 'question__title stat)
                                 :acceptance (concat
                                              (number-to-string
                                               (/ (fround (* 1000 (/ (float total-acs) total-submitted))) 10))
                                              "%")
                                 :difficulty difficulty
                                 :paid-only paid-only)
                                problems)))
                           problems)))))))

(defun leetcode--title-slug (title)
  (let* ((str1 (replace-regexp-in-string "\s+" "-" (downcase title)))
         (str2 (replace-regexp-in-string "(" "" str1))
         (str3 (replace-regexp-in-string ")" "" str2))
         (res (replace-regexp-in-string "," "" str3)))
    res))

(defun leetcode--graphql-params (opration &optional vars)
  (list
   (cons "operationName" "questionData")
   (cons "query" (graphql-query
                  questionData
                  (:arguments (($titleSlug . String!))
                              (question :arguments ((titleSlug . ($ titleSlug)))
                                        likes
                                        dislikes
                                        content
                                        (topicTags slug)
                                        (codeSnippets langSlug code)))))
   (if vars (cons "variables" vars))))

(defun leetcode--parse-problem (title)
  "
:likes     Number
:dislikes  Number
:content   String
:topicTags String
"
  (let* ((csrftoken (leetcode--csrf-token))
         (title-slug (leetcode--title-slug title))
         (url-request-method "POST")
         (url-request-extra-headers (list leetcode--User-Agent
                                          (cons "Content-Type" "application/json")))
         (url-request-data (json-encode (leetcode--graphql-params
                                         "questionData"
                                         (list (cons "titleSlug" title-slug))))))
    (with-current-buffer (url-retrieve-synchronously leetcode--api-graphql)
      (alist-get'question (alist-get 'data (json-read))))))

(defun leetcode--replace-in-buffer (regex to)
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun leetcode--make-tabulated-headers (column-names rows)
  "Column width calculated by picking the max width of every cell
under that column and the column name."
  (let ((widths
         (-reduce-from
          (lambda (acc row)
            (-zip-with
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          (-map #'length column-names)
          rows)))
    (cl-map
     #'vector #'identity
     (-zip-with
      (lambda (col size) (list col size nil))
      column-names widths))))

(defun leetcode-problems--refresh ()
  (let* ((column-names '(" " "#" "Problem" "Acceptance" "Difficulty"))
         (rows (let ((problems (reverse (plist-get leetcode--problems :problems)))
                     rows)
                 (dolist (p problems)
                   (setq rows
                         (cons
                          (vector
                           (if (string-equal (plist-get p :status) "ac")
                               leetcode--check-mark
                             " ")
                           (number-to-string (plist-get p :id))
                           (plist-get p :title)
                           (plist-get p :acceptance)
                           (cond
                            ((eq 1 (plist-get p :difficulty)) "easy")
                            ((eq 2 (plist-get p :difficulty)) "medium")
                            ((eq 3 (plist-get p :difficulty)) "difficult")))
                          rows)))
                 rows))
         (headers (leetcode--make-tabulated-headers column-names rows)))
    (setq tabulated-list-format headers)
    (setq tabulated-list-entries
          (-zip-with
           (lambda (i x) (list i x))
           (-iterate '1+ 0 (length rows))
           rows)))
  (tabulated-list-init-header))

(defun leetcode ()
  "Show leetcode problems buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*leetcode*"))
  (leetcode-problems-mode)
  (leetcode-problems--refresh)
  (tabulated-list-print t))

(defvar leetcode-problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-show-description)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)))
  "Keymap for `leetcode-problems-mode'")

(setq leetcode-problems-mode-map
      (let ((map (make-sparse-keymap)))
        (prog1 map
          (suppress-keymap map)
          (define-key map (kbd "RET") #'leetcode-show-description)
          (define-key map "n" #'next-line)
          (define-key map "p" #'previous-line))))

(defun leetcode-show-description ()
  "Show current entry problem description. Get current entry by
using `tabulated-list-get-entry' and use `shr-render-buffer' to
render problem description."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (id (aref entry 1))
         (title (aref entry 2))
         (difficulty (aref entry 4))
         (problem (leetcode--parse-problem title))
         (content (alist-get 'content problem))
         (dislikes (alist-get 'dislikes problem))
         (likes (alist-get 'likes problem))
         (snippets (alist-get 'codeSnippets problem))
         (buf-name "*leetcode-description*")
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (with-temp-buffer
      (insert (concat "<h1>" id ". " title "</h1>"))
      (insert (concat (capitalize difficulty) html-margin
                      "likes: " (number-to-string likes) html-margin
                      "dislikes: " (number-to-string dislikes)))
      (insert (alist-get 'content problem))
      (setq shr-current-font t)
      (leetcode--replace-in-buffer "" "")
      (shr-render-buffer (current-buffer)))
    (with-current-buffer "*html*"
      (save-match-data
        (re-search-forward "dislikes: .*" nil t)
        (insert (make-string 4 ?\s))
        (insert-text-button "solve it"
                            'action (lambda (btn) (leetcode-start-coding title (append snippets nil)))
                            'help-echo "solve the problem."))
      (rename-buffer buf-name)
      (leetcode-problem-description-mode)
      (switch-to-buffer (current-buffer)))))

(defvar leetcode-prefer-language "python3"
  "LeetCode programming language.")

(defconst leetcode--prefer-language-suffixes
  '(("python" . ".py") ("python3" . ".py") ("java" . ".java") ("cpp" . ".cpp")
    ("c" . ".c") ("csharp" . ".cs") ("javascript" . ".js") ("ruby" . ".rb")
    ("swift" . ".swift") ("golang" . ".go") ("scala" . ".scala") ("kotlin" . ".kt")
    ("rust" . "rs") ("php" . ".php"))
  " python, python3, java, cpp, c, csharp, javascript, ruby,
swift, golang, scala, kotlin, rust, php")

(defun leetcode-start-coding (title snippets)
  "Create a buffer which is not associated with any file for
  coding. It will choose major mode by `leetcode-prefer-language'
  and `auto-mode-alist'."
  (let ((suffix (assoc-default
                 leetcode-prefer-language
                 leetcode--prefer-language-suffixes))
        snippet)
    (catch 'break
      (dolist (s snippets)
        (when (string-equal (alist-get 'langSlug s) leetcode-prefer-language)
          (message snippet)
          (setq snippet (alist-get 'code s))
          (throw 'break "Found target snippet."))))
    (with-current-buffer (get-buffer-create
                          (concat (leetcode--title-slug title) suffix))
      (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
      (insert snippet)
      (switch-to-buffer-other-window (current-buffer)))))

(define-derived-mode leetcode-problems-mode
  tabulated-list-mode "LeetCode Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-problems--refresh nil t)
  (use-local-map leetcode-problems-mode-map))

(define-derived-mode leetcode-problem-description-mode
  special-mode "LeetCode Problem Description"
  "Major mode for display problem description.")

(add-hook 'leetcode-problems-mode-hook 'hl-line-mode)

(provide 'leetcode)
;;; leetcode.el ends here
