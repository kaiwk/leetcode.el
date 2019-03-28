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

    :id         Number
    :title      String
    :difficulty Number {1,2,3}
    :status     String
    :paid-only  Boolean {t|nil}
")

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
                                    (paid-only (eq (alist-get 'paid_only cur) t)))
                               (push
                                (list
                                 :id (alist-get 'question_id stat)
                                 :title (alist-get 'question__title stat)
                                 :difficulty difficulty
                                 :status status
                                 :paid-only paid-only)
                                problems)))
                           problems)))))))

(defun leetcode--title-slug (title)
  (replace-regexp-in-string "\s+" "-" (downcase title)))

(defun leetcode--graphql-params (opration query &optional vars)
  (list
   (cons "operationName" "questionData")
   (cons "query" (graphql-query
                  questionData
                  (:arguments (($titleSlug . String!))
                              (question :arguments ((titleSlug . ($ titleSlug)))
                                        likes
                                        dislikes
                                        content
                                        (topicTags (name))))))
   (if vars (cons "variables" vars))))

(defun leetcode--parse-problem (title)
  "
:likes    Number
:dislikes Number
:content  String
"
  (let* ((csrftoken (leetcode--csrf-token))
         (title-slug (leetcode--title-slug title))
         (url-request-method "POST")
         (url-request-extra-headers (list leetcode--User-Agent
                                          (cons "Content-Type" "application/json")))
         (url-request-data (json-encode (leetcode--graphql-params
                                         "questionData"
                                         (graphql-query
                                          questionData
                                          (:arguments (($titleSlug . String!))
                                                      (question :arguments ((titleSlug . ($ titleSlug)))
                                                                likes
                                                                dislikes
                                                                content
                                                                (topicTags (name)))))
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

(defun leetcode-problem-description (title)
  "Clean leetcode problem description."
  (with-current-buffer (get-buffer-create leetcode--problem-buffer-name)
    (insert (alist-get 'content (leetcode--parse-problem title)))
    (leetcode--replace-in-buffer "" "")
    (leetcode--replace-in-buffer "<[^<]*>" "")
    (leetcode--replace-in-buffer "&copy;" "(c)")
    (leetcode--replace-in-buffer "&amp;" "&")
    (leetcode--replace-in-buffer "lt;" "<")
    (leetcode--replace-in-buffer "gt;" ">")
    (leetcode--replace-in-buffer "&nbsp;" " "))
  (switch-to-buffer leetcode--problem-buffer-name))

(provide 'leetcode)
;;; leetcode.el ends here
