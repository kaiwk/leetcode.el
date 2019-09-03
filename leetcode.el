;;; leetcode.el --- An leetcode client.          -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "25") (request-deferred "0.2.0") (graphql "0.1.1") (spinner "1.7.3"))
;; Version: 0.1.3

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

;; leetcode.el is an unofficial LeetCode client.
;;
;; Now it implements several API:
;; - Check problems list
;; - Try testcase
;; - Submit code
;;
;; Since most HTTP requests works asynchronously, it won't block Emacs.
;;
;;; Code:


(require 'json)
(require 'shr)

(require 'seq)
(require 'request)
(require 'request-deferred)             ; Asynchronous HTTP request
(require 'graphql)                      ; Some requests of LeetCode use GraphQL

(require 'spinner)


(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defvar leetcode--user nil
  "User object.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number")

(defvar leetcode--problems nil
  "Problems info with a list of problem object.
The object with following attributes:
:num      Number
:tag      String
:problems List
The elements of :problems has attributes:
:status     String
:id         Number
:pos        Number
:title      String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}")

(defvar leetcode--problem-titles nil
    "Problem titles that have been open in solving layout.")

(defvar leetcode-checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name             "*leetcode*")
(defconst leetcode--description-buffer-name "*leetcode-description*")
(defconst leetcode--testcase-buffer-name    "*leetcode-testcase*")
(defconst leetcode--result-buffer-name      "*leetcode-result*")

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode-checkmark'"
  :group 'leetcode)

(defface leetcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems."
  :group 'leetcode)

(defface leetcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems."
  :group 'leetcode)

(defface leetcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems."
  :group 'leetcode)

;;; Login
;; URL
(defconst leetcode--domain    "leetcode.com")
(defconst leetcode--base-url  "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--base-url "/accounts/login"))

;; Header
(defconst leetcode--User-Agent       '("User-Agent" .
                                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken      "X-CSRFToken")

;; API
(defconst leetcode--api-root                (concat leetcode--base-url "/api"))
(defconst leetcode--api-graphql             (concat leetcode--base-url "/graphql"))
(defconst leetcode--api-all-problems        (concat leetcode--api-root "/problems/all/"))
;; submit
(defconst leetcode--api-submit              (concat leetcode--base-url "/problems/%s/submit/"))
(defconst leetcode--api-problems-submission (concat leetcode--base-url "/problems/%s/submissions/"))
(defconst leetcode--api-check-submission    (concat leetcode--base-url "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode--api-try                 (concat leetcode--base-url "/problems/%s/interpret_solution/"))


(defun leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--csrf-token ()
  "Return csrf token."
  (let ((token (assoc-default
                "csrftoken"
                (request-cookie-alist leetcode--domain "/" t))))
    (or token
        (progn
          (request leetcode--url-login :sync t)
          (leetcode--csrf-token)))))

(defun leetcode--credentials ()
  (let ((auth-source-creation-prompts
         '((user . "LeetCode user: ")
           (secret . "LeetCode password for %u: ")))
        (found (car (auth-source-search :max 1
                                        :host leetcode--domain
                                        :require '(:user :secret)
                                        :create t))))
    (if found
        (list (plist-get found :user)
              (let ((secret (plist-get found :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret))
              (plist-get found :save-function)))))

(defun leetcode--login ()
  "Send login request and return a deferred object.
When ACCOUNT or PASSWORD is empty string it will show a prompt."
  (let* ((credentials (leetcode--credentials))
         (account (nth 0 credentials))
         (password (nth 1 credentials))
         (save-func (nth 2 credentials)))
    (leetcode--loading-mode t)
    (request-deferred
     leetcode--url-login
     :type "POST"
     :headers `(,leetcode--User-Agent
                ,leetcode--X-Requested-With
                ,(leetcode--referer leetcode--url-login)
                ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
     :parser 'buffer-string
     :files `(("csrfmiddlewaretoken" . ("" :data ,(leetcode--csrf-token)))
              ("login"               . ("" :data ,account))
              ("password"            . ("" :data ,password)))
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (leetcode--loading-mode -1)
        (when (functionp save-func)
          (funcall save-func))))
     :error
     (cl-function
      (lambda (&rest args &key error-thrown &allow-other-keys)
        (leetcode--loading-mode -1)
        (message "LeetCode Login ERROR: %S" error-thrown)
        (auth-source-forget+ :host leetcode--domain))))))

(defun leetcode--login-p ()
  "Whether user is login."
  (let ((username (plist-get leetcode--user :username)))
    (and username
         (not (string-empty-p username))
         (assoc-default
          "LEETCODE_SESSION"
          (request-cookie-alist
           (concat "." leetcode--domain) "/" t)))))

(defun leetcode--set-user-and-problems (res)
  "Set `leetcode--user' and `leetcode--problems'.
If user isn't login, only `leetcode--problems' will be set. RES
is an alist comes from `leetcode--api-all-problems'."
  ;; user
  (let-alist res
    (setq leetcode--user (list :username .user_name
                               :solved   .num_solved
                               :easy     .ac_easy
                               :medium   .ac_medium
                               :hard     .ac_hard))
    ;; problem list
    (setq leetcode--problems (list
                              :num .num_total
                              :tag "all"
                              :problems
                              (let ((len .num_total)
                                    problems)
                                (dolist (i (number-sequence 0 (1- len)))
                                  (let-alist (aref .stat_status_pairs i)
                                    (push
                                     (list
                                      :status .status
                                      :id .stat.question_id
                                      :pos (- len i)
                                      :title .stat.question__title
                                      :acceptance (format
                                                   "%.1f%%"
                                                   (* 100
                                                      (/ (float .stat.total_acs)
                                                         .stat.total_submitted)))
                                      :difficulty .difficulty.level
                                      :paid-only (eq .paid_only t))
                                     problems)))
                                problems)))))

(defun leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'."
  (let* ((str1 (replace-regexp-in-string "\s+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),]" "" str1)))
    res))

(defun leetcode--problem-descr-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are LeetCode GraphQL parameters."
  (list
   (cons "operationName" operation)
   (cons "query" (graphql-query
                  questionData
                  (:arguments (($titleSlug . String!))
                              (question :arguments ((titleSlug . ($ titleSlug)))
                                        likes
                                        dislikes
                                        content
                                        sampleTestCase
                                        (topicTags slug)
                                        (codeSnippets langSlug code)))))
   (if vars (cons "variables" vars))))

(defun leetcode--parse-problem (title)
  "Fetch single problem.
TITLE is a problem's title.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (let* ((slug-title (leetcode--slugify-title title))
         (resp (request
                leetcode--api-graphql
                :type "POST"
                :headers `(,leetcode--User-Agent
                           ,(cons "Content-Type" "application/json"))
                :data (json-encode (leetcode--problem-descr-graphql-params
                                    "questionData"
                                    (list (cons "titleSlug" slug-title))))
                :parser 'json-read :sync t)))
    (alist-get 'question (alist-get 'data (request-response-data resp)))))

(defun leetcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun leetcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (-reduce-from
          (lambda (acc row)
            (-zip-with
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          (seq-map #'length header-names)
          rows)))
    (vconcat
     (-zip-with
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (reverse (plist-get leetcode--problems :problems)))
        (easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard")
        rows)
    (dolist (p problems)
      (setq rows
            (cons
             (vector
              (if (equal (plist-get p :status) "ac")
                  (prog1 leetcode-checkmark
                    (put-text-property
                     0 (length leetcode-checkmark)
                     'font-lock-face 'leetcode-checkmark-face leetcode-checkmark))
                " ")
              (number-to-string (plist-get p :pos))
              (plist-get p :title)
              (plist-get p :acceptance)
              (cond
               ((eq 1 (plist-get p :difficulty))
                (prog1 easy-tag
                  (put-text-property
                   0 (length easy-tag)
                   'font-lock-face 'leetcode-easy-face easy-tag)))
               ((eq 2 (plist-get p :difficulty))
                (prog1 medium-tag
                  (put-text-property
                   0 (length medium-tag)
                   'font-lock-face 'leetcode-medium-face medium-tag)))
               ((eq 3 (plist-get p :difficulty))
                (prog1 hard-tag
                  (put-text-property
                   0 (length hard-tag)
                   'font-lock-face 'leetcode-hard-face hard-tag)))))
             rows)))
    rows))

(defun leetcode-problems-refresh ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (if leetcode--loading-mode
      (deferred:next
        (lambda ()
          (message "LeetCode has been refreshing...")))
    (leetcode--loading-mode t)
    (deferred:$
      (request-deferred
       leetcode--api-all-problems
       :headers `(,leetcode--User-Agent
                  ,leetcode--X-Requested-With
                  ,(leetcode--referer leetcode--url-login))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (response)
          (leetcode--set-user-and-problems (request-response-data response))))
      (deferred:nextc it
        (lambda ()
          (let* ((header-names '(" " "#" "Problem" "Acceptance" "Difficulty"))
                 (rows (leetcode--problems-rows))
                 (headers (leetcode--make-tabulated-headers header-names rows)))
            (with-current-buffer (get-buffer-create leetcode--buffer-name)
              (leetcode--problems-mode)
              (setq tabulated-list-format headers)
              (setq tabulated-list-entries
                    (-zip-with
                     (lambda (i x) (list i x))
                     (-iterate '1+ 0 (length rows))
                     rows))
              (tabulated-list-init-header)
              (tabulated-list-print t)
              (leetcode--loading-mode -1))))))))

;;;###autoload
(defun leetcode ()
  "Show leetcode problems buffer."
  (interactive)
  (if (get-buffer leetcode--buffer-name)
      (switch-to-buffer leetcode--buffer-name)
    (deferred:$
      (if (leetcode--login-p)
          (deferred:next
            (lambda ()
              (message "User have been login in.")))
        (leetcode--login))
      (deferred:nextc it
        (lambda ()
          (deferred:nextc (leetcode-problems-refresh)
            (lambda ()
              (switch-to-buffer leetcode--buffer-name))))))))

(defun leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (let* ((code-buf (current-buffer))
         (testcase-buf (get-buffer leetcode--testcase-buffer-name))
         (slug-title (with-current-buffer code-buf
                       (file-name-base (buffer-name))))
         (id (plist-get (seq-find (lambda (p)
                                    (equal slug-title
                                           (leetcode--slugify-title
                                            (plist-get p :title))))
                                  (plist-get leetcode--problems :problems))
                        :id)))
    (leetcode--loading-mode t)
    (deferred:$
      (if testcase-buf
          (request-deferred
           (format leetcode--api-try (leetcode--slugify-title slug-title))
           :headers `(,leetcode--User-Agent
                      ("Content-Type" . "application/json")
                      ,(leetcode--referer (format
                                           leetcode--api-problems-submission
                                           slug-title))
                      ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
           :data (json-encode
                  `((data_input . ,(leetcode--buffer-content testcase-buf))
                    (judge_type . "small")
                    (lang . ,leetcode--lang)
                    (question_id . ,id)
                    (typed_code . ,(leetcode--buffer-content code-buf))))
           :parser 'json-read
           :error
           (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "LeetCode Try ERROR: %S" error-thrown))))
        (throw 'no-buffer "No testcase buffer and code buffer."))
      (deferred:nextc it
        (lambda (resp)
          (let ((res-buf (get-buffer leetcode--result-buffer-name)))
            (let-alist (request-response-data resp)
              (with-current-buffer res-buf
                (erase-buffer)
                (insert (concat "Your input:\n" .test_case "\n\n")))
              (leetcode--check-submission
               .interpret_expected_id slug-title
               (lambda (res)
                 (let ((answer (aref (alist-get 'code_answer res) 0)))
                   (with-current-buffer res-buf
                     (insert (concat "Expected:\n" answer "\n\n"))))))
              (leetcode--check-submission
               .interpret_id slug-title
               (lambda (res)
                 (let-alist res
                   (with-current-buffer res-buf
                     (insert "Output:\n")
                     (cond
                      ((eq .status_code 10)
                       (insert (aref .code_answer 0)))
                      ((eq .status_code 14)
                       (insert .status_msg))
                      ((eq .status_code 15)
                       (insert .status_msg)
                       (insert "\n\n")
                       (insert .full_runtime_error))
                      ((eq .status_code 20)
                       (insert .status_msg)
                       (insert "\n\n")
                       (insert .full_compile_error)))
                     (when (> (length .code_output) 0)
                       (insert "\n\n")
                       (insert "Code output:\n")
                       (dolist (item (append .code_output nil))
                         (insert (concat item "\n"))))
                     (insert "\n\n")
                     (leetcode--loading-mode -1))))))))))))

(defun leetcode--check-submission (submission-id slug-title cb)
  "Polling to check submission detail.
After each submission, either try testcase or submit, LeetCode
returns a SUBMISSION-ID. With the SUBMISSION-ID, client will poll
for the submission detail. SLUG-TITLE is a slugified problem
title. CB is a callback function which will be invoked after
request success."
  (request
   (format leetcode--api-check-submission submission-id)
   :type "POST"
   :headers `(,leetcode--User-Agent
              ,(leetcode--referer (format leetcode--api-problems-submission slug-title))
              ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
   :parser 'json-read
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (if (equal (alist-get 'state data) "SUCCESS")
          (funcall cb data)
        (leetcode--check-submission submission-id slug-title cb))))
   :error
   (cl-function
    (lambda (&rest args &key error-thrown &allow-other-keys)
      (leetcode--loading-mode -1)
      (message "LeetCode Login ERROR: %S" error-thrown)))))

(defun leetcode--solving-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |  Description   |
|               |                |
|               +----------------+
|     Code      |   Customize    |
|               |   Testcases    |
|               +----------------+
|               |Submit/Testcases|
|               |    Result      |
+---------------+----------------+"
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (split-window-below)
  (other-window 1)
  (split-window-below)
  (other-window -1)
  (other-window -1))

(defun leetcode--display-result (buffer &optional alist)
  "Display function for LeetCode result.
BUFFER is the one to show LeetCode result. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-testcase (buffer &optional alist)
  "Display function for LeetCode testcase.
BUFFER is the one to show LeetCode testcase. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-code (buffer &optional alist)
  "Display function for LeetCode code.
BUFFER is the one to show LeetCode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--show-submission-result (submission-detail)
  "Show error info in `leetcode--result-buffer-name' based on status code.
Error info comes from SUBMISSION-DETAIL. STATUS_CODE has
following possible value:
- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error. full_runtime_error
- 20: Compile Error. full_compile_error"
  (let-alist submission-detail
    (with-current-buffer (get-buffer-create leetcode--result-buffer-name)
      (erase-buffer)
      (insert (format "Status: %s" .status_msg))
      (cond
       ((eq .status_code 10)
        (insert (format " (%s/%s)\n\n" .total_correct .total_testcases))
        (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                        .status_runtime .runtime_percentile .pretty_lang))
        (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                        .status_memory .memory_percentile .pretty_lang)))
       ((eq .status_code 11)
        (insert (format " (%s/%s)\n\n" .total_correct .total_testcases))
        (insert (format "Test Case: \n%s\n\n" .input))
        (insert (format "Answer: %s\n\n" .code_output))
        (insert (format "Expected Answer: %s\n\n" .expected_output))
        (insert (format "Stdout: \n%s\n" .std_output)))
       ((eq .status_code 14)
        (insert "\n"))
       ((eq .status_code 15)
        (insert "\n\n")
        (insert (format (alist-get 'full_runtime_error submission-detail))))
       ((eq .status_code 20)
        (insert "\n\n")
        (insert (format (alist-get 'full_compile_error submission-detail)))))
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         leetcode--display-result)
                        (reusable-frames . visible))))))

(defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (let* ((code-buf (current-buffer))
         (code (leetcode--buffer-content code-buf))
         (slug-title (with-current-buffer code-buf
                       (file-name-base (buffer-name))))
         (id (plist-get (seq-find (lambda (p)
                                    (equal slug-title
                                           (leetcode--slugify-title
                                            (plist-get p :title))))
                                  (plist-get leetcode--problems :problems))
                        :id)))
    (leetcode--loading-mode t)
    (deferred:$
      (request-deferred
       (format leetcode--api-submit slug-title)
       :type "POST"
       :headers `(,leetcode--User-Agent
                  ,(leetcode--referer (format
                                       leetcode--api-problems-submission
                                       slug-title))
                  ,(cons "Content-Type" "application/json")
                  ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
       :data (json-encode `((lang . ,leetcode--lang)
                            (question_id . ,id)
                            (typed_code . ,code)))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (resp)
          (let ((submission-id (alist-get 'submission_id (request-response-data resp))))
            (leetcode--check-submission
             submission-id slug-title
             (lambda (res)
               (leetcode--show-submission-result res)
               (leetcode--loading-mode -1)))))))))

(defun leetcode--problem-link (title)
  "Generate problem link from title."
  (concat leetcode--base-url "/problems/" (leetcode--slugify-title title)))

(defun leetcode-show-description ()
  "Show current entry problem description.
Get current entry by using `tabulated-list-get-entry' and use
`shr-render-buffer' to render problem description."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (pos (aref entry 1))
         (title (aref entry 2))
         (difficulty (aref entry 4))
         (problem (leetcode--parse-problem title))
         (buf-name leetcode--description-buffer-name)
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (let-alist problem
      (when (get-buffer buf-name)
        (kill-buffer buf-name))
      (with-temp-buffer
        (insert (concat "<h1>" pos ". " title "</h1>"))
        (insert (concat (capitalize difficulty) html-margin
                        "likes: " (number-to-string .likes) html-margin
                        "dislikes: " (number-to-string .dislikes)))
        (insert .content)
        (setq shr-current-font t)
        (leetcode--replace-in-buffer "" "")
        ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
        (leetcode--replace-in-buffer "https" "http")
        (shr-render-buffer (current-buffer)))
      (with-current-buffer "*html*"
        (save-match-data
          (re-search-forward "dislikes: .*" nil t)
          (insert (make-string 4 ?\s))
          (insert-text-button "solve it"
                              'action (lambda (btn)
                                        (leetcode--start-coding
                                         title
                                         (append .codeSnippets nil)
                                         .sampleTestCase))
                              'help-echo "solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button "link"
                              'action (lambda (btn)
                                        (browse-url (leetcode--problem-link title)))
                              'help-echo "open the problem in browser."))
        (rename-buffer buf-name)
        (leetcode--problem-description-mode)
        (switch-to-buffer (current-buffer))))))

 (defun leetcode--kill-buff-and-delete-window (buf)
    "Kill buff and delete its window"
    (delete-windows-on buf t)
    (kill-buffer buf))

 (defun leetcode-quit ()
    "Close and delete leetcode related buffers and windows"
    (interactive)
    (leetcode--kill-buff-and-delete-window (get-buffer leetcode--buffer-name))
    (leetcode--kill-buff-and-delete-window (get-buffer leetcode--description-buffer-name))
    (leetcode--kill-buff-and-delete-window (get-buffer leetcode--result-buffer-name))
    (leetcode--kill-buff-and-delete-window (get-buffer leetcode--testcase-buffer-name))
    (mapc (lambda (x) (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--get-code-buffer-name x))))
          leetcode--problem-titles)
    )

(defvar leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, kotlin, php, python,
python3, ruby, rust, scala, swift.")

(defvar leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql.")

(defvar leetcode--lang leetcode-prefer-language
  "LeetCode programming language or sql for current problem
  internally. Default is programming language.")

(defconst leetcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py")
    ("python3" . ".py") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "LeetCode programming language suffixes.
c, cpp, csharp, golang, java, javascript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in snippets."
  (setq leetcode--lang
        (if (seq-find (lambda (s)
                        (equal (alist-get 'langSlug s)
                               leetcode-prefer-sql))
                      snippets)
            leetcode-prefer-sql
          leetcode-prefer-language)))

(defun leetcode--get-code-buffer-name (title)
  "Get code buffer name by TITLE and `leetcode-prefer-language'."
  (let ((suffix (assoc-default
                 leetcode--lang
                 leetcode--lang-suffixes)))
    (concat (leetcode--slugify-title title) suffix)))

(defun leetcode--start-coding (title snippets testcase)
  "Create a buffer for coding.
The buffer will be not associated with any file. It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'.
TITLE is a problem title. SNIPPETS is a list of alist used to
store eachprogramming language's snippet. TESTCASE is provided
for current problem."
  (add-to-list 'leetcode--problem-titles title)
  (leetcode--solving-layout)
  (leetcode--set-lang snippets)
  (let ((code-buf (get-buffer (leetcode--get-code-buffer-name title)))
        (suffix (assoc-default
                 leetcode--lang
                 leetcode--lang-suffixes)))
    (unless code-buf
      (with-current-buffer (get-buffer-create (leetcode--get-code-buffer-name title))
        (setq code-buf (current-buffer))
        (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
        (let ((snippet (seq-find (lambda (s)
                                   (equal (alist-get 'langSlug s)
                                          leetcode--lang))
                                 snippets)))
          (insert (alist-get 'code snippet))
          (leetcode--replace-in-buffer "" ""))))
    (display-buffer code-buf
                    '((display-buffer-reuse-window
                       leetcode--display-code)
                      (reusable-frames . visible))))
  (with-current-buffer (get-buffer-create leetcode--testcase-buffer-name)
    (erase-buffer)
    (insert testcase)
    (display-buffer (current-buffer)
                    '((display-buffer-reuse-window
                       leetcode--display-testcase)
                      (reusable-frames . visible))))
  (with-current-buffer (get-buffer-create leetcode--result-buffer-name)
    (erase-buffer)
    (display-buffer (current-buffer)
                    '((display-buffer-reuse-window
                       leetcode--display-result)
                      (reusable-frames . visible)))))

(defvar leetcode--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-show-description)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "g" #'leetcode-problems-refresh)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problems-mode'.")

(define-derived-mode leetcode--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-problems-refresh nil t)
  :group 'leetcode)

(add-hook 'leetcode--problems-mode-hook #'hl-line-mode)

(defvar leetcode--problem-description-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)))
  "Keymap for `leetcode--problem-description-mode'.")

(define-derived-mode leetcode--problem-description-mode
  special-mode "LC Descri"
  "Major mode for display problem description."
  :group 'leetcode)

;;; Use spinner.el to show progress indicator
(defvar leetcode--spinner (spinner-create 'progress-bar-filled)
  "Progress indicator to show request progress.")
(defconst leetcode--loading-lighter
  '(" [LeetCode" (:eval (spinner-print leetcode--spinner)) "]"))

(define-minor-mode leetcode--loading-mode
  "Minor mode to showing leetcode loading status."
  :lighter leetcode--loading-lighter
  :group 'leetcode
  :global t
  (if leetcode--loading-mode
      (spinner-start leetcode--spinner)
    (spinner-stop leetcode--spinner)))

(provide 'leetcode)
;;; leetcode.el ends here
