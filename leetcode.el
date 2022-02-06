;;; leetcode.el --- An leetcode client           -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "26") (dash "2.16.0") (graphql "0.1.1") (spinner "1.7.3") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.24

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
(eval-when-compile
  (require 'let-alist))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)                      ; Some requests of LeetCode use GraphQL
(require 'aio)
(require 'spinner)
(require 'log4e)

(log4e:deflogger "leetcode" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-leetcode "*leetcode-log*")

;;;###autoload
(defun leetcode-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (leetcode--log-debugging-p)
      (progn
        (leetcode--log-set-level 'info)
        (leetcode--log-disable-debugging)
        (message "leetcode disable debug"))
    (progn
      (leetcode--log-set-level 'debug)
      (leetcode--log-enable-debugging)
      (message "leetcode enable debug"))))

(defun leetcode--install-my-cookie ()
  "Install leetcode dependencies."
  (let ((async-shell-command-display-buffer t))
    (async-shell-command
     "pip3 install my_cookies"
     (get-buffer-create "*leetcode-install*"))))

(defun leetcode--check-deps ()
  "Check if all dependencies installed."
  (if (executable-find "my_cookies")
      t
    (leetcode--install-my-cookie)
    nil))

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

(defvar leetcode--all-problems nil
  "Problems info with a list of problem object.
The object with following attributes:
:num      Number
:tag      String
:problems List

The elements of :problems has attributes:
:status     String
:id         Number
:backend-id Number
:title      String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}
:tags       List")

(defvar leetcode--all-tags nil
  "All problems tags.")

(defvar leetcode--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar leetcode-retry-threshold 20 "`leetcode-try' or `leetcode-submit' retry times.")
(defvar leetcode--filter-regex nil "Filter rows by regex.")
(defvar leetcode--filter-tag nil "Filter rows by tag.")
(defvar leetcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")
(defconst leetcode--all-difficulties '("easy" "medium" "hard"))

(defconst leetcode--paid "•" "Paid mark.")
(defconst leetcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name             "*leetcode*")
(defconst leetcode--description-buffer-name "*leetcode-description*")
(defconst leetcode--testcase-buffer-name    "*leetcode-testcase*")
(defconst leetcode--result-buffer-name      "*leetcode-result*")

(defface leetcode-paid-face
  '((t (:foreground "gold")))
  "Face for `leetcode--paid'."
  :group 'leetcode)

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode--checkmark'."
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
(defconst leetcode--api-all-tags            (concat leetcode--base-url "/problems/api/tags"))
(defconst leetcode--api-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))
;; submit
(defconst leetcode--api-submit              (concat leetcode--base-url "/problems/%s/submit/"))
(defconst leetcode--api-problems-submission (concat leetcode--base-url "/problems/%s/submissions/"))
(defconst leetcode--api-check-submission    (concat leetcode--base-url "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode--api-try                 (concat leetcode--base-url "/problems/%s/interpret_solution/"))

(defun to-list (vec)
  "Convert VEC to list."
  (append vec '()))

(defmacro dovec (spec &rest body)
  "Loop over a vector.
EVALUATE BODY with VAR bound to each element in VEC, in turn.
SPEC is just like (VAR VEC [RESULT]).  Then evaluate RESULT to
get the return value (nil if RESULT is omitted).

\(fn (VAR VEC [RESULT]) BODY...)"
  (declare (indent 1))
  (let ((start 0)
        (counter (gensym))
        (end (gensym)))
    `(let ((,counter ,start)
           (,(car spec) nil)
           (,end (length ,(cadr spec))))
       (while (< ,counter ,end)
         (setq ,(car spec) (aref ,(cadr spec) ,counter))
         ,@body
         (setq ,counter (1+ ,counter)))
       ,@(cddr spec))))

(defun leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1)
                               "csrftoken"))
                    (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(aio-defun leetcode--csrf-token ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (aio-await (aio-url-retrieve leetcode--url-login)))
  (leetcode--maybe-csrf-token))

(defun leetcode--credentials ()
  "Receive user account and password."
  (let ((auth-source-creation-prompts
         '((user . "LeetCode user: ")
           (secret . "LeetCode password for %u: ")))
        (found (car (auth-source-search
                     :max 1
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

(defun leetcode--multipart-form-data (name value)
  "Generate multipart form data with NAME and VALUE."
  `("file"
    ("name"         . ,name)
    ("filedata"     . ,value)
    ("filename"     . "")
    ("content-type" . "")))

(aio-defun leetcode--login ()
  "Steal LeetCode login session from local browser.
It also cleans LeetCode cookies in `url-cookie-file'."
  (leetcode--loading-mode t)
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (aio-await (leetcode--csrf-token))    ;knock knock, whisper me the mysterious information
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string my-cookies))
         (cookies-list (seq-filter
                        (lambda (s) (not (string-empty-p s)))
                        (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map
                         (lambda (s) (split-string s))
                         cookies-list))
         (leetcode-session (cadr (assoc "LEETCODE_SESSION" cookies-pairs)))
         (leetcode-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (leetcode--debug "login session: %s" leetcode-session)
    (leetcode--debug "login csrftoken: %s" leetcode-csrftoken)
    (url-cookie-store "LEETCODE_SESSION" leetcode-session nil leetcode--domain "/" t)
    (url-cookie-store "csrftoken" leetcode-csrftoken nil leetcode--domain "/" t))
  (leetcode--loading-mode -1))

(defun leetcode--login-p ()
  "Whether user is login."
  (let ((username (plist-get leetcode--user :username)))
    (and username
         (not (string-empty-p username))
         (seq-find
          (lambda (item)
            (string= (aref item 1)
                     "LEETCODE_SESSION"))
          (url-cookie-retrieve leetcode--domain "/" t)))))

(defun leetcode--set-user-and-problems (user-and-problems)
  "Set `leetcode--user' and `leetcode--all-problems'.
If user isn't login, only `leetcode--all-problems' will be set.
USER-AND-PROBLEMS is an alist comes from
`leetcode--api-all-problems'."
  ;; user
  (let-alist user-and-problems
    (setq leetcode--user
          (list :username .user_name
                :solved   .num_solved
                :easy     .ac_easy
                :medium   .ac_medium
                :hard     .ac_hard))
    (leetcode--debug "set user: %s, solved %s in %s problems" .user_name .num_solved .num_total)
    ;; problem list
    (setq leetcode--all-problems
          (list
           :num .num_total
           :tag "all"
           :problems
           (let* ((len .num_total)
                  (problems nil))
             (dotimes (i len)
               (let-alist (aref .stat_status_pairs i)
                 (leetcode--debug "frontend_question_id: %s, question_id: %s, title: %s"
                                  .stat.frontend_question_id .stat.question_id .stat.question__title)
                 (push (list
                        :status .status
                        :id .stat.frontend_question_id
                        :backend-id .stat.question_id
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

(defun leetcode--set-tags (all-tags)
  "Set `leetcode--all-tags' and `leetcode--all-problems' with ALL-TAGS."
  (let ((tags-table (make-hash-table :size 2000)))
    (let-alist all-tags
      (dolist (topic (to-list .topics))
        (let-alist topic
          ;; set leetcode--all-tags
          (unless (member .slug leetcode--all-tags)
            (push .slug leetcode--all-tags))
          ;; tags-table cache
          (dolist (id (to-list .questions))
            (puthash id (cons .slug (gethash id tags-table)) tags-table)))))
    ;; set problems tags with tags-table
    (dolist (problem (plist-get leetcode--all-problems :problems))
      (let ((backend-id (plist-get problem :backend-id)))
        (plist-put problem :tags (gethash backend-id tags-table))))))

(defun leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'."
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),]" "" str1)))
    res))

(defun leetcode--problem-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are LeetCode GraphQL parameters."
  (list
   (cons "operationName" operation)
   (cons "query"
         (graphql-query
          questionData
          (:arguments
           (($titleSlug . String!))
           (question
            :arguments
            ((titleSlug . ($ titleSlug)))
            likes
            dislikes
            content
            sampleTestCase
            (topicTags slug)
            (codeSnippets langSlug code)))))
   (if vars (cons "variables" vars))))

(aio-defun leetcode--fetch-problem (title)
  "Fetch single problem.
TITLE is a problem's title.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (let* ((slug-title (leetcode--slugify-title title))
         (url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode--User-Agent
            ,(cons "Content-Type" "application/json")))
         (url-request-data
          (json-encode (leetcode--problem-graphql-params
                        "questionData"
                        (list (cons "titleSlug" slug-title)))))
         (result (aio-await (aio-url-retrieve leetcode--api-graphql))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (switch-to-buffer (cdr result))
          (leetcode--warn "LeetCode fetch problem ERROR: %S" error-info))
      (with-current-buffer (cdr result)
        (goto-char url-http-end-of-headers)
        (alist-get 'question (alist-get 'data (json-read)))))))

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
under that column and the HEADER-NAMES.  HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (seq-reduce
          (lambda (acc row)
            (cl-mapcar
             (lambda (a col) (max a (length col)))
             acc
             (append row '())))
          rows
          (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun leetcode--stringify-difficulty (difficulty)
  "Stringify DIFFICULTY level (number) to 'easy', 'medium' or 'hard'."
  (let ((easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard"))
    (cond
     ((eq 1 difficulty)
      (prog1 easy-tag
        (put-text-property
         0 (length easy-tag)
         'font-lock-face 'leetcode-easy-face easy-tag)))
     ((eq 2 difficulty)
      (prog1 medium-tag
        (put-text-property
         0 (length medium-tag)
         'font-lock-face 'leetcode-medium-face medium-tag)))
     ((eq 3 difficulty)
      (prog1 hard-tag
        (put-text-property
         0 (length hard-tag)
         'font-lock-face 'leetcode-hard-face hard-tag))))))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--all-problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (plist-get leetcode--all-problems :problems))
        (easy-tag "easy")
        (medium-tag "medium")
        (hard-tag "hard")
        rows)
    (dolist (p problems)
      (if (or leetcode--display-paid
              (not (plist-get p :paid-only)))
          (setq rows
                (cons
                 (vector
                  ;; status
                  (if (equal (plist-get p :status) "ac")
                      (prog1 leetcode--checkmark
                        (put-text-property
                         0 (length leetcode--checkmark)
                         'font-lock-face 'leetcode-checkmark-face leetcode--checkmark))
                    " ")
                  ;; id
                  (number-to-string (plist-get p :id))
                  ;; title
                  (concat
                   (plist-get p :title)
                   " "
                   (if (eq (plist-get p :paid-only) t)
                       (prog1 leetcode--paid
                         (put-text-property
                          0 (length leetcode--paid)
                          'font-lock-face 'leetcode-paid-face leetcode--paid))
                     " "))
                  ;; acceptance
                  (plist-get p :acceptance)
                  ;; difficulty
                  (leetcode--stringify-difficulty (plist-get p :difficulty))
                  ;; tags
                  (if leetcode--display-tags (string-join (plist-get p :tags) ", ") ""))
                 rows))))
    (reverse rows)))

(defun leetcode--row-tags (row)
  "Get tags from ROW."
  (aref row 5))

(defun leetcode--row-difficulty (row)
  "Get difficulty from ROW."
  (aref row 4))

(defun leetcode--filter (rows)
  "Filter ROWS by `leetcode--filter-regex', `leetcode--filter-tag' and `leetcode--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and
      (if leetcode--filter-regex
          (let ((title (aref row 2)))
            (string-match-p leetcode--filter-regex title))
        t)
      (if leetcode--filter-tag
          (let ((tags (split-string (leetcode--row-tags row) ", ")))
            (member leetcode--filter-tag tags))
        t)
      (if leetcode--filter-difficulty
          (let ((difficulty (leetcode--row-difficulty row)))
            (string= difficulty leetcode--filter-difficulty))
        t)))
   rows))

(defun leetcode-reset-filter ()
  "Reset filter."
  (interactive)
  (setq leetcode--filter-regex nil)
  (setq leetcode--filter-tag nil)
  (setq leetcode--filter-difficulty nil)
  (leetcode-refresh))

(defun leetcode-set-filter-regex (regex)
  "Set `leetcode--filter-regex' as REGEX and refresh."
  (interactive "sSearch: ")
  (setq leetcode--filter-regex regex)
  (leetcode-refresh))

(defun leetcode-set-filter-tag ()
  "Set `leetcode--filter-tag' from `leetcode--all-tags' and refresh."
  (interactive)
  (setq leetcode--filter-tag
        (completing-read "Tags: " leetcode--all-tags))
  (leetcode-refresh))

(defun leetcode-set-prefer-language ()
  "Set `leetcode-prefer-language' from `leetcode--lang-suffixes' and refresh."
  (interactive)
  (setq leetcode-prefer-language
        (completing-read "Language: " leetcode--lang-suffixes))
  (leetcode-refresh))

(defun leetcode-set-filter-difficulty ()
  "Set `leetcode--filter-difficulty' from `leetcode--all-difficulties' and refresh."
  (interactive)
  (setq leetcode--filter-difficulty
        (completing-read "Difficulty: " leetcode--all-difficulties))
  (leetcode-refresh))

(defun leetcode-toggle-tag-display ()
  "Toggle `leetcode--display-tags` and refresh"
  (interactive)
  (setq leetcode--display-tags (not leetcode--display-tags))
  (leetcode-refresh))

(defun leetcode-toggle-paid-display ()
  "Toggle `leetcode--display-paid` and refresh"
  (interactive)
  (setq leetcode--display-paid (not leetcode--display-paid))
  (leetcode-refresh))

(aio-defun leetcode--fetch-all-tags ()
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,leetcode--User-Agent
            ,leetcode--X-Requested-With
            ,(leetcode--referer leetcode--url-login)))
         (result (aio-await (aio-url-retrieve leetcode--api-all-tags))))
    (with-current-buffer (cdr result)
      (goto-char url-http-end-of-headers)
      (json-read))))

(aio-defun leetcode--fetch-user-and-problems ()
  "Fetch user and problems info."
  (if leetcode--loading-mode
      (message "LeetCode has been refreshing...")
    (leetcode--loading-mode t)
    (let ((url-request-method "GET")
          (url-request-extra-headers
           `(,leetcode--User-Agent
             ,leetcode--X-Requested-With
             ,(leetcode--referer leetcode--url-login)))
          (result (aio-await (aio-url-retrieve leetcode--api-all-problems))))
      (leetcode--loading-mode -1)
      (if-let ((error-info (plist-get (car result) :error)))
          (progn
            (switch-to-buffer (cdr result))
            (leetcode--warn "LeetCode fetch user and problems failed: %S" error-info))
        (with-current-buffer (cdr result)
          (goto-char url-http-end-of-headers)
          (json-read))))))

(defun leetcode-refresh ()
  "Make `tabulated-list-entries'."
  (interactive)
  (let* ((header-names (append '(" " "#" "Problem" "Acceptance" "Difficulty")
                               (if leetcode--display-tags '("Tags"))))
         (rows (leetcode--filter (leetcode--problems-rows)))
         (headers (leetcode--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create leetcode--buffer-name)
      (leetcode--problems-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (cl-mapcar
             (lambda (i x) (list i x))
             (number-sequence 0 (1- (length rows)))
             rows))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (leetcode--loading-mode -1))))

(aio-defun leetcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (if-let ((users-and-problems (aio-await (leetcode--fetch-user-and-problems)))
           (all-tags (aio-await (leetcode--fetch-all-tags))))
      (progn
        (leetcode--set-user-and-problems users-and-problems)
        (leetcode--set-tags all-tags))
    (leetcode--warn "LeetCode parse user and problems failed"))
  (setq leetcode--display-tags leetcode-prefer-tag-display)
  (leetcode-reset-filter)
  (leetcode-refresh))

(aio-defun leetcode--async ()
  "Show leetcode problems buffer."
  (if (get-buffer leetcode--buffer-name)
      (switch-to-buffer leetcode--buffer-name)
    (unless (leetcode--login-p)
      (aio-await (leetcode--login)))
    (aio-await (leetcode-refresh-fetch))
    (switch-to-buffer leetcode--buffer-name)))

;;;###autoload
(defun leetcode ()
  "A wrapper for `leetcode--async', because emacs-aio can not be autoloaded.
see: https://github.com/skeeto/emacs-aio/issues/3."
  (interactive)
  (if (leetcode--check-deps)
      (leetcode--async)
    (message "installing leetcode dependencies...")))

;;;###autoload(autoload 'leetcode-daily "leetcode" nil t)
(aio-defun leetcode-daily ()
  "Open the daily challenge."
  (interactive)
  (unless (leetcode--login-p)
    (aio-await (leetcode)))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(,leetcode--User-Agent
            ("Content-Type" . "application/json")
            ,(leetcode--referer leetcode--url-login) 
            ,(cons leetcode--X-CSRFToken (leetcode--maybe-csrf-token))))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,leetcode--api-daily-challenge)))))
    (with-current-buffer (url-retrieve-synchronously leetcode--api-graphql)
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
          (leetcode-show-problem (string-to-number qid)))))))

(defun leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode--get-slug-title-before-try/submit (code-buf)
  "Get slug title before try or submit with CODE-BUF.
LeetCode require slug-title as the request parameters."
  (with-current-buffer code-buf
    (if leetcode-save-solutions
        (file-name-base (cadr (split-string (buffer-name) "_")))
      (file-name-base (buffer-name)))))

(aio-defun leetcode-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (leetcode--loading-mode t)
  (let* ((code-buf (current-buffer))
         (testcase-buf (get-buffer leetcode--testcase-buffer-name))
         (slug-title (leetcode--get-slug-title-before-try/submit code-buf))
         (problem (seq-find (lambda (p)
                              (equal slug-title
                                     (leetcode--slugify-title
                                      (plist-get p :title))))
                            (plist-get leetcode--all-problems :problems)))
         (problem-id (plist-get problem :backend-id)))
    (leetcode--debug "leetcode try slug-title: %s, problem-id: %s" slug-title problem-id)
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(,leetcode--User-Agent
              ("Content-Type" . "application/json")
              ,(leetcode--referer (format
                                   leetcode--api-problems-submission
                                   slug-title))
              ,(cons leetcode--X-CSRFToken (aio-await (leetcode--csrf-token)))))
           (url-request-data
            (json-encode
             `((data_input  . ,(leetcode--buffer-content testcase-buf))
               (judge_type  . "small")
               (lang        . ,leetcode--lang)
               (question_id . ,problem-id)
               (typed_code  . ,(leetcode--buffer-content code-buf)))))
           (result (aio-await (aio-url-retrieve (format leetcode--api-try slug-title)))))
      (if-let ((error-info (plist-get (car result) :error)))
          (progn
            (switch-to-buffer (cdr result))
            (leetcode--warn "LeetCode try failed: %S" error-info))
        (let ((data (with-current-buffer (cdr result)
                      (goto-char url-http-end-of-headers)
                      (json-read)))
              (res-buf (get-buffer leetcode--result-buffer-name)))
          (let-alist data
            (with-current-buffer res-buf
              (erase-buffer)
              (insert (concat "Your input:\n" .test_case "\n\n")))
            ;; poll interpreted
            (let ((actual_res (aio-await (leetcode--check-submission .interpret_id slug-title)))
                  (retry-times 0))
              (while (and (not actual_res) (< retry-times leetcode-retry-threshold))
                (aio-await (aio-sleep 0.5))
                (setq actual_res (aio-await (leetcode--check-submission .interpret_id slug-title)))
                (setq retry-times (1+ retry-times)))
              (if (< retry-times leetcode-retry-threshold)
                  (let-alist actual_res
                    (with-current-buffer res-buf
                      (goto-char (point-max))
                      (cond
                       ((eq .status_code 10)
                        (insert "Output:\n")
                        (dotimes (i (length .code_answer))
                          (insert (aref .code_answer i))
                          (insert "\n"))
                        (insert "\n")
                        (insert "Expected:\n")
                        (dotimes (i (length .expected_code_answer))
                          (insert (aref .expected_code_answer i))
                          (insert "\n"))
                        (insert "\n"))
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
                      (insert "\n\n")))
                (leetcode--warn "LeetCode try timeout.")))
            (leetcode--loading-mode -1)))))))

(aio-defun leetcode--check-submission (submission-id slug-title)
  "Polling to check submission detail.
After each submission, either try testcase or submit, LeetCode
returns a SUBMISSION-ID. With the SUBMISSION-ID, client will poll
for the submission detail. SLUG-TITLE is a slugified problem
title. Return response data if submission success, otherwise
nil."
  (leetcode--loading-mode t)
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(,leetcode--User-Agent
            ,(leetcode--referer (format leetcode--api-problems-submission slug-title))
            ,(cons leetcode--X-CSRFToken (aio-await (leetcode--csrf-token)))))
         (result (aio-await (aio-url-retrieve (format leetcode--api-check-submission submission-id)))))
    (if-let ((error-info (plist-get (car result) :error)))
        (progn
          (leetcode--loading-mode -1)
          (switch-to-buffer (cdr result))
          (leetcode--warn "LeetCode check submission failed: %S" error-info))
      (with-current-buffer (cdr result)
        (let ((submission-res
               (progn (goto-char url-http-end-of-headers)
                      (json-read))))
          (if (equal (alist-get 'state submission-res) "SUCCESS")
              submission-res))))))

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
BUFFER is the one to show LeetCode result.  ALIST is a combined
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
BUFFER is the one to show LeetCode testcase.  ALIST is a combined
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
BUFFER is the one to show LeetCode code.  ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--show-submission-result (submission-detail)
  "Show error info in `leetcode--result-buffer-name' based on status code.
Error info comes from SUBMISSION-DETAIL.  STATUS_CODE has
following possible value:
- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
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

(aio-defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (leetcode--loading-mode t)
  (let* ((code-buf (current-buffer))
         (code (leetcode--buffer-content code-buf))
         (slug-title (leetcode--get-slug-title-before-try/submit code-buf))
         (problem-id (plist-get (seq-find (lambda (p)
                                            (equal slug-title
                                                   (leetcode--slugify-title
                                                    (plist-get p :title))))
                                          (plist-get leetcode--all-problems :problems))
                                :backend-id)))
    (leetcode--debug "leetcode submit slug-title: %s, problem-id: %s" slug-title problem-id)
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(,leetcode--User-Agent
              ,(leetcode--referer (format
                                   leetcode--api-problems-submission
                                   slug-title))
              ,(cons "Content-Type" "application/json")
              ,(cons leetcode--X-CSRFToken (aio-await (leetcode--csrf-token)))))
           (url-request-data
            (json-encode `((lang . ,leetcode--lang)
                           (question_id . ,problem-id)
                           (typed_code . ,code))))
           (result (aio-await (aio-url-retrieve (format leetcode--api-submit slug-title)))))
      (if-let ((error-info (plist-get (car result) :error)))
          (progn
            (leetcode--loading-mode -1)
            (switch-to-buffer (cdr result))
            (leetcode--warn "LeetCode check submit failed: %S" error-info))
        (let* ((resp
                (with-current-buffer (cdr result)
                  (progn (goto-char url-http-end-of-headers)
                         (json-read))))
               (submission-id (alist-get 'submission_id resp))
               (submission-res (aio-await (leetcode--check-submission submission-id slug-title)))
               (retry-times 0))
          ;; poll submission result
          (while (and (not submission-res) (< retry-times leetcode-retry-threshold))
            (aio-await (aio-sleep 0.5))
            (setq submission-res (aio-await (leetcode--check-submission submission-id slug-title)))
            (setq retry-times (1+ retry-times)))
          (if (< retry-times leetcode-retry-threshold)
              (leetcode--show-submission-result submission-res)
            (leetcode--warn "LeetCode submit timeout."))
          (leetcode--loading-mode -1))))))

(defun leetcode--problem-link (title)
  "Generate problem link from TITLE."
  (concat leetcode--base-url "/problems/" (leetcode--slugify-title title)))

(defun leetcode--show-problem (problem problem-info)
  "Show the description of PROBLEM, whose meta data is PROBLEM-INFO.
Use `shr-render-buffer' to render problem description.  This action
will show the description in other window and jump to it."
  (let* ((problem-id (plist-get problem-info :id))
         (title (plist-get problem-info :title))
         (difficulty-level (plist-get problem-info :difficulty))
         (difficulty (leetcode--stringify-difficulty difficulty-level))
         (buf-name leetcode--description-buffer-name)
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (leetcode--debug "select title: %s" title)
    (let-alist problem
      (when (get-buffer buf-name)
        (kill-buffer buf-name))
      (with-temp-buffer
        (insert (concat "<h1>" (number-to-string problem-id) ". " title "</h1>"))
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
          (insert-text-button "Solve it"
                              'action (lambda (btn)
                                        (leetcode--start-coding problem problem-info))
                              'help-echo "solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Link"
                              'action (lambda (btn)
                                        (browse-url (leetcode--problem-link title)))
                              'help-echo "open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Solution"
                              'action (lambda (btn)
                                        (browse-url (concat (leetcode--problem-link title) "/solution")))
                              'help-echo "Open the problem solution page in browser.")
          ;; Would be best to parse the solution in Emacs, but the url-retrieve-synchronously only get the Javascript which generate the solution in HTML, not directly text
          )
        (rename-buffer buf-name)
        (leetcode--problem-description-mode)
        (switch-to-buffer (current-buffer))))))

(aio-defun leetcode-show-problem (problem-id)
  "Show the description of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
description.  This action will show the description in other
window and jump to it."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem-info (leetcode--get-problem-by-id problem-id))
         (title (plist-get problem-info :title))
         (problem (aio-await (leetcode--fetch-problem title))))
    (leetcode--show-problem problem problem-info)))

(defun leetcode-show-problem-by-slug (slug-title)
  "Show the description of problem with slug title. This function will work after first run M-x leetcode. This can be used with org-link elisp:(leetcode-show-problem-by-slug \"3sum\").
Get problem by id and use `shr-render-buffer' to render problem
description.  This action will show the description in other
window and jump to it."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (seq-find (lambda (p)
                              (equal slug-title
                                     (leetcode--slugify-title
                                      (plist-get p :title))))
                            (plist-get leetcode--all-problems :problems)))
         (problem-id (plist-get problem :id))
         (problem-info (leetcode--get-problem-by-id problem-id))
         (title (plist-get problem-info :title))
         (problem  (leetcode--fetch-problem title)))
    (leetcode-show-problem problem-id)))

(defun leetcode-show-current-problem ()
  "Show current problem's description.
Call `leetcode-show-problem' on the current problem id.  This
action will show the description in other window and jump to it."
  (interactive)
  (leetcode-show-problem (leetcode--get-current-problem-id)))

(aio-defun leetcode-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `leetcode-show-problem', but instead of jumping to the
description window, this action will jump back in `LC Problems'."
  (interactive (list (read-number "View problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (aio-await (leetcode-show-problem problem-id))
  (leetcode--jump-to-window-by-buffer-name leetcode--buffer-name))

(defun leetcode-view-current-problem ()
  "View current problem while staying in `LC Problems' window.
Similar with `leetcode-show-current-problem', but instead of jumping to
the description window, this action will jump back in `LC Problems'."
  (interactive)
  (leetcode-view-problem (leetcode--get-current-problem-id)))

(defun leetcode-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (read-number "Show in browser by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem-by-id problem-id))
         (title (plist-get problem :title))
         (link (leetcode--problem-link title)))
    (leetcode--debug "Open in browser: %s" link)
    (browse-url link)))

(defun leetcode-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `leetcode-show-problem-in-browser' on the current problem id."
  (interactive)
  (leetcode-show-problem-in-browser (leetcode--get-current-problem-id)))

(aio-defun leetcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-number "Solve the problem with id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem-info (leetcode--get-problem-by-id problem-id))
         (title (plist-get problem-info :title))
         (problem (aio-await (leetcode--fetch-problem title))))
    (leetcode--show-problem problem problem-info)
    (leetcode--start-coding problem problem-info)))

(defun leetcode-solve-current-problem ()
  "Start coding the current problem.
Call `leetcode-solve-problem' on the current problem id."
  (interactive)
  (leetcode-solve-problem (leetcode--get-current-problem-id)))

(defun leetcode--jump-to-window-by-buffer-name (buffer-name)
  "Jump to window by BUFFER-NAME."
  (select-window (get-buffer-window buffer-name)))

(defun leetcode--kill-buff-and-delete-window (buf)
  "Kill BUF and delete its window."
  (delete-windows-on buf t)
  (kill-buffer buf))

(defun leetcode-quit ()
  "Close and delete leetcode related buffers and windows."
  (interactive)
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--description-buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--result-buffer-name))
  (leetcode--kill-buff-and-delete-window (get-buffer leetcode--testcase-buffer-name))
  (mapc (lambda (title)
          (leetcode--kill-buff-and-delete-window
           (get-buffer (leetcode--get-code-buffer-name title))))
        leetcode--problem-titles))

(defcustom leetcode-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :type :boolean)

(defvar leetcode--display-tags leetcode-prefer-tag-display
  "(Internal) Whether tags are displayed the *leetcode* buffer.")

(defvar leetcode--display-paid nil
  "(Internal) Whether paid problems are displayed the *leetcode* buffer.")

(defvar leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift.")

(defvar leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql.")

(defvar leetcode-directory "~/leetcode"
  "Directory to save solutions.")

(defvar leetcode-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-directory'.")

(defvar leetcode--lang leetcode-prefer-language
  "LeetCode programming language or sql for current problem internally.
Default is programming language.")

(defconst leetcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("typescript" . ".ts") ("kotlin" . ".kt") ("php" . ".php")
    ("python" . ".py") ("python3" . ".py") ("ruby" . ".rb")
    ("rust" . ".rs") ("scala" . ".scala") ("swift" . ".swift")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "LeetCode programming language suffixes.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in SNIPPETS."
  (setq leetcode--lang
        (if (seq-find (lambda (s)
                        (equal (alist-get 'langSlug s)
                               leetcode-prefer-sql))
                      snippets)
            leetcode-prefer-sql
          leetcode-prefer-language)))

(defun leetcode--get-code-buffer-name (title)
  "Get code buffer name by TITLE and `leetcode-prefer-language'."
  (let* ((suffix (assoc-default
                  leetcode--lang
                  leetcode--lang-suffixes))
         (slug-title (leetcode--slugify-title title))
         (title-with-suffix (concat slug-title suffix)))
    (if leetcode-save-solutions
        (format "%04d_%s" (leetcode--get-problem-id slug-title) title-with-suffix)
      title-with-suffix)))

(defun leetcode--get-code-buffer (buf-name)
  "Get code buffer by BUF-NAME."
  (if (not leetcode-save-solutions)
      (get-buffer-create buf-name)
    (unless (file-directory-p leetcode-directory)
      (make-directory leetcode-directory))
    (find-file-noselect
     (concat (file-name-as-directory leetcode-directory)
             buf-name))))

(defun leetcode--get-problem (slug-title)
  "Get problem from `leetcode--all-problems' by SLUG-TITLE."
  (seq-find (lambda (p)
              (equal slug-title
                     (leetcode--slugify-title
                      (plist-get p :title))))
            (plist-get leetcode--all-problems :problems)))

(defun leetcode--get-problem-by-id (id)
  "Get problem from `leetcode--all-problems' by ID."
  (seq-find (lambda (p)
              (equal id (plist-get p :id)))
            (plist-get leetcode--all-problems :problems)))

(defun leetcode--get-problem-id (slug-title)
  "Get problem id by SLUG-TITLE."
  (let ((problem (leetcode--get-problem slug-title)))
    (plist-get problem :id)))

(defun leetcode--get-current-problem-id ()
  "Get id of the current problem."
  (string-to-number (aref (tabulated-list-get-entry) 1)))

(defun leetcode--start-coding (problem problem-info)
  "Create a buffer for coding PROBLEM with meta-data PROBLEM-INFO.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
  (let-alist problem
    (let* ((title (plist-get problem-info :title))
           (snippets (append .codeSnippets nil))
           (testcase .sampleTestCase))
      (add-to-list 'leetcode--problem-titles title)
      (leetcode--solving-layout)
      (leetcode--set-lang snippets)
      (let* ((slug-title  (leetcode--slugify-title title))
             (buf-name (leetcode--get-code-buffer-name title))
             (code-buf (get-buffer buf-name))
             (suffix (assoc-default
                      leetcode--lang
                      leetcode--lang-suffixes)))
        (unless code-buf
          (with-current-buffer (leetcode--get-code-buffer buf-name)
            (setq code-buf (current-buffer))
            (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
            (let* ((snippet (seq-find (lambda (s)
                                        (equal (alist-get 'langSlug s)
                                               leetcode--lang))
                                      snippets))
                   (template-code (alist-get 'code snippet)))
              (unless (save-mark-and-excursion
                        (goto-char (point-min))
                        (search-forward (string-trim template-code) nil t))
                (insert template-code))
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
                          (reusable-frames . visible))))
      )))

(defvar leetcode--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-show-current-problem)
      (define-key map (kbd "TAB") #'leetcode-view-current-problem)
      (define-key map "o" #'leetcode-show-current-problem)
      (define-key map "O" #'leetcode-show-problem)
      (define-key map "v" #'leetcode-view-current-problem)
      (define-key map "V" #'leetcode-view-problem)
      (define-key map "b" #'leetcode-show-current-problem-in-browser)
      (define-key map "B" #'leetcode-show-problem-in-browser)
      (define-key map "c" #'leetcode-solve-current-problem)
      (define-key map "C" #'leetcode-solve-problem)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "s" #'leetcode-set-filter-regex)
      (define-key map "l" #'leetcode-set-prefer-language)
      (define-key map "t" #'leetcode-set-filter-tag)
      (define-key map "T" #'leetcode-toggle-tag-display)
      (define-key map "P" #'leetcode-toggle-paid-display)      
      (define-key map "d" #'leetcode-set-filter-difficulty)
      (define-key map "g" #'leetcode-refresh)
      (define-key map "G" #'leetcode-refresh-fetch)
      (define-key map "/" #'leetcode-reset-filter)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problems-mode'.")

(define-derived-mode leetcode--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-refresh nil t)
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
  :require 'leetcode
  :lighter leetcode--loading-lighter
  :group 'leetcode
  (if leetcode--loading-mode
      (spinner-start leetcode--spinner)
    (spinner-stop leetcode--spinner)))

(provide 'leetcode)
;;; leetcode.el ends here
