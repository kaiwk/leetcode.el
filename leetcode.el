;;; leetcode.el --- An leetcode client  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  Wang Kai
;;
;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "29.1") (dash "2.16.0") (graphql "0.1.1") (spinner "1.7.3") (log4e "0.3.3"))
;; Version: 0.1.27
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
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
  (require 'let-alist)
  (require 'dash))

(require 'json)
(require 'shr)
(require 'seq)
(require 'subr-x)
(require 'mm-url)
(require 'cl-lib)

(require 'dash)
(require 'graphql)                      ; Some requests of LeetCode use GraphQL
(require 'spinner)
(require 'log4e)

(defvar url-http-end-of-headers)

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
  (if (executable-find "my_cookies") t
    (leetcode--install-my-cookie)
    nil))

(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defcustom leetcode-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :type 'boolean)

(defcustom leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :type 'string)

(defcustom leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql."
  :type 'string)

(defcustom leetcode-directory "~/leetcode"
  "Directory to save solutions."
  :type 'string)

(defcustom leetcode-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-directory'."
  :type 'boolean)

(defcustom leetcode-focus t
  "When execute `leetcode', always delete other windows."
  :type 'boolean)

(cl-defstruct leetcode-user
  "A LeetCode User.
The object with following attributes:
:username String
:solved   Number
:easy     Number
:medium   Number
:hard     Number"
  username solved easy medium hard)

(cl-defstruct leetcode-problem
  "A single LeetCode problem.
:status     String
:id         Number
:backend-id Number
:title      String
:acceptance String
:difficulty Number {1,2,3}
:paid-only  Boolean {t|nil}
:tags       List"
  status id backend-id title acceptance
  difficulty paid-only tags)

(cl-defstruct leetcode-problems
  "All LeetCode problems, the problems can filtered by tag.
:num      Number
:tag      String
:problems List[leetcode--problems]"
  num tag problems)

(defvar leetcode--user (make-leetcode-user)
  "A User object.")

(defvar leetcode--problems (make-leetcode-problems)
  "Problems object with a list of `leetcode-problem'.")

(defvar leetcode--all-tags nil
  "All problems tags.")

(defvar leetcode--problem-titles nil
  "Problem titles that have been open in solving layout.")

(defvar leetcode--display-tags leetcode-prefer-tag-display
  "(Internal) Whether tags are displayed the *leetcode* buffer.")

(defvar leetcode--display-paid nil
  "(Internal) Whether paid problems are displayed the *leetcode* buffer.")

(defvar leetcode--lang leetcode-prefer-language
  "LeetCode programming language or sql for current problem internally.
Default is programming language.")

(defvar leetcode--description-window nil
  "(Internal) Holds the reference to description window.")

(defvar leetcode--testcase-window nil
  "(Internal) Holds the reference to testcase window.")

(defvar leetcode--result-window nil
  "(Internal) Holds the reference to result window.")

(defconst leetcode--lang-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("dart" . ".dart") ("elixir" . ".ex") ("erlang" . ".erl")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py") ("python3" . ".py")
    ("racket" . ".rkt") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift") ("typescript" . ".ts")
    ("mysql" . ".sql") ("mssql" . ".sql") ("oraclesql" . ".sql"))
  "LeetCode programming language suffixes.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defvar leetcode--filter-regex nil "Filter rows by regex.")
(defvar leetcode--filter-tag nil "Filter rows by tag.")
(defvar leetcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst leetcode--all-difficulties '("easy" "medium" "hard"))
(defconst leetcode--paid "•" "Paid mark.")
(defconst leetcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name             "*leetcode*")

(defconst leetcode--retry-times 10 "`leetcode-try' or `leetcode-submit' retry times.")

(defface leetcode-paid-face
  '((t (:foreground "gold")))
  "Face for `leetcode--paid'.")

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode--checkmark'.")

(defface leetcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problems.")

(defface leetcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problems.")

(defface leetcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problems.")

(defface leetcode-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted.")

(defface leetcode-error-face
  '((t (:inherit font-lock-warning-face)))
  "Face for submission compile error, runtime error and TLE.")

;;; Login
;; URL
(defconst leetcode--domain    "leetcode.com")
(defconst leetcode--url-base  "https://leetcode.com")
(defconst leetcode--url-login (concat leetcode--url-base "/accounts/login"))

;; Cookie key name
(defconst leetcode--cookie-csrftoken "csrftoken")
(defconst leetcode--cookie-session "LEETCODE_SESSION")

;; Header
(defconst leetcode--User-Agent       '("User-Agent" .
                                       "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:66.0) Gecko/20100101 Firefox/66.0"))
(defconst leetcode--X-Requested-With '("X-Requested-With" . "XMLHttpRequest"))
(defconst leetcode--X-CSRFToken      "X-CSRFToken")
(defconst leetcode--Content-Type     '("Content-Type" . "application/json"))

;; API URL
(defconst leetcode--url-api                 (concat leetcode--url-base "/api"))
(defconst leetcode--url-graphql             (concat leetcode--url-base "/graphql"))
(defconst leetcode--url-all-problems        (concat leetcode--url-api "/problems/all/"))
(defconst leetcode--url-all-tags            (concat leetcode--url-base "/problems/api/tags"))
(defconst leetcode--url-daily-challenge
  (concat
   "query questionOfToday { activeDailyCodingChallengeQuestion {"
   " link question { status title titleSlug qid: questionFrontendId } } }"))
;; submit
(defconst leetcode--url-submit              (concat leetcode--url-base "/problems/%s/submit/"))
(defconst leetcode--url-problems-submission (concat leetcode--url-base "/problems/%s/submissions/"))
(defconst leetcode--url-check-submission    (concat leetcode--url-base "/submissions/detail/%s/check/"))
;; try testcase
(defconst leetcode--url-try                 (concat leetcode--url-base "/problems/%s/interpret_solution/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsubst leetcode--to-list (vec)
  "Convert VEC to list."
  (append vec '()))

(defsubst leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--maybe-csrf-token ()
  "Return csrf token if it exists, otherwise return nil."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1)
                               leetcode--cookie-csrftoken))
                    (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(defun leetcode--csrf-token ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (url-retrieve-synchronously leetcode--url-login))
  (leetcode--maybe-csrf-token))

(defun leetcode--login-p ()
  "Whether user is login."
  (let ((username (leetcode-user-username leetcode--user)))
    (and username
         (not (string-empty-p username))
         (seq-find
          (lambda (item)
            (string= (aref item 1)
                     leetcode--cookie-session))
          (url-cookie-retrieve leetcode--domain "/" t)))))

(defsubst leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as \"Two Sum\" will be converted to \"two-sum\". \"Pow(x, n)\" will be
\"powx-n\"."
  (->> (downcase title)
       (replace-regexp-in-string "[\s-]+" "-")
       (replace-regexp-in-string "[(),'%]" "")))

(defun leetcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defsubst leetcode--problem-link (title)
  "Generate problem link from TITLE."
  (concat leetcode--url-base "/problems/" (leetcode--slugify-title title)))

(defun leetcode--stringify-difficulty (difficulty)
  "Stringify DIFFICULTY level (number) to \"easy\", \"medium\" or \"hard\"."
  (pcase-exhaustive difficulty
    (1 (leetcode--add-font-lock "easy" 'leetcode-easy-face))
    (2 (leetcode--add-font-lock "medium" 'leetcode-medium-face))
    (3 (leetcode--add-font-lock "hard" 'leetcode-hard-face))))


(defsubst leetcode--add-font-lock (str face)
  "Add font-locking FACE to STR."
  (propertize str 'font-lock-face face))

(defsubst leetcode--detail-buffer-name (problem-id)
  "Detail buffer name for PROBLEM-ID."
  (format "*leetcode-detail-%s*" problem-id))

(defsubst leetcode--testcase-buffer-name (problem-id)
  "Testcase buffer name for PROBLEM-ID."
  (format "*leetcode-testcase-%s*" problem-id))

(defsubst leetcode--result-buffer-name (problem-id)
  "Result buffer name for PROBLEM-ID."
  (format "*leetcode-result-%s*" problem-id))

(defsubst leetcode--maybe-focus ()
  "Delete other windows if `leetcode-focus'."
  (and leetcode-focus (delete-other-windows)))


;; -------------------------------------------------------------------
;;; API

(defun leetcode--json-read (buffer)
  "Read JSON from response BUFFER."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (condition-case err
        (prog1 (json-read)
          (kill-buffer (current-buffer)))
      (error (leetcode--debug "json-read: %s" (error-message-string err))
             (kill-buffer (current-buffer))
             nil))))

(defmacro leetcode-with-results (prefix status &rest body)
  "Anaphoric macro that binds `it' to request results.
If STATUS is an error, the error message is prefixed with PREFIX."
  (declare (indent 2))
  (macroexp-let2 nil prefix prefix
    `(if-let ((err (plist-get ,status :error)))
         (leetcode--debug (format (concat ,prefix " error: %S") err))
       (let ((it (leetcode--json-read (current-buffer))))
         ,@body))))

(cl-defun leetcode--url-retrieve
    (url &key (referrer leetcode--url-login) (method "GET") data callback cbargs)
  "Retrieve URL synchronously or asynchronously when CALLBACK is non-nil.
METHOD and REFERRER are used to set url headers.
DATA is Json encoded and used as request data.
CALLBACK and CBARGS are passed to `url-retrieve'."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (delq nil (list leetcode--User-Agent
                          leetcode--X-Requested-With
                          (cons leetcode--X-CSRFToken (leetcode--csrf-token))
                          (and referrer (leetcode--referer referrer))
                          (and (equal method "POST") leetcode--Content-Type))))
         (url-request-data (and data (json-encode data))))
    (leetcode--debug "url-retrieve[%s]: %s %S %S"
                     url-request-method url url-request-extra-headers
                     url-request-data)
    (if (null callback)
        (url-retrieve-synchronously url)
      (leetcode--loading-mode t)
      (funcall #'url-retrieve url callback cbargs))))
(put 'leetcode--url-retrieve 'lisp-indent-function 1)


(defun leetcode-login ()
  "Steal LeetCode login session from local browser.
It also cleans LeetCode cookies in `url-cookie-file'."
  (interactive)
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (leetcode--csrf-token)    ; knock knock, whisper me the mysterious information
  (let* ((my-cookies (executable-find "my_cookies"))
         (my-cookies-output (shell-command-to-string my-cookies))
         (cookies-list (seq-filter
                        (lambda (s) (not (string-empty-p s)))
                        (split-string my-cookies-output "\n")))
         (cookies-pairs (seq-map
                         (lambda (s) (split-string s))
                         cookies-list))
         (leetcode-session (cadr (assoc leetcode--cookie-session cookies-pairs)))
         (leetcode-csrftoken (cadr (assoc "csrftoken" cookies-pairs))))
    (leetcode--debug "login session: %s" leetcode-session)
    (leetcode--debug "login csrftoken: %s" leetcode-csrftoken)
    (url-cookie-store leetcode--cookie-session leetcode-session nil leetcode--domain "/" t)
    (url-cookie-store "csrftoken" leetcode-csrftoken nil leetcode--domain "/" t)))


(defun leetcode--api-fetch-all-tags ()
  "Fetch all problems' tags."
  (-> (leetcode--url-retrieve leetcode--url-all-tags)
      (leetcode--json-read)))

(defun leetcode--api-fetch-user-and-problems ()
  "Fetch user and problems info."
  (-> (leetcode--url-retrieve leetcode--url-all-problems)
      (leetcode--json-read)))

(defun leetcode--problem-graphql-params (operation &optional vars)
  "Construct a GraphQL parameter.
OPERATION and VARS are LeetCode GraphQL parameters."
  (list
   (cons "operationName" operation)
   (cons "query"
         (graphql-query questionData
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

(defun leetcode--api-fetch-problem (title)
  "Fetch single problem.
TITLE is a problem's title.
Return a object with following attributes:
:likes     Number
:dislikes  Number
:content   String
:topicTags String"
  (let ((slug-title (leetcode--slugify-title title)))
    (->> (leetcode--json-read
          (leetcode--url-retrieve leetcode--url-graphql
            :method "POST"
            :data (leetcode--problem-graphql-params
                   "questionData" (list (cons "titleSlug" slug-title)))))
         (alist-get 'data)
         (alist-get 'question))))


(defun leetcode--set-user-and-problems (user-and-problems)
  "Set `leetcode--user' and `leetcode--problems'.
If user isn't logged in, only sets `leetcode--problems'.
USER-AND-PROBLEMS is an alist returned by `leetcode--url-all-problems'."
  ;; User
  (let-alist user-and-problems
    (setf (leetcode-user-username leetcode--user) .user_name
          (leetcode-user-solved leetcode--user) .num_solved
          (leetcode-user-easy leetcode--user) .ac_easy
          (leetcode-user-medium leetcode--user) .ac_medium
          (leetcode-user-hard leetcode--user) .ac_hard)
    (leetcode--debug "set user: %s, solved %s in %s problems"
                     .user_name .num_solved .num_total)
    ;; Problem list
    (setf (leetcode-problems-num leetcode--problems) .num_total
          (leetcode-problems-tag leetcode--problems) "all")
    (let (problems)
      (cl-loop for el across .stat_status_pairs
               do (let-alist el
                    (leetcode--debug
                     "frontend_question_id: %s, question_id: %s, title: %s"
                     .stat.frontend_question_id .stat.question_id
                     .stat.question__title)
                    (push
                     (make-leetcode-problem
                      :status .status
                      :id .stat.frontend_question_id
                      :backend-id .stat.question_id
                      :title .stat.question__title
                      :acceptance (format "%.1f%%"
                                          (* 100
                                             (/ (float .stat.total_acs)
                                                .stat.total_submitted)))
                      :difficulty .difficulty.level
                      :paid-only (eq .paid_only t))
                     problems)))
      (setf (leetcode-problems-problems leetcode--problems) problems))))

(defun leetcode--set-tags (all-tags)
  "Set `leetcode--all-tags' and `leetcode--problems' with ALL-TAGS."
  (let ((tags-table (make-hash-table :size 2000)))
    (let-alist all-tags
      (dolist (topic (leetcode--to-list .topics))
        (let-alist topic
          ;; Set leetcode--all-tags
          (unless (member .slug leetcode--all-tags)
            (push .slug leetcode--all-tags))
          ;; tags-table cache with backend-id
          (dolist (id (leetcode--to-list .questions))
            (let ((tags (gethash id tags-table)))
              (setf (gethash id tags-table) (cons .slug tags)))))))
    ;; Set problems tags with tags-table
    (dolist (problem (leetcode-problems-problems leetcode--problems))
      (let ((backend-id (leetcode-problem-backend-id problem)))
        (setf (leetcode-problem-tags problem) (gethash backend-id tags-table))))))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (leetcode-problems-problems leetcode--problems))
        rows)
    (dolist (problem problems (reverse rows))
      (pcase-let (((cl-struct leetcode-problem
                              paid-only id status title acceptance tags difficulty)
                   problem))
        (when (or leetcode--display-paid (not paid-only))
          (push (vector
                 (if (equal status "ac")
                     (leetcode--add-font-lock leetcode--checkmark
                                              'leetcode-checkmark-face)
                   " ")
                 (number-to-string id)
                 (concat title " " (if (eq paid-only t)
                                       (leetcode--add-font-lock
                                        leetcode--paid 'leetcode-paid-face)
                                     " "))
                 acceptance
                 (leetcode--stringify-difficulty difficulty)
                 (and leetcode--display-tags (string-join tags ", ")))
                rows))))))

(defsubst leetcode--row-tags (row)
  "Get tags from ROW."
  (aref row 5))

(defsubst leetcode--row-difficulty (row)
  "Get difficulty from ROW."
  (aref row 4))

(defun leetcode--filter (rows)
  "Filter ROWS.
Rows are filtered by `leetcode--filter-regex', `leetcode--filter-tag' and
`leetcode--filter-difficulty'."
  (seq-filter
   (lambda (row)
     (and (if leetcode--filter-regex
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


;; -------------------------------------------------------------------
;;; User Commands

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
  "Toggle `leetcode--display-tags' and refresh."
  (interactive)
  (setq leetcode--display-tags (not leetcode--display-tags))
  (leetcode-refresh))

(defun leetcode-toggle-paid-display ()
  "Toggle `leetcode--display-paid' and refresh."
  (interactive)
  (setq leetcode--display-paid (not leetcode--display-paid))
  (leetcode-refresh))

(defun leetcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths (seq-reduce (lambda (acc row)
                              (cl-mapcar (lambda (a col)
                                           (max a (length col)))
                                         acc
                                         (append row '())))
                            rows
                            (seq-map #'length header-names))))
    (vconcat
     (cl-mapcar
      (lambda (col size) (list col size nil))
      header-names widths))))

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
      (tabulated-list-print t))))

(defun leetcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (let ((users-and-problems (leetcode--api-fetch-user-and-problems))
        (all-tags (leetcode--api-fetch-all-tags)))
    (if users-and-problems
        (leetcode--set-user-and-problems users-and-problems)
      (leetcode--warn "LeetCode parse user and problems failed"))
    (if all-tags
        (leetcode--set-tags all-tags)
      (leetcode--warn "LeetCode fetch all tags failed")))
  (setq leetcode--display-tags leetcode-prefer-tag-display)
  (leetcode-reset-filter)
  (leetcode-refresh))

(defun leetcode--show-problems ()
  "Show leetcode problems buffer."
  (if (get-buffer leetcode--buffer-name)
      (switch-to-buffer leetcode--buffer-name)
    (unless (leetcode--login-p)
      (leetcode-login))
    (leetcode-refresh-fetch)
    (switch-to-buffer leetcode--buffer-name))
  (leetcode--maybe-focus))

;;;###autoload
(defun leetcode ()
  "A wrapper for `leetcode--problems', because emacs-aio can not be autoloaded.
see: https://github.com/skeeto/emacs-aio/issues/3."
  (interactive)
  (if (leetcode--check-deps)
      (leetcode--show-problems)
    (message "installing leetcode dependencies...")))

;;;###autoload
(defun leetcode-daily ()
  "Open the daily challenge."
  (interactive)
  (or (leetcode--login-p)
      (leetcode))
  (let ((resp (leetcode--json-read
               (leetcode--url-retrieve leetcode--url-graphql
                 :method "POST"
                 :data `((operationName . "questionOfToday")
                         (query         . ,leetcode--url-daily-challenge))))))
    (let-alist resp
      (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
        (leetcode-show-problem (string-to-number qid))))))

(defun leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode--get-slug-title (code-buf)
  "Get slug title before try or submit with CODE-BUF.
LeetCode require slug-title as the request parameters."
  (with-current-buffer code-buf
    (if leetcode-save-solutions
        (file-name-base (cadr (split-string (buffer-name) "_")))
      (file-name-base (buffer-name)))))


;;; Submissions

(defun leetcode--poll-submission
    (status attempt wait submission-id slug-title callback &optional cbargs)
  "Poll STATUS and apply CALLBACK to result and CBARGS on sucess.
ATTEMPT is the polling count.
WAIT is time to wait when not successful.
SUBMISSION-ID and SLUG-TITLE belong to the problem being submitted."
  (leetcode-with-results "poll" status
    (if (and it (equal (alist-get 'state it) "SUCCESS"))
        (apply callback it cbargs)
      (if (>= attempt leetcode--retry-times)
          (leetcode--warn "Polling timeout: %S %S" callback cbargs)
        (sit-for wait)
        (leetcode--api-check-submission
         submission-id slug-title callback cbargs (1+ attempt) (* 1.05 wait))))))

(defun leetcode--api-check-submission
    (submission-id slug-title callback &optional cbargs attempt wait)
  "Check api to for successful submission of problem SUBMISSION-ID.
On sucess, CALLBACK is called with result and CBARGS.
SLUG-TITLE, ATTEMPT, and WAIT are passed to `leetcode--poll-submission'.
After each submission, either try testcase or submit, LeetCode returns a
SUBMISSION-ID. With the SUBMISSION-ID, client will poll for the submission
detail. SLUG-TITLE is a slugified problem title."
  (or attempt (setq attempt 0))
  (or wait (setq wait 0.5))
  (leetcode--url-retrieve (format leetcode--url-check-submission submission-id)
    :referrer (format leetcode--url-problems-submission slug-title)
    :callback #'leetcode--poll-submission
    :cbargs (list attempt wait submission-id slug-title callback cbargs)))

(defun leetcode--poll-callback (type callback &optional on-result)
  "Return a callback for submission TYPE that polls the api's check endpoint.
If ON-RESULT is non-nil, called with initial request response, not the checked
result. On success, CALLBACK is called with results."
  (macroexpand-all
   (lambda (status slug-title &rest args)
     (leetcode-with-results type status
       (let-alist it
         (and on-result (funcall on-result it))
         (leetcode--api-check-submission
          (if (equal type "submit") .submission_id .interpret_id)
          slug-title callback args))))))

(defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (pcase-let* ((code-buf (current-buffer))
               (slug-title (leetcode--get-slug-title code-buf))
               ((cl-struct leetcode-problem id backend-id)
                (leetcode--get-problem slug-title))
               (result-buf (leetcode--result-buffer-name id)))
    (leetcode--debug "leetcode-submit slug-title=%s, problem-id=%s" slug-title id)
    (leetcode--url-retrieve (format leetcode--url-submit slug-title)
      :method "POST"
      :referrer (format leetcode--url-problems-submission slug-title)
      :data `((lang        . ,leetcode--lang)
              (question_id . ,backend-id)
              (typed_code  . ,(leetcode--buffer-content code-buf)))
      :callback (leetcode--poll-callback
                 "submit" #'leetcode--show-submission-result)
      :cbargs (list slug-title result-buf code-buf))))

(defun leetcode--show-submission-result (result result-buf code-buf)
  "Format RESULT in RESULT-BUF based on status code.
CODE-BUF is the source buffer.
Error info comes from submission RESULT.

\\='status_code has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (with-current-buffer code-buf (leetcode--loading-mode -1))
  (cl-macrolet ((status-error (msg)
                  `(format "Status: %s"
                           (leetcode--add-font-lock ,msg 'leetcode-error-face)))
                (status-result (msg correct tests face)
                  `(format "Status: %s\n\n"
                           (leetcode--add-font-lock
                            (format "%s (%s/%s)" ,msg ,correct ,tests) ',face))))
    (let-alist result
      (with-current-buffer (get-buffer-create result-buf)
        (erase-buffer)
        (font-lock-mode +1)
        (pcase .status_code
          (10 (insert
               (status-result .status_msg .total_correct .total_testcases
                              leetcode-accepted-face)
               (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                       .status_runtime .runtime_percentile .pretty_lang)
               (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                       .status_memory .memory_percentile .pretty_lang)))
          (11 (insert (status-result .status_msg .total_correct .total_testcases
                                     leetcode-error-face)
                      (format "Test Case: \n%s\n\n" .input)
                      (format "Answer: %s\n\n" .code_output)
                      (format "Expected Answer: %s\n\n" .expected_output))
              (unless (string-empty-p .std_output)
                (insert (format "Stdout: \n%s\n" .std_output))))
          (14 (insert (status-error .status_msg) "\n"))
          (15 (insert (status-error .status_msg) "\n\n"
                      (format .full_runtime_error)))
          (20 (insert (status-error .status_msg) "\n\n"
                      (format .full_compile_error))))
        (display-buffer (current-buffer)
                        '((display-buffer-reuse-window
                           leetcode--display-result)
                          (reusable-frames . visible)))))))


(defun leetcode-try ()
  "Asynchronously test the code using testcases from test buffer."
  (interactive)
  (pcase-let* ((code-buf (current-buffer))
               (slug-title (leetcode--get-slug-title code-buf))
               ((cl-struct leetcode-problem id backend-id)
                (leetcode--get-problem slug-title))
               (result-buf (get-buffer (leetcode--result-buffer-name id)))
               (testcase-buf (get-buffer (leetcode--testcase-buffer-name id))))
    (leetcode--debug "leetcode-try slug-title=%s, problem-id=%s" slug-title id)
    (leetcode--url-retrieve (format leetcode--url-try slug-title)
      :referrer (format leetcode--url-problems-submission slug-title)
      :method "POST"
      :data `((data_input  . ,(leetcode--buffer-content testcase-buf))
              (judge_type  . "small")
              (lang        . ,leetcode--lang)
              (question_id . ,backend-id)
              (typed_code  . ,(leetcode--buffer-content code-buf)))
      :callback (leetcode--poll-callback
                 "try" #'leetcode--update-results
                 (lambda (res)
                   (let-alist res
                     (with-current-buffer result-buf
                       (erase-buffer)
                       (insert (concat "Your input:\n" .test_case "\n\n"))))))
      :cbargs (list slug-title result-buf code-buf))))

(defun leetcode--update-results (results result-buf code-buf)
  "Update RESULTS in RESULT-BUF for SLUG-TITLE after running test cases.
CODE-BUF is the source buffer."
  (with-current-buffer code-buf (leetcode--loading-mode -1))
  (let-alist results
    (with-current-buffer result-buf
      (goto-char (point-max))
      (pcase .status_code
        (10 (insert "Output:\n")
            (dotimes (i (length .code_answer))
              (insert (aref .code_answer i) "\n"))
            (insert "\nExpected:\n")
            (dotimes (i (length .expected_code_answer))
              (insert (aref .expected_code_answer i) "\n"))
            (insert "\n"))
        (14 (insert .status_msg))
        (15 (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face)
                    "\n\n" .full_runtime_error))
        (20 (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face)
                    "\n\n" .full_compile_error)))
      (when (> (length .code_output) 0)
        (insert "\n\n")
        (insert "Code output:\n")
        (dolist (item (append .code_output nil))
          (insert item "\n")))
      (insert "\n\n"))))


(defun leetcode--solving-window-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |     Detail     |
|               |                |
|               +----------------+
|     Code      |   Customize    |
|               |   Testcases    |
|               +----------------+
|               |Submit/Testcases|
|               |    Result      |
+---------------+----------------+"
  (delete-other-windows)
  (setq leetcode--description-window (split-window-horizontally))
  (other-window 1)
  (setq leetcode--testcase-window (split-window-below))
  (other-window 1)
  (setq leetcode--result-window (split-window-below))
  (other-window -1)
  (other-window -1))

(defun leetcode--display-result (buffer &optional _alist)
  "Display function for LeetCode result.
BUFFER is used to show LeetCode result. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-testcase (buffer &optional _alist)
  "Display function for LeetCode testcase.
BUFFER is used to show LeetCode testcase. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-detail (buffer &optional _alist)
  "Display function for LeetCode detail.
BUFFER is used to show LeetCode detail. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-top-child
                 (window-next-sibling
                  (window-left-child
                   (frame-root-window))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-code (buffer &optional _alist)
  "Display function for LeetCode code.
BUFFER is the one to show LeetCode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))


(defun leetcode--show-problem (problem problem-info)
  "Show the detail of PROBLEM, whose meta data is PROBLEM-INFO.
Use `shr-render-buffer' to render problem detail. This action
will show the detail in other window and jump to it."
  (let* ((problem-id (leetcode-problem-id problem-info))
         (title (leetcode-problem-title problem-info))
         (difficulty-level (leetcode-problem-difficulty problem-info))
         (difficulty (leetcode--stringify-difficulty difficulty-level))
         (buf-name (leetcode--detail-buffer-name problem-id))
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (leetcode--debug "select title: %s" title)
    (leetcode--maybe-focus)
    (let-alist problem
      (when (get-buffer buf-name)
        (kill-buffer buf-name))
      (with-temp-buffer
        (insert (concat "<h1>" (number-to-string problem-id) ". " title "</h1>"))
        (insert (concat (capitalize difficulty) html-margin
                        "likes: " (number-to-string .likes) html-margin
                        "dislikes: " (number-to-string .dislikes)))
        ;; Sometimes LeetCode don't have a '<p>' at the outermost...
        (insert "<p>" .content "</p>")
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
                              'action (lambda (_btn)
                                        (leetcode--start-coding problem problem-info))
                              'help-echo "Solve the problem.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Link"
                              'action (lambda (_btn)
                                        (browse-url (leetcode--problem-link title)))
                              'help-echo "Open the problem in browser.")
          (insert (make-string 4 ?\s))
          (insert-text-button "Solution"
                              'action (lambda (_btn)
                                        (browse-url (concat (leetcode--problem-link title) "/solution")))
                              'help-echo "Open the problem solution page in browser."))
        (rename-buffer buf-name)
        (leetcode--problem-detail-mode)
        (switch-to-buffer (current-buffer))
        (search-backward "Solve it")))))

(defun leetcode-show-problem (problem-id)
  "Show the detail of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
detail. This action will show the detail in other window and jump
to it."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem-info (leetcode--get-problem-by-id problem-id))
         (title (leetcode-problem-title problem-info))
         (problem (leetcode--api-fetch-problem title)))
    (leetcode--show-problem problem problem-info)))

(defun leetcode-show-problem-by-slug (slug-title)
  "Show the detail of problem with SLUG-TITLE.
This function will work after first run \\[leetcode]. Get problem by id and use
`shr-render-buffer' to render problem detail. This action will show the detail
in other window and jump to it.

It can be used in org-link elisp:(leetcode-show-problem-by-slug \"3sum\")."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (seq-find (lambda (p)
                              (equal slug-title
                                     (leetcode--slugify-title
                                      (leetcode-problem-title p))))
                            (leetcode-problems-problems leetcode--problems)))
         (problem-id (leetcode-problem-id problem))
         (problem-info (leetcode--get-problem-by-id problem-id))
         (title (leetcode-problem-title problem-info))
         (_problem  (leetcode--api-fetch-problem title)))
    (leetcode-show-problem problem-id)))

(defun leetcode-show-current-problem ()
  "Show current problem's detail.
Call `leetcode-show-problem' on the current problem id. This
action will show the detail in other window and jump to it."
  (interactive)
  (leetcode-show-problem (leetcode--get-current-problem-id)))

(defun leetcode-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `leetcode-show-problem', but instead of jumping to
the detail window, this action will jump back in `LC Problems'."
  (interactive (list (read-number "View problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (leetcode-show-problem problem-id)
  (leetcode--jump-to-window-by-buffer-name leetcode--buffer-name))

(defun leetcode-view-current-problem ()
  "View current problem while staying in `LC Problems' window.
Similar with `leetcode-show-current-problem', but instead of
jumping to the detail window, this action will jump back in `LC
Problems'."
  (interactive)
  (leetcode-view-problem (leetcode--get-current-problem-id)))

(defun leetcode-show-problem-in-browser (problem-id)
  "Open the problem with id PROBLEM-ID in browser."
  (interactive (list (read-number "Show in browser by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem-by-id problem-id))
         (title (leetcode-problem-title problem))
         (link (leetcode--problem-link title)))
    (leetcode--debug "open in browser: %s" link)
    (browse-url link)))

(defun leetcode-show-current-problem-in-browser ()
  "Open the current problem in browser.
Call `leetcode-show-problem-in-browser' on the current problem id."
  (interactive)
  (leetcode-show-problem-in-browser (leetcode--get-current-problem-id)))

(defun leetcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-number "Solve the problem with id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem-info (leetcode--get-problem-by-id problem-id))
         (title (leetcode-problem-title problem-info))
         (problem (leetcode--api-fetch-problem title)))
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
  (mapc (lambda (title)
          (leetcode--kill-buff-and-delete-window
           (get-buffer (leetcode--get-code-buffer-name title)))
          (let* ((slug-title (leetcode--slugify-title title))
                 (problem (leetcode--get-problem slug-title))
                 (problem-id (leetcode-problem-id problem)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--detail-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--result-buffer-name problem-id)))
            (leetcode--kill-buff-and-delete-window (get-buffer (leetcode--testcase-buffer-name problem-id)))))
        leetcode--problem-titles))

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in SNIPPETS."
  (setq leetcode--lang
        ;; if there is a mysql snippet, we use mysql as our prefer language.
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
  "Get problem from `leetcode--problems' by SLUG-TITLE."
  (seq-find (lambda (p)
              (equal slug-title
                     (leetcode--slugify-title
                      (leetcode-problem-title p))))
            (leetcode-problems-problems leetcode--problems)))

(defun leetcode--get-problem-by-id (id)
  "Get problem from `leetcode--problems' by ID."
  (seq-find (lambda (p)
              (equal id (leetcode-problem-id p)))
            (leetcode-problems-problems leetcode--problems)))

(defun leetcode--get-problem-id (slug-title)
  "Get problem id by SLUG-TITLE."
  (let ((problem (leetcode--get-problem slug-title)))
    (leetcode-problem-id problem)))

(defun leetcode--get-current-problem-id ()
  "Get id of the current problem."
  (string-to-number (aref (tabulated-list-get-entry) 1)))

(defun leetcode--start-coding (problem problem-info)
  "Create a buffer for coding PROBLEM with meta-data PROBLEM-INFO.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
  (let-alist problem
    (let* ((title (leetcode-problem-title problem-info))
           (problem-id (leetcode-problem-id problem-info))
           (testcase-buf-name (leetcode--testcase-buffer-name problem-id))
           (result-buf-name (leetcode--result-buffer-name problem-id))
           (snippets (append .codeSnippets nil))
           (testcase .sampleTestCase))
      (add-to-list 'leetcode--problem-titles title)
      (leetcode--solving-window-layout)
      (leetcode--set-lang snippets)
      (let* ((_slug-title (leetcode--slugify-title title))
             (code-buf-name (leetcode--get-code-buffer-name title))
             (code-buf (leetcode--get-code-buffer code-buf-name))
             (suffix (assoc-default
                      leetcode--lang
                      leetcode--lang-suffixes)))
        (if (= (buffer-size code-buf) 0)
            (with-current-buffer code-buf
              (setq code-buf (current-buffer))
              (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
              (leetcode-solution-mode t)
              (let* ((snippet (seq-find (lambda (s)
                                          (equal (alist-get 'langSlug s)
                                                 leetcode--lang))
                                        snippets))
                     (template-code (alist-get 'code snippet)))
                (unless (save-mark-and-excursion
                          (goto-char (point-min))
                          (search-forward (string-trim template-code) nil t))
                  (insert template-code))
                (leetcode--replace-in-buffer "" "")))
          (with-current-buffer code-buf
            (leetcode-solution-mode t)))

        (display-buffer code-buf
                        '((display-buffer-reuse-window
                           leetcode--display-code)
                          (reusable-frames . visible))))
      (with-current-buffer (get-buffer-create testcase-buf-name)
        (erase-buffer)
        (insert testcase)
        (set-window-buffer leetcode--testcase-window (current-buffer)))
      (with-current-buffer (get-buffer-create result-buf-name)
        (erase-buffer)
        (set-window-buffer leetcode--result-window (current-buffer))))))

(defun leetcode-restore-layout ()
  "This command should be run in LeetCode code buffer.
It will restore the layout based on current buffer's name."
  (interactive)
  (let* ((slug-title (file-name-sans-extension (buffer-name)))
         (problem (leetcode--get-problem slug-title))
         (problem-id (leetcode-problem-id problem))
         (desc-buf (get-buffer (leetcode--detail-buffer-name problem-id)))
         (testcase-buf (get-buffer-create (leetcode--testcase-buffer-name problem-id)))
         (result-buf (get-buffer-create (leetcode--result-buffer-name problem-id))))
    (leetcode--solving-window-layout)
    (unless desc-buf
      (leetcode-show-problem problem-id))
    (display-buffer desc-buf
                    '((display-buffer-reuse-window
                       leetcode--display-detail)
                      (reusable-frames . visible)))
    (display-buffer testcase-buf
                    '((display-buffer-reuse-window
                       leetcode--display-testcase)
                      (reusable-frames . visible)))
    (display-buffer result-buf
                    '((display-buffer-reuse-window
                       leetcode--display-result)
                      (reusable-frames . visible)))))

;; -------------------------------------------------------------------
;;; Problems Mode

(defvar-keymap leetcode--problems-mode-map
  :doc "Keymap active in `leetcode--problems-mode'."
  :suppress t
  "RET" #'leetcode-show-current-problem
  "TAB" #'leetcode-view-current-problem
  "o" #'leetcode-show-current-problem
  "O" #'leetcode-show-problem
  "v" #'leetcode-view-current-problem
  "V" #'leetcode-view-problem
  "b" #'leetcode-show-current-problem-in-browser
  "B" #'leetcode-show-problem-in-browser
  "c" #'leetcode-solve-current-problem
  "C" #'leetcode-solve-problem
  "s" #'leetcode-set-filter-regex
  "L" #'leetcode-set-prefer-language
  "t" #'leetcode-set-filter-tag
  "T" #'leetcode-toggle-tag-display
  "P" #'leetcode-toggle-paid-display
  "d" #'leetcode-set-filter-difficulty
  "g" #'leetcode-refresh
  "G" #'leetcode-refresh-fetch
  "r" #'leetcode-reset-filter
  "q" #'quit-window)

(define-derived-mode leetcode--problems-mode tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-refresh nil t))

(defvar evil-normal-state-local-map)
(defun leetcode--set-evil-local-map (map)
  "Set `evil-normal-state-local-map' to MAP."
  (when (featurep 'evil)
    (define-key map "h" nil)
    (define-key map "v" nil)
    (define-key map "V" nil)
    (define-key map "b" nil)
    (define-key map "B" nil)
    (define-key map "g" nil)
    (define-key map "G" nil)
    (define-key map "z" #'leetcode-refresh)
    (define-key map "Z" #'leetcode-refresh-fetch)
    (setq evil-normal-state-local-map map)))

(add-hook 'leetcode--problems-mode-hook #'hl-line-mode)
(add-hook 'leetcode--problems-mode-hook
          (lambda () (leetcode--set-evil-local-map leetcode--problems-mode-map)))


;; -------------------------------------------------------------------
;;; Detail Mode

(defvar-keymap leetcode--problem-detail-mode-map
  :doc "Keymap active in `leetcode--problem-detail-mode'."
  :suppress t
  "q" #'quit-window)

(define-derived-mode leetcode--problem-detail-mode special-mode "LC Detail"
  "Major mode for displaying problem details.")

(add-hook 'leetcode--problem-detail-mode-hook
          (lambda () (leetcode--set-evil-local-map leetcode--problem-detail-mode-map)))


;; -------------------------------------------------------------------
;;; Loading Mode 

;;; Use spinner.el to show progress indicator
(defvar leetcode--spinner (spinner-create 'progress-bar-filled)
  "Progress indicator to show request progress.")

(defconst leetcode--loading-lighter
  '(" [LeetCode" (:eval (spinner-print leetcode--spinner)) "]"))

(define-minor-mode leetcode--loading-mode
  "Minor mode to showing leetcode loading status."
  :require 'leetcode
  :lighter leetcode--loading-lighter
  (if leetcode--loading-mode
      (spinner-start leetcode--spinner)
    (spinner-stop leetcode--spinner)))


;; -------------------------------------------------------------------
;;; Solution Mode
 
(defvar-keymap leetcode-solution-mode-map
  :doc "Keymap for `leetcode-solution-mode'."
  "C-c C-t" #'leetcode-try
  "C-c C-s" #'leetcode-submit
  "C-c C-r" #'leetcode-restore-layout)

(define-minor-mode leetcode-solution-mode
  "Minor mode to provide shortcut and hooks."
  :lighter " LC-Solution"
  :require 'leetcode)

(provide 'leetcode)
;;; leetcode.el ends here
