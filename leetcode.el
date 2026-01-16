;;; leetcode.el --- An leetcode client           -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2019  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/leetcode.el
;; Package-Requires: ((emacs "28.1") (s "1.13.0") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.27

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

(require 's)
(require 'aio)
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
  (let ((async-shell-command-display-buffer t)
        (pipx (executable-find "pipx"))
        (python3 (executable-find "python3"))
        (python (executable-find "python")))
    (async-shell-command
     (if pipx
         (format "%s install my_cookies" pipx)
       (format "%s -m venv --clear %s && %s/bin/pip3 install my_cookies"
               (or python3 python "python") ; require python environment
               leetcode-python-environment leetcode-python-environment))
     (get-buffer-create "*leetcode-install*"))))

(defun leetcode--my-cookies-path ()
  "Find the path to the my_cookies executable."
  (or (executable-find (format "%s/bin/my_cookies" leetcode-python-environment))
      (executable-find "my_cookies")))

(defun leetcode--check-deps ()
  "Check if all dependencies installed."
  (if (leetcode--my-cookies-path)
      t
    (leetcode--install-my-cookie)
    nil))

(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

(defcustom leetcode-prefer-tag-display t
  "Whether to display tags by default in the *leetcode* buffer."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-prefer-language "python3"
  "LeetCode programming language.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-prefer-sql "mysql"
  "LeetCode sql implementation.
mysql, mssql, oraclesql."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-directory "~/leetcode"
  "Directory to save solutions."
  :group 'leetcode
  :type 'string)

(defcustom leetcode-save-solutions nil
  "If it's t, save leetcode solutions to `leetcode-directory'."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-focus t
  "When execute `leetcode', always delete other windows."
  :group 'leetcode
  :type 'boolean)

(defcustom leetcode-python-environment (file-name-concat user-emacs-directory "leetcode-env")
  "The path to the isolated python virtual-environment to use."
  :group 'leetcode
  :type 'directory)

(cl-defstruct leetcode-user
  "A LeetCode User.
The object with following attributes:
:username   String
:id         Int
:is-premium Boolean {t|nil}"
  username id is-premium)

(cl-defstruct leetcode-snippet
  "A code snippet.
:lang String
:lang-slug String
:code String

We need both :lang and :lang-slug, because some programming
languages name conversion is not 'a-b-c' <=> 'aBC'.

For example: :lang 'C++' and :lang-slug 'cpp', :lang 'C#' and
:lang-slug 'csharp'."
  lang lang-slug code)

(cl-defstruct leetcode-problem
  "A single LeetCode problem.
:status     String
:id         String
:backend-id String
:title      String
:title-slug String
:acceptance String
:difficulty String {Easy,Medium,Hard}
:paid-only  Boolean {t|nil}
:likes      Number
:dislikes   Number
:tags       List
:content    String
:snippets   List {leetcode-snippet}
:testcases  List {String}

'id' is frontend id in LeetCode. We almost always use frontend id
in 'leetcode.el'."
  status id backend-id title title-slug acceptance
  difficulty paid-only likes dislikes tags content
  snippets testcases)

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
  "A map of language slug name to LeetCode programming language suffix.
c, cpp, csharp, golang, java, javascript, typescript, kotlin, php, python,
python3, ruby, rust, scala, swift, mysql, mssql, oraclesql.")

(defconst leetcode--code-start "// code_start"
  "Code start mark in LeetCode description.")
(defconst leetcode--code-end "// code_end"
  "Code end mark in LeetCode description.")

(defvar leetcode--filter-regex nil "Filter rows by regex.")
(defvar leetcode--filter-tag nil "Filter rows by tag.")
(defvar leetcode--filter-difficulty nil
  "Filter rows by difficulty, it can be \"easy\", \"medium\" and \"hard\".")

(defconst leetcode--all-difficulties '("Easy" "Medium" "Hard"))
(defconst leetcode--paid "•" "Paid mark.")
(defconst leetcode--checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name             "*leetcode*")

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

(defface leetcode-accepted-face
  '((t (:foreground "#228b22")))
  "Face for submission accepted."
  :group 'leetcode)

(defface leetcode-error-face
  '((t (:foreground "#dc143c")))
  "Face for submission compile error, runtime error and TLE."
  :group 'leetcode)

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
(defconst leetcode--url-problems            (concat leetcode--url-base "/problems/%s/"))

(defconst leetcode--graphql-global-data "
query globalData {
  userStatus { userId username isPremium activeSessionId isSignedIn }
}")

;; graphql.el doesn't support `:as' keyword, so let's use the raw graphQL string.
(defconst leetcode--graphql-problemset-question-list "
query problemsetQuestionList($categorySlug: String, $limit: Int, $skip: Int, $filters: QuestionListFilterInput) {
  problemsetQuestionList: questionList(
    categorySlug: $categorySlug
    limit: $limit
    skip: $skip
    filters: $filters
  ) {
    total: totalNum
    questions: data {
      acRate
      difficulty
      freqBar
      frontendQuestionId: questionFrontendId
      isFavor
      paidOnly: isPaidOnly
      status
      title
      titleSlug
      topicTags { name id slug }
      hasSolution
      hasVideoSolution
    }
  }
}")

(defconst leetcode--graphql-question-title "
query questionTitle($titleSlug: String!) {
  question(titleSlug: $titleSlug) { questionId questionFrontendId title titleSlug
                                    isPaidOnly difficulty likes dislikes categoryTitle } }")

(defconst leetcode--graphql-question-content "
query questionContent($titleSlug: String!) {
  question(titleSlug: $titleSlug) { content mysqlSchemas dataSchemas } }")

(defconst leetcode--graphql-question-editor-data "
query questionEditorData($titleSlug: String!) {
  question(titleSlug: $titleSlug) {
    questionId
    questionFrontendId
    codeSnippets { lang langSlug code }
    envInfo
    enableRunCode
    hasFrontendPreview
    frontendPreviews
  }
}")

(defconst leetcode--graphql-question-hints "
query questionHints($titleSlug: String!) {
  question(titleSlug: $titleSlug) { hints } }")

(defconst leetcode--graphql-console-panel-config "
query consolePanelConfig($titleSlug: String!) {
  question(titleSlug: $titleSlug) {
    questionId
    questionFrontendId
    questionTitle
    enableDebugger
    enableRunCode
    enableSubmit
    enableTestMode
    exampleTestcaseList
    metaData
  }
}")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode--insert-code-start-marker ()
  "Insert code start marker."
  (when (or (string= leetcode--lang "c")
            (string= leetcode--lang "cpp"))
    (insert (format "\n\n%s\n\n" leetcode--code-start))))

(defun leetcode--insert-code-end-marker ()
  "Insert code end marker."
  (when (or (string= leetcode--lang "c")
            (string= leetcode--lang "cpp"))
    (insert (format "\n\n%s\n\n" leetcode--code-end))))

(defun leetcode--referer (value)
  "It will return an alist as the HTTP Referer Header.
VALUE should be the referer."
  (cons "Referer" value))

(defun leetcode--cookie-get-all ()
  "Get leetcode session with `my_cookies'. You can install it with pip."
  (let* ((my-cookies (leetcode--my-cookies-path))
         (my-cookies-output (shell-command-to-string (leetcode--my-cookies-path)))
         (cookies-list (seq-filter (lambda (s) (not (string-empty-p s)))
                                   (s-split "\n" my-cookies-output 'OMIT-NULLS)))
         (cookies-pairs (seq-map (lambda (s) (s-split-up-to " " s 1 'OMIT-NULLS)) cookies-list)))
    cookies-pairs))

(defun leetcode--cookie-get (cookie-key)
  "Get LeetCode cookie value by COOKIE-KEY."
  (if-let ((cookie (seq-find
                    (lambda (item)
                      (string= (aref item 1) cookie-key))
                    (url-cookie-retrieve leetcode--domain "/" t))))
      (aref cookie 2)))

(defun leetcode--maybe-csrf-token ()
  "Return LeetCode CSRF token if it exists, otherwise return nil."
  (leetcode--cookie-get leetcode--cookie-csrftoken))

(defun leetcode--maybe-session ()
  "Return LeetCode session if it exists, otherwise return nil."
  (leetcode--cookie-get leetcode--cookie-session))

(aio-defun leetcode--csrf-token ()
  "Return csrf token."
  (unless (leetcode--maybe-csrf-token)
    (aio-await (leetcode--login))
    (aio-await (leetcode--login)))
  (leetcode--maybe-csrf-token))

(defun leetcode--login-p ()
  "Whether user is login."
  (let ((username (leetcode-user-username leetcode--user)))
    (and username
         (not (string-empty-p username))
         (leetcode--maybe-session))))

(defun leetcode--slugify-title (title)
  "Make TITLE a slug title.
Such as 'Two Sum' will be converted to 'two-sum'. 'Pow(x, n)' will be 'powx-n'"
  (let* ((str1 (replace-regexp-in-string "[\s-]+" "-" (downcase title)))
         (res (replace-regexp-in-string "[(),']" "" str1)))
    res))

(defun leetcode--replace-in-buffer (regex to)
  "Replace string matched REGEX in `current-buffer' to TO."
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward regex (point-max) t)
          (replace-match to))))))

(defun leetcode--problem-link (title)
  "Generate problem link from TITLE."
  (concat leetcode--url-base "/problems/" (leetcode--slugify-title title)))

(defun leetcode--stringify-difficulty (difficulty)
  "Add font-lock to DIFFICULTY."
  (pcase difficulty
    ("Easy" (leetcode--add-font-lock "Easy" 'leetcode-easy-face))
    ("Medium" (leetcode--add-font-lock "Medium" 'leetcode-medium-face))
    ("Hard" (leetcode--add-font-lock "Hard" 'leetcode-hard-face))))

(defun leetcode--add-font-lock (str face)
  "Add font-lock FACE to STR."
  (prog1 str
    (put-text-property 0 (length str) 'font-lock-face face str)))

(defun leetcode--detail-buffer-name (problem-id)
  "Detail buffer name with PROBLEM-ID."
  (format "*leetcode-detail-%s*" problem-id))

(defun leetcode--testcase-buffer-name (problem-id)
  "Testcase buffer name with PROBLEM-ID."
  (format "*leetcode-testcase-%s*" problem-id))

(defun leetcode--result-buffer-name (problem-id)
  "Result buffer name with PROBLEM-ID."
  (format "*leetcode-result-%s*" problem-id))

(defun leetcode--maybe-focus ()
  "Delete other windows, keep only *leetcode* buffer."
  (if leetcode-focus (delete-other-windows)))

(defun leetcode--parse-buffer (buffer)
  "Parse BUFFER content from json to alist."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (json-read)))

(aio-defun leetcode--common-extra-headers ()
  "Common extra headers for `url-request-extra-headers'."
  `(,leetcode--User-Agent ,leetcode--Content-Type
    ,(cons leetcode--X-CSRFToken (aio-await (leetcode--csrf-token)))))

(defun leetcode--buffer-content (buf)
  "Get content without text properties of BUF."
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun leetcode--testcase-buffer-data (problem-id)
  "Get testcases buffer content of PROBLEM-ID."
  (leetcode--buffer-content (get-buffer (leetcode--testcase-buffer-name problem-id))))

(defun leetcode--code-buffer-data ()
  "Get code buffer content, that is, the `current-buffer'."
  (let ((code (leetcode--buffer-content (current-buffer)))
        (pattern (concat leetcode--code-start "\\([\0-\377]*?\\)" leetcode--code-end)))
    (if (string-match pattern code)
        (match-string 1 code)
      code)))

(defun leetcode--get-slug-title (code-buf)
  "Get slug title before try or submit with CODE-BUF.
LeetCode require slug-title as the request parameters."
  (with-current-buffer code-buf
    (if leetcode-save-solutions
        (file-name-base (cadr (s-split "_" (buffer-name))))
      (file-name-base (buffer-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LeetCode API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode--graphql-payload (operation query &optional vars)
  "Construct GraphQL request payload with OPERATION, QUERY or maybe VARS."
  (json-encode
   (let ((ret `(("operationName" . ,operation)
                ("query" . ,query))))
     (if vars `(,@ret ("variables" . ,vars)) ret))))


(defmacro leetcode--define-graphql (query-name args &rest body)
  "Define LeetCode GraphQL queries.
Define a function with name of 'leetcode--fetch-<QUERY-NAME>',
and the ARGS will be the arguments of the defined function. BODY
will be executed when query successfully.

GraphQL request is defined with 'leetcode--graphql-<QUERY-NAME>'.
In the GraqhQL request body, operation name is lower camel case
of QUERY-NAME."
  (declare (indent defun) (doc-string 3))
  (let ((variables (mapcar (lambda (arg) `(cons (s-lower-camel-case (symbol-name ',arg)) ,arg)) args)))
    `(aio-defun ,(intern (concat "leetcode--fetch-" (symbol-name query-name))) ,args
       (let* ((graphql-operation-name ,(s-lower-camel-case (symbol-name query-name)))
              (graphql-body ,(intern (concat "leetcode--graphql-" (symbol-name query-name))))
              (payload (leetcode--graphql-payload graphql-operation-name
                                                  graphql-body
                                                  (list ,@variables)))
              (url-request-method "POST")
              (url-request-extra-headers `(,leetcode--User-Agent ,leetcode--Content-Type))
              (url-request-data payload)
              (response (aio-await (aio-url-retrieve leetcode--url-graphql)))
              (response-status (car response))
              (response-buffer (cdr response)))
         (if-let ((error (plist-get response-status :error)))
             (switch-to-buffer response-buffer)
           (let-alist (with-current-buffer response-buffer (goto-char url-http-end-of-headers) (json-read))
             ,@body))))))

(leetcode--define-graphql global-data ()
  (setf (leetcode-user-id leetcode--user) .data.userStatus.userId)
  (setf (leetcode-user-username leetcode--user) .data.userStatus.username)
  (setf (leetcode-user-is-premium leetcode--user) .data.userStatus.isPremium))

(leetcode--define-graphql problemset-question-list (category-slug skip limit filters)
  ;; problem list
  (setf (leetcode-problems-num leetcode--problems) .data.problemsetQuestionList.total
        (leetcode-problems-tag leetcode--problems) "all")
  (let (problems)
    (dotimes (i .data.problemsetQuestionList.total)
      (let-alist (aref .data.problemsetQuestionList.questions i)
        (push (make-leetcode-problem
               :status     .status
               :id         .frontendQuestionId
               :title      .title
               :title-slug .titleSlug
               :acceptance (format "%.1f%%" .acRate)
               :difficulty .difficulty
               :paid-only  (eq .paidOnly t)
               :tags       (seq-reduce (lambda (tags tag)
                                         (let-alist tag
                                           (push .slug tags)))
                                       .topicTags '()))
              problems)
        (setq leetcode--all-tags (append leetcode--all-tags (leetcode-problem-tags (car problems))))))
    (setf (leetcode-problems-problems leetcode--problems) (nreverse problems))
    ;; problem tags
    (delete-dups leetcode--all-tags)))

(leetcode--define-graphql question-content (title-slug)
  (let ((problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-content problem) .data.question.content)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql question-title (title-slug)
  (let ((problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-likes problem) .data.question.likes)
          (setf (leetcode-problem-dislikes problem) .data.question.dislikes)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql console-panel-config (title-slug)
  (let ((id .data.question.questionFrontendId)
        (testcases (append .data.question.exampleTestcaseList nil))
        (problem (leetcode--get-problem title-slug)))
    (if problem
        (progn
          (setf (leetcode-problem-testcases problem) testcases)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(leetcode--define-graphql question-editor-data (title-slug)
  (let ((id .data.question.questionFrontendId)
        (problem (leetcode--get-problem title-slug))
        (snippets (seq-map (lambda (snippet-alist)
                             (let-alist snippet-alist
                               (make-leetcode-snippet
                                :lang      .lang
                                :lang-slug .langSlug
                                :code      .code)))
                           .data.question.codeSnippets)))
    (if problem
        (progn
          (setf (leetcode-problem-snippets problem) snippets)
          (setf (leetcode-problem-backend-id problem) .data.question.questionId)
          problem)
      (user-error "LeetCode problem not found: %s" title-slug))))

(defalias 'leetcode--fetch-user-status (symbol-function 'leetcode--fetch-global-data))
(defalias 'leetcode--fetch-question-list (symbol-function 'leetcode--fetch-problemset-question-list))
(defalias 'leetcode--fetch-question-testcases (symbol-function 'leetcode--fetch-console-panel-config))
(defalias 'leetcode--fetch-question-snippets (symbol-function 'leetcode--fetch-question-editor-data))

(aio-defun leetcode--ensure-question-title (problem)
  (if (and (leetcode-problem-dislikes problem)
           (leetcode-problem-likes problem))
      problem
    (aio-await (leetcode--fetch-question-title
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-content (problem)
  (if (leetcode-problem-content problem)
      problem
    (aio-await (leetcode--fetch-question-content
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-snippets (problem)
  (if (leetcode-problem-snippets problem)
      problem
    (aio-await (leetcode--fetch-question-snippets
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--ensure-question-testcases (problem)
  (if (leetcode-problem-testcases problem)
      problem
    (aio-await (leetcode--fetch-question-testcases
                (leetcode-problem-title-slug problem)))))

(aio-defun leetcode--api-interpret-solution  (problem)
  "Fetch PROBLEM interpret_id."
  (let* ((title-slug (leetcode-problem-title-slug problem))
         (problem-id (leetcode-problem-id problem))
         (backend-id (leetcode-problem-backend-id problem))
         (payload (json-encode `((data_input . ,(leetcode--testcase-buffer-data problem-id))
                                 (lang . ,leetcode--lang)
                                 (question_id . ,backend-id)
                                 (typed_code . ,(leetcode--code-buffer-data)))))
         (url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer (format leetcode--url-problems title-slug))))
         (url-request-data payload)
         (response (aio-await (aio-url-retrieve (format leetcode--url-try title-slug))))
         (response-status (car response))
         (response-buffer (cdr response)))
    (if-let ((error-info (plist-get response-status :error)))
        (progn
          (switch-to-buffer response-buffer)
          (leetcode--warn "LeetCode interpret problem ERROR: %S" error-info))
      (let-alist (with-current-buffer response-buffer (goto-char url-http-end-of-headers) (json-read))
        .interpret_id))))

(aio-defun leetcode--api-submit (backend-id slug-title code)
  "Submit CODE for problem which has BACKEND-ID and SLUG-TITLE."
  (message "LeetCode submit slug-title: %s, backend-id: %s" slug-title backend-id)
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer (format leetcode--url-problems-submission slug-title))))
         (url-request-data
          (json-encode `((lang . ,leetcode--lang)
                         (question_id . ,backend-id)
                         (typed_code . ,code)))))
    (aio-await (aio-url-retrieve (format leetcode--url-submit slug-title)))))

(aio-defun leetcode--api-check-submission (interpret-id problem on-success)
  "Polling problem with PROBLEM-ID submission by INTERPRET-ID.
When check submission success, execute ON-SUCCESS. Both
`leetcode-try' and `leetcode-submit' rely on this to poll
submission status."
  (message "LeetCode check submission: %s" (format leetcode--url-check-submission interpret-id))
  (let* ((title-slug (leetcode-problem-title-slug problem))
         (problem-id (leetcode-problem-id problem))
         (url-request-method "GET")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer (format leetcode--url-problems title-slug))))
         (response (aio-await (aio-url-retrieve (format leetcode--url-check-submission interpret-id))))
         (response-status (car response))
         (response-buffer (cdr response)))
    (if-let ((error-info (plist-get response-status :error)))
        (progn
          (switch-to-buffer response-buffer)
          (leetcode--warn "LeetCode check submission ERROR: %S" error-info))
      (let ((result (leetcode--parse-buffer response-buffer)))
        (let-alist result
          (pcase .state
            ((or "PENDING" "STARTED") ((aio-await (aio-sleep 0.2))
                                       (aio-await (leetcode--api-check-submission interpret-id problem on-success))))
            ("SUCCESS" (funcall on-success problem-id result))))))))


(aio-defun leetcode--login ()
  "We are not login actually, we are retrieving LeetCode login session
from local browser. It also cleans LeetCode cookies in `url-cookie-file'."
  (ignore-errors (url-cookie-delete-cookies leetcode--domain))
  (let* ((leetcode-cookie (leetcode--cookie-get-all)))
    (cl-loop for (key value) in leetcode-cookie
             do (url-cookie-store key value nil leetcode--domain "/" t)))
  ;; After login, we should have our user data already.
  (message "LeetCode fetching user data...")
  (aio-await (leetcode--fetch-user-status)))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
  (let ((problems (leetcode-problems-problems leetcode--problems))
        rows)
    (dolist (p problems (reverse rows))
      (if (or leetcode--display-paid (not (leetcode-problem-paid-only p)))
          (let* ((p-status (if (equal (leetcode-problem-status p) "ac")
                               (leetcode--add-font-lock leetcode--checkmark 'leetcode-checkmark-face)
                             " "))
                 (p-id (leetcode-problem-id p))
                 (p-title (concat
                           (leetcode-problem-title p)
                           " "
                           (if (leetcode-problem-paid-only p)
                               (leetcode--add-font-lock leetcode--paid 'leetcode-paid-face)
                             " ")))
                 (p-acceptance (leetcode-problem-acceptance p))
                 (p-difficulty (leetcode--stringify-difficulty (leetcode-problem-difficulty p)))
                 (p-tags (if leetcode--display-tags (string-join (leetcode-problem-tags p) ", ") ""))
                 (single-row (vector p-status p-id p-title p-acceptance p-difficulty p-tags)))
            (setq rows (cons single-row rows)))))))

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
          (let ((tags (s-split ", " (leetcode--row-tags row))))
            (member leetcode--filter-tag tags))
        t)
      (if leetcode--filter-difficulty
          (let ((difficulty (leetcode--row-difficulty row)))
            (string= difficulty leetcode--filter-difficulty))
        t)))
   rows))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun leetcode-reset-filter-and-refresh ()
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
  "Toggle `leetcode--display-tags` and refresh."
  (interactive)
  (setq leetcode--display-tags (not leetcode--display-tags))
  (leetcode-refresh))

(defun leetcode-toggle-paid-display ()
  "Toggle `leetcode--display-paid` and refresh."
  (interactive)
  (setq leetcode--display-paid (not leetcode--display-paid))
  (leetcode-refresh))

(defun leetcode--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
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

(aio-defun leetcode-refresh-fetch ()
  "Refresh problems and update `tabulated-list-entries'."
  (interactive)
  (message "LeetCode refreshing question list...")
  (aio-await (leetcode--fetch-question-list "all-code-essentials" 0 4000 #s(hash-table))) ; TODO pagination?
  (setq leetcode--display-tags leetcode-prefer-tag-display)
  (leetcode-reset-filter-and-refresh))

(aio-defun leetcode--ensure-login (&optional force)
  (when (or force (not (leetcode--login-p)))
    (aio-await (leetcode--login)) ; It's weird that somehow we have to login twice to be real login...
    (aio-await (leetcode--login))))

;;;###autoload(autoload 'leetcode "leetcode" nil t)
(aio-defun leetcode ()
  "Start Leetcode."
  (interactive)
  (when (leetcode--check-deps)
    (if (get-buffer leetcode--buffer-name)
        (switch-to-buffer leetcode--buffer-name)
      (aio-await (leetcode--ensure-login))
      (aio-await (leetcode-refresh-fetch))
      (switch-to-buffer leetcode--buffer-name))
    (leetcode--maybe-focus)))

;;;###autoload(autoload 'leetcode-daily "leetcode" nil t)
(aio-defun leetcode-daily ()
  "Open the daily challenge."
  (interactive)
  (aio-await (leetcode--ensure-login))
  (let* ((url-request-method "POST")
         (url-request-extra-headers `(,@(aio-await (leetcode--common-extra-headers))
                                      ,(leetcode--referer leetcode--url-login)))
         (url-request-data
          (json-encode
           `((operationName . "questionOfToday")
             (query . ,leetcode--url-daily-challenge)))))
    (with-current-buffer (url-retrieve-synchronously leetcode--url-graphql)
      (goto-char url-http-end-of-headers)
      (let-alist (json-read)
        (let ((qid .data.activeDailyCodingChallengeQuestion.question.qid))
          (leetcode-show-problem qid))))))

(aio-defun leetcode-try ()
  "Asynchronously test the code using customized testcase."
  (interactive)
  (leetcode-restore-layout)
  (aio-await (leetcode--ensure-login t))
  (let* ((title-slug (leetcode--get-slug-title (current-buffer)))
         (problem (leetcode--get-problem title-slug))
         (problem-id (leetcode-problem-id problem))
         (interpret-id (aio-await (leetcode--api-interpret-solution problem))))
    (aio-await (leetcode--api-check-submission interpret-id problem #'leetcode--show-testcases-result))))

(aio-defun leetcode-submit ()
  "Asynchronously submit the code and show result."
  (interactive)
  (leetcode-restore-layout)
  (aio-await (leetcode--ensure-login t))
  (let* ((code-buf (current-buffer))
         (code (leetcode--code-buffer-data))
         (slug-title (leetcode--get-slug-title code-buf))
         (problem (leetcode--get-problem slug-title))
         (problem-id (leetcode-problem-id problem))
         (backend-id (leetcode-problem-backend-id problem))
         (response (aio-await (leetcode--api-submit backend-id slug-title code)))
         (response-status (car response))
         (response-buffer (cdr response)))
    (if-let ((error-info (plist-get response-status :error)))
        (switch-to-buffer response-buffer)
      (let* ((resp (leetcode--parse-buffer response-buffer))
             (submission-id (number-to-string (alist-get 'submission_id resp))))
        (aio-await (leetcode--api-check-submission submission-id problem #'leetcode--show-submission-result))))))

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

(defun leetcode--display-result (buffer &optional alist)
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

(defun leetcode--display-testcase (buffer &optional alist)
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

(defun leetcode--display-detail (buffer &optional alist)
  "Display function for LeetCode detail.
BUFFER is used to show LeetCode detail. ALIST is a combined alist
specified in `display-buffer-alist'."
  (let ((window (window-top-child
                 (window-next-sibling
                  (window-left-child
                   (frame-root-window))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-code (buffer &optional alist)
  "Display function for LeetCode code.
BUFFER is the one to show LeetCode code. ALIST is a combined
alist specified in `display-buffer-alist'."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--show-testcases-result (problem-id result)
  "Show testcases RESULT by PROBLEM-ID."
  (let-alist result
    (with-current-buffer (get-buffer (leetcode--result-buffer-name problem-id))
      (erase-buffer)
      (goto-char (point-max))
      (cond
       ((eq .status_code 10)
        (if (equal .code_answer .expected_code_answer)
            (insert (leetcode--add-font-lock "PASS: " 'leetcode-accepted-face))
          (insert (leetcode--add-font-lock "FAIL: " 'leetcode-error-face)))
        (insert "\n\n")
        ;; Code Answer
        (insert "Code Answer:\n")
        (dotimes (i (length .code_answer))
          (insert (format "%s\n" (aref .code_answer i))))
        (insert "\n")
        ;; Expected
        (insert "Expected Code Answer:\n")
        (dotimes (i (length .expected_code_answer))
          (insert (format "%s\n" (aref .expected_code_answer i))))
        (insert "\n")
        ;; Std output
        (when (seq-find (lambda (s) (not (string-empty-p s))) .std_output_list)
          (insert "Std Output:\n")
          (dotimes (i (length .std_output_list))
            (when (aref .std_output_list i)
              (insert (aref .std_output_list i))))))
       ((or (eq .status_code 12) (eq .status_code 14))
        (insert (format "Status: %s\n\n"
                        (leetcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-error-face)))
        (insert (format "Test Case: \n%s\n\n" .last_testcase))
        (insert (format "Expected Answer: %s\n\n" .expected_output))
        (unless (string-empty-p .std_output)
          (insert (format "Stdout: \n%s\n" .std_output))))
       ((eq .status_code 15)
        (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face))
        (insert "\n\n")
        (insert .full_runtime_error))
       ((eq .status_code 20)
        (insert (leetcode--add-font-lock .status_msg 'leetcode-error-face))
        (insert "\n\n")
        (insert .full_compile_error))))))

(defun leetcode--show-submission-result (problem-id result)
  "Show error info in `leetcode--result-buffer-name' by PROBLEM-ID.
Error info comes from RESULT.

STATUS_CODE has following possible value:

- 10: Accepted
- 11: Wrong Anwser
- 12: Memory Limit Exceeded
- 13: Output Limit Exceeded
- 14: Time Limit Exceeded
- 15: Runtime Error.  full_runtime_error
- 20: Compile Error.  full_compile_error"
  (let-alist result
    (with-current-buffer (get-buffer-create (leetcode--result-buffer-name problem-id))
      (erase-buffer)
      (font-lock-mode +1)
      (cond
       ((eq .status_code 10)
        (insert (format "Status: %s\n\n"
                        (leetcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-accepted-face)))
        (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n"
                        .status_runtime .runtime_percentile .pretty_lang))
        (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions."
                        .status_memory .memory_percentile .pretty_lang)))
       ((eq .status_code 11)
        (insert (format "Status: %s\n\n"
                        (leetcode--add-font-lock
                         (format "%s (%s/%s)" .status_msg .total_correct .total_testcases)
                         'leetcode-error-face)))
        (insert (format "Test Case: \n%s\n\n" .input))
        (insert (format "Answer: %s\n\n" .code_output))
        (insert (format "Expected Answer: %s\n\n" .expected_output))
        (unless (string-empty-p .std_output)
          (insert (format "Stdout: \n%s\n" .std_output))))
       ((eq .status_code 12)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase)))
       ((eq .status_code 13)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face))))
       ((eq .status_code 14)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase)))
       ((eq .status_code 15)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert (format "\n\n%s / %s testcases passed\n" .total_correct .total_testcases))
        (insert (format "Last Test Case: %s\n" .last_testcase))
        (insert "\n\n")
        (insert (format .full_runtime_error)))
       ((eq .status_code 20)
        (insert (format "Status: %s" (leetcode--add-font-lock .status_msg 'leetcode-error-face)))
        (insert "\n\n")
        (insert (format .full_compile_error))))
      (display-buffer (current-buffer)
                      '((display-buffer-reuse-window
                         leetcode--display-result)
                        (reusable-frames . visible))))))

(defun leetcode--show-problem (problem)
  "Show the detail of PROBLEM.
Use `shr-render-buffer' to render problem detail. This action
will show the detail in other window and jump to it."
  (let* ((problem-id (leetcode-problem-id problem))
         (title (leetcode-problem-title problem))
         (difficulty (leetcode-problem-difficulty problem))
         (likes (leetcode-problem-likes problem))
         (dislikes (leetcode-problem-dislikes problem))
         (content (leetcode-problem-content problem))
         (buf-name (leetcode--detail-buffer-name problem-id))
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (leetcode--debug "select title: %s" title)
    (leetcode--maybe-focus)
    ;; Kill defail buffer if exists, we'll re-create a new one.
    (when (get-buffer buf-name) (kill-buffer buf-name))
    ;; Render question with `shr'.
    (with-temp-buffer
      (insert (concat "<h1>" problem-id ". " title "</h1>"))
      (insert (concat (capitalize difficulty) html-margin
                      "likes: " (number-to-string likes) html-margin
                      "dislikes: " (number-to-string dislikes)))
      ;; Sometimes LeetCode don't have a '<p>' at the outermost...
      (insert "<p>" content "</p>")
      (leetcode--replace-in-buffer "" "")
      ;; NOTE: shr.el can't render "https://xxxx.png", so we use "http"
      (leetcode--replace-in-buffer "https" "http")
      (shr-render-buffer (current-buffer)))

    ;; `shr-render-buffer' will put the result in buffer *html*.
    (with-current-buffer "*html*"
      (save-match-data
        (re-search-forward "dislikes: .*" nil t)
        (insert (make-string 4 ?\s))
        (insert-text-button "Solve it"
                            'action (lambda (btn) (leetcode--start-coding problem))
                            'help-echo "Solve the problem.")
        (insert (make-string 4 ?\s))
        (insert-text-button "Link"
                            'action (lambda (btn) (browse-url (leetcode--problem-link title)))
                            'help-echo "Open the problem in browser.")
        (insert (make-string 4 ?\s))
        (insert-text-button "Solution"
                            'action (lambda (btn) (browse-url (concat (leetcode--problem-link title) "/solution")))
                            'help-echo "Open the problem solution page in browser."))
      (rename-buffer buf-name)
      (leetcode--problem-detail-mode)
      (switch-to-buffer (current-buffer))
      (search-backward "Solve it"))))

(aio-defun leetcode-show-problem (problem-id)
  "Show the detail of problem with id PROBLEM-ID.
Get problem by id and use `shr-render-buffer' to render problem
detail. This action will show the detail in other window and jump
to it."
  (interactive (list (read-string "Show problem by problem id: "
                                  (when (derived-mode-p 'leetcode--problems-mode)
                                    (leetcode--get-current-problem-id)))))
  (let* ((problem (leetcode--get-problem-by-id problem-id))
         (title-slug (leetcode-problem-title-slug problem))
         (problem-with-title (aio-await (leetcode--ensure-question-title problem)))
         (problem-with-content (aio-await (leetcode--ensure-question-content problem)))
         (problem-with-testcases (aio-await (leetcode--ensure-question-testcases problem)))
         (problem-with-snippets (aio-await (leetcode--ensure-question-snippets problem))))
    (leetcode--show-problem problem-with-snippets)))

(defun leetcode-show-problem-by-slug (slug-title)
  "Show the detail of problem with SLUG-TITLE.
This function will work after first run
\\[execute-extended-command] leetcode. Get problem by id and use
`shr-render-buffer' to render problem detail. This action will
show the detail in other window and jump to it.

It can be used in org-link elisp:(leetcode-show-problem-by-slug \"3sum\")."
  (interactive (list (read-number "Show problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem slug-title))
         (problem-id (leetcode-problem-id problem)))
    (leetcode-show-problem problem-id)))

(defun leetcode-show-current-problem ()
  "Show current problem's detail.
Call `leetcode-show-problem' on the current problem id. This
action will show the detail in other window and jump to it."
  (interactive)
  (leetcode-show-problem (leetcode--get-current-problem-id)))

(aio-defun leetcode-view-problem (problem-id)
  "View problem by PROBLEM-ID while staying in `LC Problems' window.
Similar with `leetcode-show-problem', but instead of jumping to
the detail window, this action will jump back in `LC Problems'."
  (interactive (list (read-string "View problem by problem id: "
                                  (leetcode--get-current-problem-id))))
  (aio-await (leetcode-show-problem problem-id))
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

(aio-defun leetcode-solve-problem (problem-id)
  "Start coding the problem with id PROBLEM-ID."
  (interactive (list (read-number "Solve the problem with id: "
                                  (leetcode--get-current-problem-id))))
  (let* ((problem (leetcode--get-problem-by-id problem-id)))
    (leetcode--show-problem problem)
    (leetcode--start-coding problem)))

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
  (when buf
    (delete-windows-on buf t)
    (kill-buffer buf)))

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
        leetcode--problem-titles)
  (setq leetcode--problem-titles '()))

(defun leetcode--set-lang (snippets)
  "Set `leetcode--lang' based on langSlug in SNIPPETS."
  (setq leetcode--lang
        ;; if there is a mysql snippet, we use `leetcode-prefer-sql'.
        (if (seq-find (lambda (s)
                        (equal (leetcode-snippet-lang-slug s)
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
        (format "%s_%s" (leetcode--get-problem-id slug-title) title-with-suffix)
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
              (equal slug-title (leetcode-problem-title-slug p)))
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
  (aref (tabulated-list-get-entry) 1))

(defun leetcode--start-coding (problem)
  "Create a buffer for coding PROBLEM.
The buffer will be not associated with any file.  It will choose
major mode by `leetcode-prefer-language'and `auto-mode-alist'."
  (let* ((title (leetcode-problem-title problem))
         (slug-title (leetcode-problem-title-slug problem))
         (problem-id (leetcode-problem-id problem))
         (snippets (leetcode-problem-snippets problem))
         (testcases (leetcode-problem-testcases problem))
         (testcase-buf-name (leetcode--testcase-buffer-name problem-id))
         (result-buf-name (leetcode--result-buffer-name problem-id)))

    ;; Record windows opened for later cleanup.
    (unless (member title leetcode--problem-titles)
      (push title leetcode--problem-titles))

    (leetcode--solving-window-layout)

    ;; Set current programming language.
    (leetcode--set-lang snippets)

    ;; Setup code buffer
    (let* ((code-buf-name (leetcode--get-code-buffer-name title))
           (code-buf (leetcode--get-code-buffer code-buf-name))
           (suffix (assoc-default leetcode--lang leetcode--lang-suffixes)))
      (with-current-buffer code-buf
        (when (= (buffer-size code-buf) 0)
          (let* ((snippet (seq-find (lambda (s)
                                      (equal (leetcode-snippet-lang-slug s) leetcode--lang))
                                    snippets))
                 (template-code (leetcode-snippet-code snippet)))
            (leetcode--insert-code-start-marker)
            (insert template-code)
            (leetcode--insert-code-end-marker)
            (leetcode--replace-in-buffer "" "")))
        (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
        (leetcode-solution-mode t))

      (display-buffer code-buf
                      '((display-buffer-reuse-window
                         leetcode--display-code)
                        (reusable-frames . visible))))

    ;; Setup testcase buffer
    (with-current-buffer (get-buffer-create testcase-buf-name)
      (erase-buffer)
      (insert (s-join "\n" testcases))
      (set-window-buffer leetcode--testcase-window (current-buffer)))
    (with-current-buffer (get-buffer-create result-buf-name)
      (erase-buffer)
      (set-window-buffer leetcode--result-window (current-buffer)))))

(aio-defun leetcode-restore-layout ()
  "This command should be run in LeetCode code buffer.
It will restore the layout based on current buffer's name."
  (interactive)
  (let* ((slug-title (leetcode--get-slug-title (current-buffer)))
         (problem (leetcode--get-problem slug-title))
         (problem-id (leetcode-problem-id problem))
         (desc-buf (get-buffer (leetcode--detail-buffer-name problem-id)))
         (testcase-buf (get-buffer-create (leetcode--testcase-buffer-name problem-id)))
         (result-buf (get-buffer-create (leetcode--result-buffer-name problem-id))))
    (leetcode--solving-window-layout)
    (unless desc-buf
      (aio-await (leetcode-show-problem problem-id)))
    (with-current-buffer result-buf
      (erase-buffer)
      (insert "Waiting for result..."))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problems Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (define-key map "s" #'leetcode-set-filter-regex)
      (define-key map "L" #'leetcode-set-prefer-language)
      (define-key map "t" #'leetcode-set-filter-tag)
      (define-key map "T" #'leetcode-toggle-tag-display)
      (define-key map "P" #'leetcode-toggle-paid-display)
      (define-key map "d" #'leetcode-set-filter-difficulty)
      (define-key map "g" #'leetcode-refresh)
      (define-key map "G" #'leetcode-refresh-fetch)
      (define-key map "r" #'leetcode-reset-filter-and-refresh)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problems-mode'.")

(define-derived-mode leetcode--problems-mode
  tabulated-list-mode "LC Problems"
  "Major mode for browsing a list of problems."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'leetcode-refresh nil t)
  :group 'leetcode
  :keymap leetcode--problems-mode-map)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Detail Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode--problem-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problem-detail-mode'.")

(define-derived-mode leetcode--problem-detail-mode
  special-mode "LC Detail"
  "Major mode for display problem detail."
  :group 'leetcode
  :keymap leetcode--problem-detail-mode-map)

(add-hook 'leetcode--problem-detail-mode-hook
          (lambda () (leetcode--set-evil-local-map leetcode--problem-detail-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Solution Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leetcode-solution-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-t") #'leetcode-try)
      (define-key map (kbd "C-c C-s") #'leetcode-submit)
      (define-key map (kbd "C-c C-r") #'leetcode-restore-layout)))
  "Keymap for `leetcode-solution-mode'.")

(define-minor-mode leetcode-solution-mode
  "Minor mode to provide shortcut and hooks."
  :require 'leetcode
  :lighter " LC-Solution"
  :group 'leetcode
  :keymap leetcode-solution-mode-map)

(provide 'leetcode)
;;; leetcode.el ends here
