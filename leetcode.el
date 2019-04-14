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
(require 'shr)

(require 'dash)
(require 'request)
(require 'request-deferred)             ; Asynchronous HTTP request
(require 'graphql)                      ; Some requests of LeetCode use GraphQL

(require 'spinner)


(defgroup leetcode nil
  "A Leetcode client."
  :prefix 'leetcode-
  :group 'tools)

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
:hard     Number")

(defvar leetcode--problems nil
  "Problems info with a list of problem object.
:num      Number
:tag      String
:problems List

    :status     String
    :id         Number
    :pos        Number
    :title      String
    :acceptance String
    :difficulty Number {1,2,3}
    :paid-only  Boolean {t|nil}")

(defvar leetcode-checkmark "✓" "Checkmark for accepted problem.")
(defconst leetcode--buffer-name          "*leetcode*")
(defconst leetcode--descr-buffer-name    "*leetcode-description*")
(defconst leetcode--testcase-buffer-name "*leetcode-testcase*")
(defconst leetcode--result-buffer-name   "*leetcode-result*")

(defface leetcode-checkmark-face
  '((t (:foreground "#5CB85C")))
  "Face for `leetcode-checkmark'"
  :group 'leetcode)

(defface leetcode-easy-face
  '((t (:foreground "#5CB85C")))
  "Face for easy problem."
  :group 'leetcode)

(defface leetcode-medium-face
  '((t (:foreground "#F0AD4E")))
  "Face for medium problem."
  :group 'leetcode)

(defface leetcode-hard-face
  '((t (:foreground "#D9534E")))
  "Face for hard problem."
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
  "HTTP Referer Header."
  (cons "Referer" value))

(defun leetcode--csrf-token ()
  (let ((token (assoc-default
                "csrftoken"
                (request-cookie-alist leetcode--domain "/" t))))
    (or token
        (progn
          (request leetcode--url-login :sync t)
          (leetcode--csrf-token)))))

(defun leetcode--login (account password)
  "Send login request and return a deferred object."
  (when (or (string-empty-p account) (string-empty-p password))
    (setq account (read-string "account: "))
    (setq password (read-string "password: ")))
  (leetcode--global-loading-mode t)
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
   (cl-function (lambda (&key data &allow-other-keys)
                  (leetcode--global-loading-mode -1)))
   :error
   (cl-function
    (lambda (&rest args &key error-thrown &allow-other-keys)
      (leetcode--global-loading-mode -1)
      (message "Login failed: %S" error-thrown)))))

(defun leetcode--login-p ()
  (let ((username (plist-get leetcode--user :username)))
    (and username
         (not (string-empty-p username))
         (assoc-default
          "LEETCODE_SESSION"
          (request-cookie-alist
           (concat "." leetcode--domain) "/" t)))))

(defun leetcode--set-user-and-problems (response)
  "Set `leetcode--user' and `leetcode--problems', if user isn't
 login, only `leetcode--problems' will be set."
  (let ((data (request-response-data response)))
    ;; user
    (setq leetcode--user (plist-put leetcode--user :username (alist-get 'user_name data)))
    (setq leetcode--user (plist-put leetcode--user :solved (alist-get 'num_solved data)))
    (setq leetcode--user (plist-put leetcode--user :easy (alist-get 'ac_easy data)))
    (setq leetcode--user (plist-put leetcode--user :medium (alist-get 'ac_medium data)))
    (setq leetcode--user (plist-put leetcode--user :hard (alist-get 'ac_hard data)))
    ;; problem list
    (setq leetcode--problems (plist-put leetcode--problems :num (alist-get 'num_total data)))
    (setq leetcode--problems (plist-put leetcode--problems :tag "all"))
    (setq leetcode--problems
          (plist-put leetcode--problems :problems
                     (let ((raw-vec (alist-get 'stat_status_pairs data))
                           (len (plist-get leetcode--problems :num))
                           problems)
                       (dolist (i (number-sequence 0 (1- len)))
                         (let* ((cur (aref raw-vec i))
                                (stat (alist-get 'stat cur))
                                (status (alist-get 'status cur))
                                (difficulty (alist-get 'level (alist-get 'difficulty cur)))
                                (paid-only (eq (alist-get 'paid_only cur) t))
                                (question-id (alist-get 'question_id stat))
                                (total-submitted (alist-get 'total_submitted stat))
                                (total-acs (alist-get 'total_acs stat)))
                           (push
                            (list
                             :status status
                             :id question-id
                             :pos (- len i)
                             :title (alist-get 'question__title stat)
                             :acceptance (format "%.1f%%" (* 100 (/ (float total-acs) total-submitted)))
                             :difficulty difficulty
                             :paid-only paid-only)
                            problems)))
                       problems)))))

(defun leetcode--fetch-user-and-problems ()
  (request-deferred
   leetcode--api-all-problems
   :headers `(,leetcode--User-Agent
              ,leetcode--X-Requested-With
              ,(leetcode--referer leetcode--url-login))
   :parser 'json-read))

(defun leetcode--slugify-title (title)
  (let* ((str1 (replace-regexp-in-string "\s+" "-" (downcase title)))
         (str2 (replace-regexp-in-string "(" "" str1))
         (str3 (replace-regexp-in-string ")" "" str2))
         (res (replace-regexp-in-string "," "" str3)))
    res))

(defun leetcode--problem-descr-graphql-params (opration &optional vars)
  (list
   (cons "operationName" "questionData")
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
:likes     Number
:dislikes  Number
:content   String
:topicTags String
"
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
  "Replace string in `current-buffer'."
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
    (vconcat
     (-zip-with
      (lambda (col size) (list col size nil))
      column-names widths))))

(defun leetcode--problems-rows ()
  "Generate tabulated list rows from `leetcode--problems',
([<checkmark> <position> <acceptance> <difficulty>] ...)"
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
  (leetcode--global-loading-mode t)
  (deferred:$
    (leetcode--fetch-user-and-problems)
    (deferred:nextc it
      (lambda (response) (leetcode--set-user-and-problems response)))
    (deferred:nextc it
      (lambda ()
        (let* ((column-names '(" " "#" "Problem" "Acceptance" "Difficulty"))
               (rows (leetcode--problems-rows))
               (headers (leetcode--make-tabulated-headers column-names rows)))
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
            (leetcode--global-loading-mode -1)))))))

;;;###autoload
(defun leetcode ()
  "Show leetcode problems buffer."
  (interactive)
  (if (get-buffer leetcode--buffer-name)
      (switch-to-buffer leetcode--buffer-name)
    (if (leetcode--login-p)
        (deferred:$
          (deferred:nextc (leetcode-problems-refresh)
            (lambda ()
              (switch-to-buffer leetcode--buffer-name))))
      (deferred:$
        (leetcode--login leetcode-account leetcode-password)
        (deferred:nextc it
          (lambda ()
            (deferred:nextc (leetcode-problems-refresh)
              (lambda ()
                (switch-to-buffer leetcode--buffer-name)))))))))

(defun leetcode--buffer-content (buf)
  (with-current-buffer buf
    (buffer-substring-no-properties
     (point-min) (point-max))))

;; TODO
(defun leetcode-try (title question-id)
  "Test the code using customized testcase."
  (let ((testcase-buf (get-buffer leetcode--testcase-buffer-name))
        (code-buf (get-buffer (leetcode--get-code-buffer-name title))))
    (if (and testcase-buf code-buf)
        (request-deferred
         (format leetcode--api-try (leetcode--slugify-title title))
         :headers `(leetcode--User-Agent
                    ("Content-Type" . "application/json")
                    ("Referer" . ,(leetcode--referer (format leetcode--api-problems-submission title)))
                    (leetcode--X-CSRFToken . ,(leetcode--csrf-token)))
         :data (json-encode `((data_input . ,(leetcode--buffer-content testcase-buf))
                              (judge_type . "small")
                              (lang . ,leetcode-prefer-language)
                              (question_id . ,question-id)
                              (typed_code . ,(leetcode--buffer-content code-buf)))))
      (throw 'no-buffer "No testcase buffer and code buffer."))))

(defun leetcode--check-submission (submission-id slug-title)
  (request
   (format leetcode--api-check-submission submission-id)
   :type "POST"
   :headers `(,leetcode--User-Agent
              ,(leetcode--referer (format leetcode--api-problems-submission slug-title))
              ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
   :parser 'json-read :sync t))

(defun leetcode--solving-layout ()
  "Specify layout for solving problem.
+---------------+----------------+
|               |                |
|               |                |
|               |  Description   |
|               |                |
|               |                |
|     Code      +----------------+
|               |   Customize    |
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
  (let ((window (window-next-sibling
                 (window-next-sibling
                  (window-top-child
                   (window-next-sibling
                    (window-left-child
                     (frame-root-window))))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-testcase (buffer &optional alist)
  (let ((window (window-next-sibling
                 (window-top-child
                  (window-next-sibling
                   (window-left-child
                    (frame-root-window)))))))
    (set-window-buffer window buffer)
    window))

(defun leetcode--display-code (buffer &optional alist)
  "Display code buffer in the left."
  (let ((window (window-left-child (frame-root-window))))
    (set-window-buffer window buffer)
    window))

(defun leetcode-submit ()
  "Submit the code and popup a buffer to show result."
  (interactive)
  (let* ((code (leetcode--buffer-content (current-buffer)))
         (slug-title (with-current-buffer (current-buffer)
                       (file-name-base (buffer-name))))
         (id (catch 'break
               (dolist (p (plist-get leetcode--problems :problems))
                 (when (equal slug-title (leetcode--slugify-title (plist-get p :title)))
                   (throw 'break (plist-get p :id)))))))
    (leetcode--loading-mode t)
    (deferred:$
      (request-deferred
       (format leetcode--api-submit slug-title)
       :type "POST"
       :headers `(,leetcode--User-Agent
                  ,(leetcode--referer (format leetcode--api-problems-submission slug-title))
                  ,(cons "Content-Type" "application/json")
                  ,(cons leetcode--X-CSRFToken (leetcode--csrf-token)))
       :data (json-encode `((lang . ,leetcode-prefer-language)
                            (question_id . ,id)
                            (typed_code . ,code)))
       :parser 'json-read)
      (deferred:nextc it
        (lambda (resp)
          (alist-get 'submission_id (request-response-data resp))))
      (deferred:nextc it
        (lambda (submission-id)
          (with-local-quit
            (let* ((resp (leetcode--check-submission submission-id slug-title))
                   (data (request-response-data resp)))
              (while (not (equal (alist-get 'state data) "SUCCESS"))
                ;; poll until the state is SUCCESS
                (sleep-for 0.5)
                (setq resp (leetcode--check-submission submission-id slug-title))
                (setq data (request-response-data resp)))
              data))))
      (deferred:nextc it
        (lambda (res)
          (let ((submission-id (alist-get 'submission_id res))
                (runtime (alist-get 'status_runtime res))
                (memory (alist-get 'status_memory res))
                (runtime-perc (alist-get 'runtime_percentile res))
                (memory-perc (alist-get 'memory_percentile res))
                (total-correct (alist-get 'total_correct res))
                (total-testcases (alist-get 'total_testcase res))
                (status-msg (alist-get 'status_msg res))
                (lang (alist-get 'pretty_lang res)))
            (with-current-buffer (get-buffer-create leetcode--result-buffer-name)
              (erase-buffer)
              (insert (format "Status: %s\n\n" status-msg))
              (when (equal status-msg "Accepted")
                (insert (format "Runtime: %s, faster than %.2f%% of %s submissions.\n\n" runtime runtime-perc lang))
                (insert (format "Memory Usage: %s, less than %.2f%% of %s submissions." memory memory-perc lang)))
              (display-buffer (current-buffer)
                              '((display-buffer-reuse-window
                                 leetcode--display-result)
                                (reusable-frames . visible)))
              (leetcode--loading-mode -1))))))))

(defun leetcode-show-descri ()
  "Show current entry problem description. Get current entry by
using `tabulated-list-get-entry' and use `shr-render-buffer' to
render problem description."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (pos (aref entry 1))
         (title (aref entry 2))
         (difficulty (aref entry 4))
         (problem (leetcode--parse-problem title))
         (content (alist-get 'content problem))
         (dislikes (alist-get 'dislikes problem))
         (likes (alist-get 'likes problem))
         (snippets (alist-get 'codeSnippets problem))
         (testcase (alist-get 'sampleTestCase problem))
         (buf-name leetcode--descr-buffer-name)
         (html-margin "&nbsp;&nbsp;&nbsp;&nbsp;"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (with-temp-buffer
      (insert (concat "<h1>" pos ". " title "</h1>"))
      (insert (concat (capitalize difficulty) html-margin
                      "likes: " (number-to-string likes) html-margin
                      "dislikes: " (number-to-string dislikes)))
      (insert (alist-get 'content problem))
      (setq shr-current-font t)
      (leetcode--replace-in-buffer "" "")
      ;; NOTE: shr.el can't render "https://xxxx.png", now we use "http"
      (leetcode--replace-in-buffer "https" "http")
      (shr-render-buffer (current-buffer)))
    (with-current-buffer "*html*"
      (save-match-data
        (re-search-forward "dislikes: .*" nil t)
        (insert (make-string 4 ?\s))
        (insert-text-button "solve it"
                            'action (lambda (btn)
                                      (leetcode--start-coding title (append snippets nil) testcase))
                            'help-echo "solve the problem."))
      (rename-buffer buf-name)
      (leetcode--problem-description-mode)
      (switch-to-buffer (current-buffer)))))

(defvar leetcode-prefer-language "python3"
  "LeetCode programming language: c, cpp, csharp, golang, java,
  javascript, kotlin, php, python, python3, ruby, rust, scala,
  swift")

(defconst leetcode--prefer-language-suffixes
  '(("c" . ".c") ("cpp" . ".cpp") ("csharp" . ".cs")
    ("golang" . ".go") ("java" . ".java") ("javascript" . ".js")
    ("kotlin" . ".kt") ("php" . ".php") ("python" . ".py")
    ("python3" . ".py") ("ruby" . ".rb") ("rust" . ".rs")
    ("scala" . ".scala") ("swift" . ".swift"))
  "c, cpp, csharp, golang, java, javascript, kotlin, php, python,
  python3, ruby, rust, scala, swift")

(defun leetcode--get-code-buffer-name (title)
  (let ((suffix (assoc-default
                 leetcode-prefer-language
                 leetcode--prefer-language-suffixes)))
    (concat (leetcode--slugify-title title) suffix)))

(defun leetcode--start-coding (title snippets testcase)
  "Create a buffer which is not associated with any file for
coding. It will choose major mode by
`leetcode-prefer-language'and `auto-mode-alist'."
  (leetcode--solving-layout)
  (let ((code-buf (get-buffer (leetcode--get-code-buffer-name title)))
        (suffix (assoc-default
                 leetcode-prefer-language
                 leetcode--prefer-language-suffixes)))
    (unless code-buf
      (with-current-buffer (get-buffer-create (leetcode--get-code-buffer-name title))
        (setq code-buf (current-buffer))
        (funcall (assoc-default suffix auto-mode-alist #'string-match-p))
        (catch 'break
          (dolist (s snippets)
            (when (equal (alist-get 'langSlug s) leetcode-prefer-language)
              (insert (alist-get 'code s))
              (throw 'break "Found target snippet."))))))
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
    (display-buffer (current-buffer)
                    '((display-buffer-reuse-window
                       leetcode--display-result)
                      (reusable-frames . visible)))))

(defvar leetcode--problems-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'leetcode-show-descri)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "g" #'leetcode-problems-refresh)
      (define-key map "q" #'quit-window)))
  "Keymap for `leetcode--problems-mode'")

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
      (define-key map "p" #'previous-line))))

(define-derived-mode leetcode--problem-description-mode
  special-mode "LC Descri"
  "Major mode for display problem description."
  :group 'leetcode)

;;; Use spinner.el to show progress indicator
(defvar leetcode--spinner (spinner-create 'progress-bar-filled))
(defconst leetcode--loading-lighter
  '(" [LeetCode" (:eval (spinner-print leetcode--spinner)) "]"))

(define-minor-mode leetcode--loading-mode
  "Minor mode to showing leetcode loading status."
  :lighter leetcode--loading-lighter
  :group 'leetcode
  (if leetcode--loading-mode
      (spinner-start leetcode--spinner)
    (spinner-stop leetcode--spinner)))

(defun leetcode--turn-on-loading-mode ()
  (leetcode--loading-mode t))

(define-global-minor-mode leetcode--global-loading-mode
  leetcode--loading-mode leetcode--turn-on-loading-mode
  :group 'leetcode)

(provide 'leetcode)
;;; leetcode.el ends here
