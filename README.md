[![MELPA](https://melpa.org/packages/leetcode-badge.svg)](https://melpa.org/#/leetcode)
# Introduction

LeetCode brings you offer, and now Emacs brings you LeetCode!

# Usage

![screencast](images/screencast.gif)

1. Execute `leetcode` command, and in problem list buffer:

| Keymap              | Description                              |
|---------------------|------------------------------------------|
| o                   | show current problem                     |
| O                   | show problem by prompting problem id     |
| v                   | view the current problem                 |
| V                   | view a problem by prompting problem id   |
| b                   | show the current problem in browser      |
| B                   | show a problem by  problem id in browser |
| c                   | start coding the current problem         |
| C                   | start coding a problem by problem id     |
| s                   | filter problems by regex                 |
| t                   | filter problems by tag                   |
| T                   | toggle tag display                       |
| d                   | filter problems by difficulty            |
| r                   | reset filters                            |
| P                   | toggle paid problems display             |
| g (z for evil-mode) | refresh without fetching from LeetCode   |
| G (Z for evil-mode) | refresh all problems                     |
| L                   | change prefer language                   |
| RET                 | show current problem                     |
| TAB                 | view current problem                     |

2. Press `<RET>`, show problem detail, move cursor to "solve it", press `<RET>` again, start coding!

3. After finishing your code, you can edit testcase and execute `leetcode-try` or execute `leetcode-submit`.

![leetcode-submit](images/leetcode-submit.png)


In `leetcode-solution-mode`, you will have:

| Keymap  | Description            |
|---------|------------------------|
| C-c C-t | run code with testcase |
| C-c C-s | submit                 |
| C-c C-r | restore window layout  |

You can also disable IDE-like features by adding hook to `leetcode-solution-mode-hook`:

``` elisp
(add-hook 'leetcode-solution-mode-hook
          (lambda() (flycheck-mode -1)))
```

# Installation

- Vanilla Emacs: `package-install` it from melpa directly
- [Spacemacs](https://github.com/syl20bnr/spacemacs):
  [leetcode-emacs-layer](https://github.com/anmoljagetia/leetcode-emacs-layer)

LeetCode do not allow third party login, one workaround is restore LeetCode session from local Firefox or Chrome cookies. By default, this package will install a Python3 package called [my\_cookies](https://github.com/kaiwk/my_cookies), or you can install it manually: `pip3 install my_cookies`.

Since we are using [shr.el](https://www.emacswiki.org/emacs/HtmlRendering) to render HTML, you may also want to look at [shrface](https://github.com/chenyanming/shrface).

## Manually

1. Clone this repository and install all dependencies
2. Move it to your load-path
3. Require it in your emacs config

# Configuration

You can set your preferred LeetCode programming language and SQL by setting `leetcode-prefer-language` and `leetcode-prefer-sql`:

If you prefer not to see problems' tags in the `*leetcode**`buffer by default. set `leetcode-prefer-tag-display` to nil

```elisp
(setq leetcode-prefer-language "python3")
(setq leetcode-prefer-sql "mysql")
```

All supported languages can be found in variable `leetcode--lang-suffixes`.

You can save solution by setting `leetcode-save-solutions`:

```elisp
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")
```

# Work with Org Mode

`leetcode-show-problem-by-slug` will let you put to org files with a link in this format to show the question after the *leetcode* buffer is load like [elisp:(leetcode-show-problem-by-slug (leetcode--slugify-title "ZigZag Conversion"))]

# Debug

Call `leetcode-toggle-debug`, log will output in `*leetcode-log*` buffer.

# Contributing

Please submit PR to develop branch.

