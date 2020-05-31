[![MELPA](https://melpa.org/packages/leetcode-badge.svg)](https://melpa.org/#/leetcode)
# Introduction

LeetCode brings you offer, and now Emacs brings you LeetCode!

# Installation

- Vanilla Emacs: `package-install` it from melpa directly
- [Spacemacs](https://github.com/syl20bnr/spacemacs): [leetcode-emacs-layer](https://github.com/anmoljagetia/leetcode-emacs-layer)

LeetCode do not allow third party login, one workaround is restore LeetCode session from local Chrome cookies. By default, this package will install a Python3 package called [my\_cookies](https://github.com/kaiwk/my_cookies), or you can install it manually: `pip3 install my_cookies`.

## Manually

1. Clone this repository and install all dependencies
2. Move it to your load-path
3. Require it in your emacs config

# Configuration

You can set your preferred LeetCode programming language and SQL by setting `leetcode-prefer-language` and `leetcode-prefer-sql`:

```elisp
(setq leetcode-prefer-language "python3")
(setq leetcode-prefer-sql "mysql")
```

All supported languages can be found in variable `leetcode--prefer-language-suffixes`.

You can save solution by setting `leetcode-save-solutions`:

```elisp
(setq leetcode-save-solutions t)
(setq leetcode-directory "~/leetcode")
```

# Usage

1. Execute `leetcode` command.

![leetcode](images/leetcode.png)

In leetcode problems list buffer:

| keymap | command                                |
|--------|----------------------------------------|
| n      | cursor move down                       |
| p      | cursor move up                         |
| s      | filter problem by regex                |
| t      | filter problem by tag                  |
| /      | clear filters                          |
| g      | refresh without fetching from LeetCode |
| G      | refresh all data                       |
| RET    | show current problem description       |

2. Press `<RET>`, show problem description, move cursor to "solve it", press `<RET>` again, start coding!

3. After finishing your code, you can edit testcase and execute `leetcode-try` or execute `leetcode-submit`.

![leetcode-submit](images/leetcode-submit.png)

# Debug

Call `leetcode-toggle-debug`, log will output in `*leetcode-log*` buffer.

# Contribution

Please submit PR to develop branch.
