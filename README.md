[![MELPA](https://melpa.org/packages/leetcode-badge.svg)](https://melpa.org/#/leetcode)
# Introduction

LeetCode brings you offer, and now Emacs brings you LeetCode!

# Installation

You can `package-install` it from melpa directly.

## Manually

1. Clone this repository and install all dependencies
2. Move it to your load-path
3. Require it in your emacs config

If you use [spacemacs](https://github.com/syl20bnr/spacemacs), there is a [leetcode-emacs-layer](https://github.com/anmoljagetia/leetcode-emacs-layer). Thanks for [Anmol Jagetia](https://github.com/anmoljagetia)!

# Configuration

You can set your preferred LeetCode programming language and SQL by setting `leetcode-prefer-language` and `leetcode-prefer-sql`:

```elisp
(setq leetcode-prefer-language "python3")
(setq leetcode-prefer-sql "mysql")
```

All supported languages can be found in variable `leetcode--prefer-language-suffixes`.

# Usage

1. Execute `leetcode` command, then Emacs will prompt you to input account and password. If login successful, Emacs will save it into a file. If you are interested in what happend here, you can check [auth-source.el](https://www.gnu.org/software/emacs/manual/html_mono/auth.html).

![leetcode](images/leetcode.png)

In leetcode problems list buffer:

| keymap | command                          |
|--------|----------------------------------|
| n      | cursor move down                 |
| p      | cursor move up                   |
| RET    | show current problem description |

2. Press `<RET>`, show problem description, move cursor to "solve it", press `<RET>` again, start coding!

3. After finishing your code, you can edit testcase and execute `leetcode-try` or execute `leetcode-submit`.

![leetcode-submit](images/leetcode-submit.png)

# Debug

If you are unable to start Leetcode, set these variables and try again to see a full stacktrace:

```elisp
(setq url-debug t)
```

# Contributing

This package use [Cask](https://cask.readthedocs.io/en/latest/guide/introduction.html) to develop, build and test.

It is a suggestion for you to use `Cask`, but if you don't want to bother to use it, it's totally fine too.

`Cask` is a build tools for emacs lisp, you can think it of `npm` for emacs lisp.

Enter project root, execute `cask install`, this command will install all dependencies. After that, execute `cask emacs` which will start a emacs with extra `load-path` to load dependencies.
