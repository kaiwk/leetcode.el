[![MELPA](https://melpa.org/packages/leetcode-badge.svg)](https://melpa.org/#/leetcode)
# Introduction 

LeetCode brings you offer, and now Emacs brings you LeetCode!

# Dependencies

## Emacs Library

- dash.el
- request.el
- request-deferred.el
- graphql.el
- spinner.el

## External software

- cURL

# Installation

1. Clone this repository
2. Move it to your load-path
3. Require it in your emacs config

If you use [spacemacs](https://github.com/syl20bnr/spacemacs), there is a [leetcode-emacs-layer](https://github.com/anmoljagetia/leetcode-emacs-layer). Thanks for [Anmol Jagetia](https://github.com/anmoljagetia)!

# Configuration

You can choose to set your LeetCode account and password like this:

```elisp
(setq leetcode-account "your-account")
(setq leetcode-password "your-password")
```

Put password as plain text into your emacs config may not be a good idea, you can either read your password from somewhere else, or you can ignore this setting completely, a prompt will be given everytime you enter `M-x leetcode`.

You can also set your preferred LeetCode programming language by setting
`leetcode-prefer-language`:

```elisp
(setq leetcode-prefer-language "python3")
```

All supported languages can be found in `leetcode--prefer-language-suffixes` variable.

# Usage

1.  Execute `leetcode` command

![leetcode](images/leetcode.png)

In leetcode problems list buffer:

| keymap | command                          |
|--------|----------------------------------|
| n      | cursor move down                 |
| p      | cursor move up                   |
| RET    | show current problem description |

2. Press `<RET>`, show problem description, move cursor to 'solve it', press
   `<RET>` again, start coding!

3. After finishing your code, you can edit testcase and execute `leetcode-try` or execute `leetcode-submit`.

![leetcode-submit](images/leetcode-submit.png)

# Debug

If you are unable to start Leetcode, set these variables and try again to see a full stacktrace:

```elisp
(setq request-message-level 'debug)
(setq request-log-level 'debug)
```

## Windows

If you are using Windows, it is possible that `curl` executable comes from `Windows\System32\curl.exe` (you can check it by running `where curl` in the command line), which can give the following error:

```bash
curl: option --compressed: the installed libcurl version doesn't support this
```

To solve this error, it is suggested to use `curl` provided by Git. Add `<path to Git>\mingw64\bin` to the `Path`.

# Contributing

This package use [Cask](https://cask.readthedocs.io/en/latest/guide/introduction.html) to develop, build and test.

It is a suggestion for you to use `Cask`, but if you don't want to bother to use it, it's totally fine too.

`Cask` is a build tools for emacs lisp, you can think it of `npm` for emacs lisp.

Enter project root, execute `cask install --dev`, this command will install all dependencies. After that, execute `cask emacs` which will start a emacs with extra `load-path` to load dependencies.
