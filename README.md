# leetcode.el

LeetCode brings you offer, and now Emacs brings you LeetCode!

## Installation

dependencies:

- dash
- furl
- graphql

1. Clone this repository
2. Move it to your load-path
3. Require it in your emacs config

## Usage

You need to set your LeetCode account and password:

    (setq leetcode-account "your-account")
    (setq leetcode-password "your-password")

Put password into your emacs config is not a good idea, you may want to read you
password from somewhere else.

1. Execute `leetcode` command

![leetcode](images/leetcode.png)

In leetcode problems list buffer, n: down, p: up, <RET>: show problem description

2. Press `<RET>`, show problem description, move cursor to 'solve it', press
   `<RET>` again, start coding!

3. After finishing your code, execute `leetcode-submit`.

![leetcode-submit](images/leetcode-submit.png)
