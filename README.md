
# Hacker News in the Terminal

Written in Common Lisp

## Inspiration
The inspiration for this came from the [hacker-top](http://www.catonmat.net/blog/follow-hacker-news-from-the-console/) program written in Python.

My program doesn't work exactly like hacker-top. In addition to being written in Common Lisp instead of Python, the main differences are:

* browse articles in the terminal using lynx
* read comments and user profiles in the terminal
* switch between the "front page" and "newest" articles without restarting the program

## Requirements
I developed this on a Ubuntu machine. The requirements I know of are:

* the lynx text based browser
* sbcl (Steel Bank Common Lisp)
* ncurses-dev (the ncurses library)

## Instructions (Ubuntu)
    sudo apt-get install ncurses-dev sbcl lynx
    sbcl --load save-core.lisp
    ./hackernews

## Instructions (other *nix)
    *install ncurses-dev library, sbcl, and lynx*
    sbcl --load save-core.lisp
    ./hackernews

## Commands
### All pages
**q** - quit

**b** - go back to a previous screen or page

type in a username to view the user's profile

### Front and Newest pages
**h** - go to the front page

**n** - go to the newest page

**r** - reload the current page

**m** - more posts

type in the post number to view it in lynx

type in "c" plus the post number to view the comments for it

## TODOS
* Allow upvoting of posts and comments
* Get this to work in Windows
