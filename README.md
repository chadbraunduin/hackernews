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

## Installation Instructions (Ubuntu)
    sudo apt-get install ncurses-dev sbcl lynx
    git clone git@github.com:chadbraunduin/hackernews.git
    cd hackernews
    sbcl --load save-core.lisp
    ./hackernews

## Installation Instructions (other *nix)
    *install ncurses-dev library, sbcl, and lynx*
    yum install ncurses-devel #install ncurses dev on Red Hat / Fedora
    git clone git@github.com:chadbraunduin/hackernews.git
    cd hackernews
    sbcl --load save-core.lisp
    ./hackernews
    
## Known Installation Issues
You may get the following error while installing hackernews
    Could not open library 'libncurses.so': /usr/lib/libncurses.so: file too short.
The fix is:
    sudo mv /usr/lib/libncurses.so{,.bak}
    sudo ln -s /lib/libncurses.so.5 /usr/lib/libncurses.so

## Commands
### All pages
use the arrow keys, page keys, home key, and end key to navigate items

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

## Credits
I use the [Unofficial Hacker News API](http://api.ihackernews.com/) developed by [Ronnie Roller](http://ronnieroller.com/) to gather the data.

I also require the following libraries within the code:

* asdf
* drakma
* cl-json
* cl-ppcre
* uffi
* cl-ncurses

## Known issues
Every once in a while the cl-json method that converts the raw data into a CL form blows up. The error message returned indicates that there is an invalid character within the data. Usually, this issue goes away within a couple of minutes. I haven't haven't taken the time to fully investigate it.

Also, within the comments, the little ellipse character that appears after an edit is not displaying correctly. Not sure if it is getting mangled by the API in transmission or if it is something my code is doing.

## TODOS
* Allow upvoting of posts and comments
* Get this to work in Windows
