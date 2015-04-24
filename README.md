At the moment, this is **only an idea**. It can be used.

univOpWrap:
-----------
This small program wants to provide a simple and universal wrapper for shell
commands. The Goal is to build a universal alternative to 
 - https://github.com/rupa/v and
 - https://github.com/rupa/z

IDEAS/PLANS:
------
 * implement a metric, which tells how important a file is, depending on
   - how often it was opened
   - how 'good' the match is
 * keep track of opened files by creating lists in some folder '.univOpWrap'

TODO:
-----
 * nearly all

USAGE:
------
Simply add the following to yourt aliasrc:

    alias zathura="/PATH/TO/univOpWrap zathura"
