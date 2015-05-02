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
 * implement better heuristics
 * testing
 * faster?
 * save as binary file / faster parsing / use a db?
 * use cereal for faster / real serialization

USAGE:
------

    /PATH/TO/univOpWrap [-f] [-a] cmd [-l] [-h] [arg [arg [ ...]]]

Simply add the following to yourt aliasrc:

    alias zathura="/PATH/TO/univOpWrap zathura"

or if you do not want to overwrite the command:

    alias p="/PATH/TO/univOpWrap llpp"

if you want the process to detach, use

    alias p="/PATH/TO/univOpWrap -f llpp"

if you want to be asked, wether you realy want to call the command on the file,
use

    alias p="/PATH/TO/univOpWrap -a llpp"
