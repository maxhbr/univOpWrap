univOpWrap:
-----------
This small program wants to provide a simple and generic alternative to
 - https://github.com/rupa/v for `vim` and
 - https://github.com/rupa/z for `cd`
which can be for example used to wrap the pdf-viewer command.

IDEAS/PLANS:
------
 * implement a metric, which tells how important a file is, depending on
   - how often it was opened
   - how 'good' the match is
 * keep track of opened files by creating lists in some folder '.univOpWrap'

TODO:
-----
 * ~~implement better heuristics~~
 * implement TUI for interactive search
 * testing
 * faster?
 * ~~save as binary file / faster parsing / use a db?~~
 * ~~use cereal for faster / real serialization~~

USAGE:
------

    /PATH/TO/univOpWrap [-f] [-a] [-t] cmd [-l] [-h] [arg [arg [ ...]]]

Simply add the following to yourt aliasrc:

    alias zathura="/PATH/TO/univOpWrap zathura"

or if you do not want to overwrite the command:

    alias p="/PATH/TO/univOpWrap llpp"

if you want the process to detach, use

    alias p="/PATH/TO/univOpWrap -f llpp"

if you want to be asked, wether you realy want to call the command on the file,
use

    alias p="/PATH/TO/univOpWrap -a llpp"

Filetype dependent commands are given like:

    alias p="/PATH/TO/univOpWrap \".djvu:zathura llpp\""
