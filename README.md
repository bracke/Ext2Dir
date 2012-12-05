# Ext2Dir
### Purpose:  Turn xxx/y into y.xxx

   File extensions used by non RISC OS systems are normally mapped
   to  directories under RISC OS. To do that appropriate directories
   have to be created, files have to be copied to these directories
   and the file extensions have to be removed.
   Ext2Dir does that for you.


   Example:

   main/adb ------> creates a directory 'adb' and copies
                    the file into that directory.

   test/c   ------> creates a directory 'c' and copies
                           the file into that directory

   Ext2Dir is a command-line program but is supplied with a frontend
   (!Ext2Dir).
   
NOTE: This is a RISC OS app.

## Frontend use

   Setup window:

   Icon  |            Action/Meaning                    |   Default
   ----   |           --------------                     |  -------
   Source  |          Where Ext2Dir will search for files (mandatory, typed or dragged)|   nil                     
   Target   |         Where directories are created (typed or dragged)       |  nil                     
   Change name |      Make names follow RISC OS conventions.| on
   Remove extension|  Removes file extension.                |on
   Access attributes| Set access attributes to 'unprotected'| on
   Set filetype      |Set filetype to text (&fff).           |on
   Remove Original  | Remove the original file.             | on
   Descend          | Recursive descend                     | on
   Mode files up    | Move subdirectory files to start dir  | on

## Setup Menu

   Entry     |        Action/Meaning         |              Default
   -----      |       --------------          |             -------
   Command line |  any other options can be entered.  (See the manual for full details.)  |   auto
                    


## Display (Ext2Dir) Window
This window is opened automatically and displays any output produced by Ext2Dir.

## Display (Ext2Dir) Menu
From this menu you can pause/continue and abort Ext2Dir.


## Commandline use
Use Ext2Dir -h to get help.


## License

MIT: <http://bracke.mit-license.org>
