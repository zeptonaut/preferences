Purpose: help file containing useful emacs commands.

## General
  * `C-h m` (`describe-mode`) Get a description of the current mode, along with a list of keybindings.

## c-mode
  * `C-c C-s` Shows the syntactic element you're at in the code. Useful for customizing indentation with c-offset-alist.

## org-mode
  * `C-c C-s` Specifies the day that you expect to start working on a task
  * `C-c C-d` Specifies the deadline for a task
  * `C-c a t` (`org-agenda-todo`): Brings up a list of all oustanding TODOs in agenda files.
  * `C-c a a`: Brings up a list of all tasks scheduled for completion over the next week.
    * `f`: Go forward a week
    * `b`: Go back a week
    * `l`: Toggles whether DONE tasks are shown in the agenda
  * `qc` (`org-capture`): Captures a task from any buffer
  * `qr` (`org-capture-goto-last-stored`) Goes to the place where captured tasks are stored
  * `C-c C-w` (`org-capture-refile`) Refiles the task into another file
  * `C-c C-c` Toggle state of a checkbox in a sublist
  * `C-c a w` Retrieves a list of all tasks labeled WAIT
  * `C-c C-x o` Toggles the ordered state of an entry. Task lists that are ordered must be completed in order.
  * `C-u C-u C-u C-c C-t` Toggles the TODO state of an entry, circumventing any task blocking.
  * `C-c ,` Sets the priority of the current entry.
  * `C-c *` Turns a plain list into a headline

## org-agenda-mode
  * `n` Move to the next task
  * `p` Move to the previous task
  * `]` Move to the next day
  * `[` Move to the previous day

## smerge-mode
  * `qm` Accepts my changes.
  * `qt` Accepts their changes.
  * `qn` Goes to the next conflict.
  * `qp` Goes to the previous conflict.
  
## projectile-mode

* `C-c p p` Switch to another project
* `qf` Finds a file within a project
