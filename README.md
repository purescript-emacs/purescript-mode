[![MELPA](https://melpa.org/packages/purescript-mode-badge.svg)](https://melpa.org/#/purescript-mode)
[![Build Status](https://github.com/purescript-emacs/purescript-mode/workflows/CI/badge.svg)](https://github.com/purescript-emacs/purescript-mode/actions)

PureScript Mode for Emacs
----------------------

This is the PureScript mode package for Emacs.

To report problems or suggestions, please
[open an issue](https://github.com/dysinger/purescript-mode/issues?state=open)
in the issue tracker.

Below is a brief setup guide.

Quick Emacs rundown
--------------------

When Emacs is started up, it normally loads the
[Emacs initialization file](http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
usually called `~/.emacs`, `~/.emacs.el`, or `~/.emacs.d/init.el`;
with `~` standing for for your home directory.  This file should
contain all of your personal customisations written as a series of
Emacs Lisp commands.  In the following sections, this file will simply
be referred to as the `.emacs` file.

Installation
------------

GNU Emacs version 25.1 or later is officially supported.  It may work
with other Emacsen, but we don't have the resources to support other
versions.

### Installation using package.el

Users of [MELPA](https://melpa.org) can install `purescript-mode`
using `M-x package-install`. This is the most straightforward
and recommended installation method.

### Installation from Git

-   `git clone https://github.com/purescript-emacs/purescript-mode.git` into a
    suitable directory, e.g. `~/lib/emacs/purescript-mode/` where `~`
    stands for your home directory.

-   Assuming you have unpacked the various purescript-mode modules
    (`purescript-mode.el` and the rest) in the directory
    `~/lib/emacs/purescript-mode/`, you need generate the autoloads file
    (`purescript-mode-autoloads.el`) by either

    - Invoking `make purescript-mode-autoloads.el`, or `make all` (use
      this to perform byte-compilation and Info manual generation)

    - From inside Emacs, `M-x update-directory-autoloads` and answering the question for
      the folder with `~/lib/emacs/purescript-mode/` and the question for the output-file with
      `~/lib/emacs/purescript-mode/purescript-mode-autoloads.el`

    and then adding the following command to your `.emacs`:

    ```el
    (add-to-list 'load-path "~/lib/emacs/purescript-mode/")
    (require 'purescript-mode-autoloads)
    (add-to-list 'Info-default-directory-list "~/lib/emacs/purescript-mode/")
    ```

-   After updating your purescript-mode working directory, you need to
    re-run `make all` or `M-x update-directory-autoloads`.

Basic Configuration
-------------------

PureScript mode provides multiple indentation engines, and leaves the choice up to the user. To have indentation an according indentation mode needs to be enabled. Otherwise, attempting to indent will print an error describing this.

Minimal configuration may look something like:

```lisp
(use-package purescript-mode
  :defer t
  :config
  (defun myhook-purescript-mode ()
    (turn-on-purescript-indentation)
    (add-hook 'before-save-hook #'purescript-sort-imports nil t))
  (add-hook 'purescript-mode-hook #'myhook-purescript-mode))
```

Support
-------

- [Github homepage](https://github.com/purescript-emacs/purescript-mode)

Contributing
------------

Please make sure your pull requests are at least properly rebased and up to date.
