PureScript Mode for Emacs
----------------------

[![Build Status](https://travis-ci.org/purescript/purescript-mode.png?branch=master)](https://travis-ci.org/purescript/purescript-mode)

This is the PureScript mode package for Emacs.

Please see
[the online purescript-mode manual](https://github.com/purescript/purescript-mode/wiki)
for setup and use guide.

To report problems or suggestions, please
[open an issue](https://github.com/purescript/purescript-mode/issues?state=open)
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

GNU Emacs version 23 or later is officially supported.  It may work
with other Emacsen, but we don't have the resources to support other
versions.

There are many ways to install `purescript-mode`. The following sections
describe the most common ones; pick the one that you're most
comfortable with.

### `package.el`-based Installation

*This is the recommended way*

`package.el` is the new
[built-in package manager](http://www.emacswiki.org/emacs/ELPA#toc4)
included in Emacs 24.x. On Emacs 23.x you will need to download
[`package.el`](http://bit.ly/pkg-el23) yourself and place `package.el`
somewhere in your
[`load-path`](http://www.emacswiki.org/emacs/LoadPath).

#### Marmalade

**Stable releases** of `purescript-mode` are available on
[Marmalade](http://marmalade-repo.org/packages/purescript-mode).

If you're not already using Marmalade, add the following snippet to
your `.emacs` and evaluate it with `M-x eval-buffer`:

```el
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
```

Refresh the package index by `M-x package-refresh-contents` and install
`purescript-mode` via `M-x package-install [RET] purescript-mode`.

Alternatively, you can also download the `.tar` file via the
_Download_ link at http://marmalade-repo.org/packages/purescript-mode and
install the package `.tar`-file via `M-x package-install-file`

#### MELPA

**Unstable snapshots** can be installed via the
[MELPA](http://melpa.milkbox.net) community maintained repository.

For MELPA the code you need to add is:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

Refresh the package index by `M-x package-refresh-contents` and install
`purescript-mode` via `M-x package-install [RET] purescript-mode`.

### el-get based Installation

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs.
If you're an el-get user just do `M-x el-get-install` to get `purescript-mode` installed.

### Emacs Prelude

`purescript-mode` is bundled with
[Emacs Prelude](https://github.com/bbatsov/prelude). If you're a
Prelude user you can start using it right away.

### Debian

If you are using Debian, you can install an older version (e.g. Wheezy
ships with version 2.8.0) of `purescript-mode` with a command like:

```bash
$ apt-get install purescript-mode
```

### Installation from Git

*This installation method requires more work and recommended for purescript-mode developers/contributors only as it allows to load purescript-mode directly from the checked out Git working copy. If you just want to use bleeding edge versions of purescript-mode please use the MELPA installation method described above.*

-   `git clone https://github.com/purescript/purescript-mode.git` into a
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

For setup instructions, please consult the new integrated purescript-mode
[Info](https://www.gnu.org/software/texinfo/manual/info/info.html)
manual which can be accessed after installation via
`M-x info-display-manual [RET] purescript-mode`.
Alternatively, you can also direct your browser to the
[latest online HTML version](http://purescript.github.io/purescript-mode/manual/latest/).

Support
-------

- [Mailing list](http://projects.purescript.org/cgi-bin/mailman/listinfo/purescriptmode-emacs)
- [Github homepage](https://github.com/purescript/purescript-mode)

Contributing
------------

For submitting pull requests, please see the wiki
[page on contributing](https://github.com/purescript/purescript-mode/wiki/Contributing). You
don't have to follow this guide, but please make sure your pull
requests are at least properly rebased and up to date.
