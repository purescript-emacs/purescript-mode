;; Sample file for the new session/process stuff
;; Based on my own configuration. Well, it IS my configuration.
;;
;; NOTE: If you don't have cabal-dev, or you don't want to use it, you
;; should change purescript-process-type (see below) to 'ghci.
;;
;; To merely TRY this mode (and for debugging), do the below:
;;
;;     cd into purescript-mode's directory, and run
;;     $ emacs --load examples/init.el
;;
;; To get started, open a .purs file in one of your projects, and hit…
;;
;;   1. F5 to load the current file (and start a repl session), or
;;   2. C-` to just start a REPL associated with this project, or
;;   3. C-c C-c to build the cabal project (and start a repl session).

;; Add the current dir for loading purescript-site-file.
(add-to-list 'load-path ".")
;; Always load via this. If you contribute you should run `make all`
;; to regenerate this.
(load "purescript-mode-autoloads")

;; Customization
(custom-set-variables
 ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
 ;;'(purescript-process-type 'cabal-dev)
 
 ;; Use notify.el (if you have it installed) at the end of running
 ;; Cabal commands or generally things worth notifying.
 '(purescript-notify-p t)

 ;; To enable tags generation on save.
 '(purescript-tags-on-save t)

 ;; To enable stylish on save.
 '(purescript-stylish-on-save t))

(add-hook 'purescript-mode-hook 'purescript-hook)
(add-hook 'purescript-cabal-mode-hook 'purescript-cabal-hook)

;; PureScript main editing mode key bindings.
(defun purescript-hook ()
  ;; Use simple indentation.
  (turn-on-purescript-simple-indent)
  (define-key purescript-mode-map (kbd "<return>") 'purescript-simple-indent-newline-same-col)
  (define-key purescript-mode-map (kbd "C-<return>") 'purescript-simple-indent-newline-indent)

  ;; Load the current file (and make a session if not already made).
  (define-key purescript-mode-map [?\C-c ?\C-l] 'purescript-process-load-file)
  (define-key purescript-mode-map [f5] 'purescript-process-load-file)

  ;; Switch to the REPL.
  (define-key purescript-mode-map [?\C-c ?\C-z] 'purescript-interactive-switch)
  ;; “Bring” the REPL, hiding all other windows apart from the source
  ;; and the REPL.
  (define-key purescript-mode-map (kbd "C-`") 'purescript-interactive-bring)

  ;; Build the Cabal project.
  (define-key purescript-mode-map (kbd "C-c C-c") 'purescript-process-cabal-build)
  ;; Interactively choose the Cabal command to run.
  (define-key purescript-mode-map (kbd "C-c c") 'purescript-process-cabal)

  ;; Get the type and info of the symbol at point, print it in the
  ;; message buffer.
  (define-key purescript-mode-map (kbd "C-c C-t") 'purescript-process-do-type)
  (define-key purescript-mode-map (kbd "C-c C-i") 'purescript-process-do-info)

  ;; Contextually do clever things on the space key, in particular:
  ;;   1. Complete imports, letting you choose the module name.
  ;;   2. Show the type of the symbol after the space.
  (define-key purescript-mode-map (kbd "SPC") 'purescript-mode-contextual-space)

  ;; Jump to the imports. Keep tapping to jump between import
  ;; groups. C-u f8 to jump back again.
  (define-key purescript-mode-map [f8] 'purescript-navigate-imports)

  ;; Jump to the definition of the current symbol.
  (define-key purescript-mode-map (kbd "M-.") 'purescript-mode-tag-find)

  ;; Indent the below lines on columns after the current column.
  (define-key purescript-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (purescript-move-nested 1)))
  ;; Same as above but backwards.
  (define-key purescript-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (purescript-move-nested -1))))

;; Useful to have these keybindings for .cabal files, too.
(defun purescript-cabal-hook ()
  (define-key purescript-cabal-mode-map (kbd "C-c C-c") 'purescript-process-cabal-build)
  (define-key purescript-cabal-mode-map (kbd "C-c c") 'purescript-process-cabal)
  (define-key purescript-cabal-mode-map (kbd "C-`") 'purescript-interactive-bring)
  (define-key purescript-cabal-mode-map [?\C-c ?\C-z] 'purescript-interactive-switch))
