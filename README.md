# Naga: an Emacs theme (WIP)

The main feature of this dark color theme is its usage of a green foreground
color, achieving a retro look while still being easy on the eyes.

Note that while I've been using this as my main theme for nearly a year now, it
still is very much a work in progress, as I've only styled and checked the
packages I actually use so far. Some main colors might still be subject to
change, I'm mainly unhappy with pink (maybe try out a more purple-ish tone
instead) and the coloring of strings.

## Installation

`naga` is [on MELPA](https://melpa.org/#/naga-theme). Once you have MELPA
enabled as a package repository, you could do

```
M-x package-install naga-theme RET
```

After installing the package (see below for alternative ways to install it),
`naga` should appear as an option in `M-x load-theme`. (Note that in Emacs
multiple themes might be "enabled" at the same time, so if you get weird
results, make sure to `M-x disable-theme` other themes.)

### `use-package`

Add the following to your `init.el`:

```elisp
(use-package naga-theme
  :config
  ;; If you with to disable accentuated org blocks
  (setq naga-theme-use-lighter-org-block-background nil)
  (setq naga-theme-modeline-style 'filled-green)
  (load-theme 'naga t))
```

### Manual installation

I'm planning to put this on MELPA at some point. For now, clone this repository
and add make the location known to Emacs as follows:

```elisp
(add-to-list 'custom-theme-load-path
             "~/path/to/emacs-naga-theme")
(load-theme 'naga t)
```

## Customization

Check out `M-x customize-group naga-theme RET`.

## Screenshots

### Org, Emacs Lisp, Nix

![A screenshot of the naga theme, showing org-mode, nix-mode, and some Emacs Lisp](https://i.imgur.com/AkcFGhx.png)

### Haskell, Magit, Diredfl

![A screenshot of the naga theme, showing Haskell code, diredfl, and a magit status buffer](https://i.imgur.com/JKFGqfk.png)
