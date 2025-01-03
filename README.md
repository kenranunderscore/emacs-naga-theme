# Naga: an Emacs theme

[![MELPA](https://melpa.org/packages/naga-theme-badge.svg)](https://melpa.org/#/naga-theme)

The main feature of these dark color themes is their usage of a green foreground
color, achieving a retro look while still being easy on the eyes.

Note that while I've been using this as my main theme for nearly a year now, it
only styles packages I actually use plus a small number of others.

Aside from the main `naga` theme, this package now ships with a more muted,
lower-contrast variant called `naga-dimmed`.

[Screenshots below](#screenshots), but note that they're a bit outdated
color-wise.

Feel free to create issues in case you find something that doesn't look good
with any variant, and I'll check it out.

## Installation

`naga` is [on MELPA](https://melpa.org/#/naga-theme). Once you have MELPA
enabled as a package repository, you could do

```
M-x package-install naga-theme RET
```

After installing the package (see below for alternative ways to install it),
`naga` and `naga-dimmed` should appear as options in `M-x load-theme`. (Note
that in Emacs multiple themes might be "enabled" at the same time, so if you get
weird results, make sure to `M-x disable-theme` other themes.)

### `use-package`

Add the following to your `init.el`; this lists all the options you may set, but
default values are fine here as well:

```elisp
(use-package naga-theme
  :config
  (setq naga-theme-use-lighter-org-block-background t)
  (setq naga-theme-surround-org-blocks t)
  (setq naga-theme-modeline-style 'green-box)
  (setq naga-theme-use-red-cursor nil)
  (setq naga-theme-use-legacy-sea-green nil)
  (load-theme 'naga t))
```

### Manual installation

After cloning this repository you need to make the location known to Emacs as
follows:

```elisp
(add-to-list 'custom-theme-load-path
             "/path/to/emacs-naga-theme")
(load-theme 'naga t)
```

## Customization

There are a couple of styling options available. It's best to check out
`M-x customize-group naga-theme RET`.

## Screenshots

### Org, Emacs Lisp, Nix
#### `naga`

![A screenshot of the naga theme, showing org-mode, nix-mode, and some Emacs Lisp](https://i.imgur.com/NzeuDtP.png)

#### `naga-dimmed`

![A screenshot of the naga-dimmed theme, showing org-mode, nix-mode, and some Emacs Lisp](https://i.imgur.com/MxZ5o1u.png)

### Haskell, Magit, Diredfl
#### `naga`

![A screenshot of the naga theme, showing Haskell code, diredfl, and a magit status buffer](https://i.imgur.com/UJ255uS.png)

#### `naga-dimmed`

![A screenshot of the naga-dimmed theme, showing Haskell code, diredfl, and a magit status buffer](https://i.imgur.com/700CuLt.png)
