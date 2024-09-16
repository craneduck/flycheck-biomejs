
# flycheck-biomejs.el

This Flycheck extension is an **EXPERIMENTAL PACKAGE** intended to provide JavaScript/TypeScript/CSS syntax checking by [Biome](https://biomejs.dev/).

このFlycheck拡張は [Biome](https://biomejs.dev/) による JavaScript/TypeScript/CSS 構文チェックを提供するための**実験的パッケージ**です。

## Requirements

* Biome 1.9.0 or later

Please follow the installation instructions in [Biome's Getting Started](https://biomejs.dev/guides/getting-started/) guide.

Or try to install manually, `$ brew install biome` .

## Installation

``` emacs-lisp
(leaf flycheck-biomejs
  :straight (flycheck-biomejs :type git :host github :repo "craneduck/flycheck-biomejs")
  :require t)
```

``` emacs-lisp
(straight-use-package '(flycheck-biomejs :type git :host github :repo "craneduck/flycheck-biomejs"))
```
