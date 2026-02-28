
# flycheck-biomejs.el

This Flycheck extension is an **EXPERIMENTAL PACKAGE** intended to provide JavaScript/TypeScript/CSS syntax checking by [Biome](https://biomejs.dev/).

このFlycheck拡張は [Biome](https://biomejs.dev/) による JavaScript/TypeScript/CSS 構文チェックを提供するための**実験的パッケージ**です。

## Requirements

* Biome 2.4.0 or later

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

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE) for details.
