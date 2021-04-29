<!---
 - Copyright (c) 2019 Charlie Burnett <burne251@umn.edu>
 -
 - Permission to use, copy, modify, and distribute this software for any
 - purpose with or without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies.
 -
 - THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 - WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 - MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 - ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 - WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 - ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 - OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--->
## Personal Configs

Just a personal repository for configs. The me_logo.png is property the
University of Minnesota, it is simply in this repo for ease of use. Everything
else here is released under the ISC license, which can be found in the LICENSE
file, or in the header of the files themselves.

## Installation

The only things with external dependencies are my init.el file for Emacs, and I
guess technically speaking the Latex template needs Latex. All required packages
include:

- [**Clang Tools Extra**]()
- [**Clang**](https://clang.llvm.org/)
- [**CMake**](https://cmake.org/)
- [**CScope**](http://cscope.sourceforge.net/)
- [**Emacs**](https://www.gnu.org/software/emacs/)
- [**GDB**](https://www.gnu.org/s/gdb/)
- [**Git**](https://git-scm.com/)
- [**Inconsolata**](https://fonts.google.com/specimen/Inconsolata)
- [**LLVM**](https://llvm.org/)
- [**Python 3**](https://www.python.org/)
- [**Texlive**](https://www.tug.org/texlive/)
- [**YCMD**](https://github.com/ycm-core/ycmd/)

I imagine all of these are probably in your OS's repository (or in WSL's repo in
the case of Windows) with the exception of YCMD, which has build instructions
[on their Github page](https://github.com/ycm-core/ycmd#Building).

## NOTE: Custom Theme (for now)

Currently I have a fork of 
[fniessen's Emacs Leuven theme,](https://github.com/fniessen/emacs-leuven-theme) 
with centaur-tabs and doom-modeline support added. I've opened up a pull request, 
but for now you can just download 
[my fork]https://github.com/burne251/emacs-leuven-theme and put it into 
~/.emacs.d/- if you choose not to, just comment out the custom path for leuven and 
uncomment \(use-package leuven-theme\).
