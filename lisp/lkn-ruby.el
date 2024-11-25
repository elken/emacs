;;; lkn-ruby.el -- The least shit Kaiser Chiefs song -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2024Ellis Kenyő

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Currently my $WORKLANG, this will end up having a fair amount of
;;; config. A lot of the power will come from LSP, and I lean quite a
;;; bit on that.
;;; Code:

(use-package rbenv
  :defer t
  :hook (ruby-mode . global-rbenv-mode))

(provide 'lkn-ruby)
;;; lkn-ruby.el ends here
