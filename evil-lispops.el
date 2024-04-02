;;; evil-lispops.el --- operations for editing lisp evilly  -*- lexical-binding: t -*-

;; Copyright (C) 2024 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/evil-lispops
;; Created: April 1, 2024
;; Modified: April 1, 2024
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (evil "1.2.10"))

;; evil-lispops - operations for editing lisp evilly

;; Copyright (C) 2024 precompute

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; - Operators always open inside blocks.
;; - All operators accept prefix numbers.

;; | Operator | Action                                    |
;; |----------+-------------------------------------------|
;; | >.       | Go to end of current paren-block          |
;; | <.       | Go to beg of current paren-block          |
;; | >i       | Open at end of current paren-block        |
;; | <i       | Open at beg of current paren-block        |
;; | >j       | Open at end of child paren-block          |
;; | <j       | Open at beg of child paren-block          |
;; | >J       | Go to end of child paren-block            |
;; | <J       | Go to beg of child paren-block            |
;; | >k       | Open at end of parent paren-block         |
;; | <k       | Open at beg of parent paren-block         |
;; | >K       | Go to end of parent paren-block           |
;; | <K       | Go to beg of parent paren-block           |
;; | >h       | Open at end of left adjacent paren-block  |
;; | <h       | Open at beg of left adjacent paren-block  |
;; | >H       | Go to end of left adjacent paren-block    |
;; | <H       | Go to beg of left adjacent paren-block    |
;; | >l       | Open at end of right adjacent paren-block |
;; | <l       | Open at beg of right adjacent paren-block |
;; | >L       | Go to end of right adjacent paren-block   |
;; | <L       | Go to beg of right adjacent paren-block   |

;;; Code:
(require 'evil)

;;;; Operators:

(defun evil-lispops--get-range ()
  "Use `evil-select-paren’ to get the value of points at the
ends of a paren block."
  (evil-select-paren ?\( ?\) (point) 0 nil 1))

(defun evil-lispops-goto-beg ()
  (interactive)
  (let ((e (evil-lispops--get-range)))
    (if e
        (goto-char (car e))
      (message "No brackets found."))))

(defun evil-lispops-goto-end ()
  (interactive)
  (let ((e (evil-lispops--get-range)))
    (if e
        (goto-char (cadr e))
      (message "No brackets found."))))

(defun evil-lispops-open-beg ()
  (interactive)
  (progn
    (evil-lispops-goto-beg)
    (evil-insert 0)))

(defun evil-lispops-open-end ()
  (interactive)
  (progn
    (evil-lispops-goto-end)
    (evil-insert 0)))

(defun evil-lispops-goto-parent-beg (&optional count)
  (interactive)
  (let ((c (or count 1)))
    (evil-lispops-goto-beg)
    (dotimes (_ c)
      (progn
        (goto-char (- (point) 2))
        (unless (looking-at "(")
          (evil-lispops-goto-beg))))))

(defun evil-lispops-goto-parent-end (&optional count)
  (interactive)
  (let ((c (or count 1)))
    (evil-lispops-goto-end)
    (dotimes (_ c)
      (progn
        (goto-char (1+ (point)))
        (unless (looking-at "(")
          (evil-lispops-goto-end))))))

(defun evil-lispops-open-parent-beg (&optional count)
  (interactive)
  (let ((c (or count 1)))
    (evil-lispops-goto-parent-beg c)
    (evil-insert 0)))

(defun evil-lispops-open-parent-end ()
  (interactive)
  (let ((c (or count 1)))
    (evil-lispops-goto-parent-end c)
    (evil-insert 0)))
