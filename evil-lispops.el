;;; evil-lispops.el --- operations for editing lisp evilly  -*- lexical-binding: t -*-

;; Copyright (C) 2024 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/evil-lispops
;; Created: April 1, 2024
;; Modified: April 8, 2024
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

;; - Operators always open inside pairs.
;; - All operators accept prefix numbers.
;; - The `<’ and `>’ prefixes overwrite some defaults,but I use them
;; very infrequently, so I don't mind making a visual selection
;; first.

;; | Operator | Action                                   |
;; |----------+------------------------------------------|
;; | >.       | Go to end of current paren pair          |
;; | <.       | Go to beg of current paren pair          |
;; | >i       | Open at end of current paren pair        |
;; | <i       | Open at beg of current paren pair        |
;; | >j       | Open at end of child paren pair          |
;; | <j       | Open at beg of child paren pair          |
;; | >J       | Go to end of child paren pair            |
;; | <J       | Go to beg of child paren pair            |
;; | >k       | Open at end of parent paren pair         |
;; | <k       | Open at beg of parent paren pair         |
;; | >K       | Go to end of parent paren pair           |
;; | <K       | Go to beg of parent paren pair           |
;; | >h       | Open at end of left adjacent paren pair  |
;; | <h       | Open at beg of left adjacent paren pair  |
;; | >H       | Go to end of left adjacent paren pair    |
;; | <H       | Go to beg of left adjacent paren pair    |
;; | >l       | Open at end of right adjacent paren pair |
;; | <l       | Open at beg of right adjacent paren pair |
;; | >L       | Go to end of right adjacent paren pair   |
;; | <L       | Go to beg of right adjacent paren pair   |

;;; Code:
(require 'evil)

;;;; Variables
(defvar evil-lispops-open-inside nil
  "Whether evil-lispops-open-* commands open inside the bracket (`t’) or on the bracket (`nil’).")

;;;; Helper Functions
(defmacro evil-lispops--dec-var (var)
  "Decrement and set `VAR’."
  `(setq ,var (- ,var 1)))

(defun evil-lispops--get-range (&optional count inclusive?)
  "Use `evil-select-paren’ to get the value of points at the ends of a
paren pair.  Accepts `COUNT’.  `INCLUSIVE?’ determines whether range is
inside the paren block or outside."
  (let ((count (or count 1)))
    (evil-select-paren ?\( ?\) (point) 0 nil count inclusive?)))

;;;; Operators:

(defun evil-lispops-goto-beg (&optional count inclusive?)
  "Go to beginning of paren pair."
  (interactive)
  (progn
    (if (looking-at ")")
        (goto-char (- (point) 1)))
    (let ((count (or count 1)))
      (goto-char (car (evil-lispops--get-range count inclusive?))))))

(defun evil-lispops-goto-end (&optional count inclusive?)
  "Go to end of paren pair."
  (interactive)
  (progn
    (if (looking-at ")")
        (goto-char (- (point) 1)))
    (let ((count (or count 1)))
      (goto-char (cadr (evil-lispops--get-range count inclusive?))))))

(defun evil-lispops-open-beg ()
  "Open at beginning of paren pair."
  (interactive)
  (progn
    (if evil-lispops-open-inside
        (evil-lispops-goto-beg)
      (evil-lispops-goto-beg t))
    (evil-insert 0)))

(defun evil-lispops-open-end ()
  "Open at end of paren pair."
  (interactive)
  (progn
    (if evil-lispops-open-inside
        (evil-lispops-goto-end)
      (evil-lispops-goto-end t))
    (evil-insert 0)))

(defun evil-lispops-goto-parent-beg (&optional count inclusive?)
  "Go to beginning of parent paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (if (looking-at "(")
        (goto-char (+ (point) 1)))
    (evil-lispops-goto-beg (1+ count) inclusive?)))

(defun evil-lispops-goto-parent-end (&optional count inclusive?)
  "Go to end of parent paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (evil-lispops-goto-end (1+ count) inclusive?)))

(defun evil-lispops-open-parent-beg (&optional count)
  "Open at beginning of parent paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (if evil-lispops-open-inside
        (evil-lispops-goto-parent-beg count)
      (evil-lispops-goto-parent-beg count t))
    (evil-insert 0)))

(defun evil-lispops-open-parent-end (&optional count)
  "Open at end of parent paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (if evil-lispops-goto-parent-end
        (evil-lispops-goto-parent-end count)
      (evil-lispops-goto-parent-end count t))
    (unless (eq (point) point)
      (evil-insert 0))))

(defun evil-lispops-goto-child-beg (&optional count)
  "Go to beginning of child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (evil-lispops-goto-beg)
    (let ((parentrange (evil-lispops--get-range))
          (postloop? nil))
      (while (> count 0)
        (while (and (not (looking-at "("))
                    (> (cadr parentrange) (point)))
          (evil-forward-WORD-begin))
        (evil-lispops-goto-beg nil t)
        (if (not (= count 1))
            (progn
              (evil-lispops-goto-end)
              (goto-char (+ (point) 1))))
        (evil-lispops--dec-var count))
      (if (looking-at "(")
          (goto-char (+ (point) 1))
        (goto-char point)))))
