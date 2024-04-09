;;; evil-lispops.el --- operations for editing lisp evilly  -*- lexical-binding: t -*-

;; Copyright (C) 2024 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/evil-lispops
;; Created: April 1, 2024
;; Modified: April 9, 2024
;; Version: 0.3.0
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

;; | Operator | Action                                         |
;; |----------+------------------------------------------------|
;; | >.       | Go to end of current paren pair                |
;; | <.       | Go to beg of current paren pair                |
;; | >i       | Open at end of current paren pair              |
;; | <i       | Open at beg of current paren pair              |
;; | >j       | Open at end of child paren pair                |
;; | <j       | Open at beg of child paren pair                |
;; | >J       | Go to end of child paren pair                  |
;; | <J       | Go to beg of child paren pair                  |
;; | >k       | Open at end of parent paren pair               |
;; | <k       | Open at beg of parent paren pair               |
;; | >K       | Go to end of parent paren pair                 |
;; | <K       | Go to beg of parent paren pair                 |
;; | >h       | Open at end of left adjacent child paren pair  |
;; | <h       | Open at beg of left adjacent child paren pair  |
;; | >H       | Go to end of left adjacent child paren pair    |
;; | <H       | Go to beg of left adjacent child paren pair    |
;; | >l       | Open at end of right adjacent child paren pair |
;; | <l       | Open at beg of right adjacent child paren pair |
;; | >L       | Go to end of right adjacent child paren pair   |
;; | <L       | Go to beg of right adjacent child paren pair   |

;;; Code:
(require 'evil)

;;;; Variables
(defvar evil-lispops-open-inside t
  "Whether evil-lispops-open-* commands open inside the bracket (t) or before the bracket (nil).")

;;;; Helper Functions
(defmacro evil-lispops--dec-var (var)
  "Decrement and set `VAR’."
  `(setq ,var (- ,var 1)))

(defun evil-lispops--get-range (&optional count inclusive?)
  "Use `evil-select-paren’ to get the value of points at the ends of a paren pair.
Accepts `COUNT’.  `INCLUSIVE?’ determines whether range is
inside the paren block or outside."
  (let ((count (or count 1)))
    ;; evil-select-paren does not work when bracket is at [:space:]+(eobp)
    ;; (if (and (or (eobp)
    ;;              (= (point-max) (+ (point) 1))
    ;;              (= (point-max) (+ (point) 2)))
    ;;          (looking-at ")"))
    ;;     (save-excursion
    ;;       (goto-char (- (point) 1))
    ;;       (evil-select-paren ?\( ?\) (point) 0 nil count inclusive?))
    ;;   (evil-select-paren ?\( ?\) (point) 0 nil count inclusive?))
    (evil-select-paren ?\( ?\) (point) 0 nil count inclusive?)))

;;;; Operators:

(defun evil-lispops-goto-beg (&optional count inclusive?)
  "Go to beginning of paren pair.  `COUNT’ to enable jumping to nth parent.
`INCLUSIVE’ for cursor placement style."
  (interactive)
  (progn
    (if (looking-at ")")
        (goto-char (- (point) 1)))
    (let ((count (or count 1)))
      (goto-char (car (evil-lispops--get-range count inclusive?))))))

(defun evil-lispops-goto-end (&optional count inclusive?)
  "Go to end of paren pair.  `COUNT’ to enable jumping to nth parent.
`INCLUSIVE’ for cursor placement style."
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
      (evil-lispops-goto-beg nil t))
    (evil-insert 0)))

(defun evil-lispops-open-end ()
  "Open at end of paren pair."
  (interactive)
  (progn
    (if evil-lispops-open-inside
        (evil-lispops-goto-end)
      (evil-lispops-goto-end nil t))
    (evil-insert 0)))

(defun evil-lispops-goto-parent-beg (&optional count inclusive?)
  "Go to beginning of parent paren pair.  Accepts `COUNT’.
`INCLUSIVE’ for cursor placement style."
  (interactive "P")
  (let ((count (or count 1)))
    (if (looking-at "(")
        (goto-char (+ (point) 1)))
    (evil-lispops-goto-beg (1+ count) inclusive?)))

(defun evil-lispops-goto-parent-end (&optional count inclusive?)
  "Go to end of parent paren pair.  Accepts `COUNT’.
`INCLUSIVE’ for cursor placement style."
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
    (evil-lispops-open-beg)))

(defun evil-lispops-open-parent-end (&optional count)
  "Open at end of parent paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (if evil-lispops-goto-parent-end
        (evil-lispops-goto-parent-end count)
      (evil-lispops-goto-parent-end count t))
    (unless (eq (point) point)
      (evil-lispops-open-end))))

(defun evil-lispops-goto-child-beg (&optional count goto-end? relative?)
  "Go to beginning of child paren pair.  Accepts `COUNT’.
`GOTO-END?’ for the reverse operation.  `RELATIVE?’ for relative child."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (unless relative? (evil-lispops-goto-beg))
    (let ((parentrange (evil-lispops--get-range))
          (postloop? nil))
      (while (> count 0)
        (progn
          (while (and (not (looking-at "("))
                      (> (cadr parentrange) (point)))
            (evil-forward-WORD-begin))
          (evil-lispops-goto-beg nil t)
          (if (not (= count 1))
              (progn
                (evil-lispops-goto-end)
                (goto-char (+ (point) 1))))
          (evil-lispops--dec-var count)))
      (if (looking-at "(")
          (progn (goto-char (+ (point) 1))
                 (when goto-end? (evil-lispops-goto-end)))
        (goto-char point)))))

(defun evil-lispops-goto-child-end (&optional count)
  "Go to end of child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (evil-lispops-goto-child-beg count t)))

(defun evil-lispops-open-child-beg (&optional count)
  "Open at beginning of child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (progn
      (evil-lispops-goto-child-beg count)
      (unless (eq (point) point)
        (evil-lispops-open-beg)))))

(defun evil-lispops-open-child-end (&optional count)
  "Open at end of child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (progn
      (evil-lispops-goto-child-end count)
      (unless (eq (point) point)
        (evil-lispops-open-end)))))

(defun evil-lispops-goto-right-adjacent-child-beg (&optional count)
  "Go to beginning of right adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (evil-lispops-goto-child-beg count nil t)))

(defun evil-lispops-goto-right-adjacent-child-end (&optional count)
  "Go to end of right adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (evil-lispops-goto-child-beg count t t)))

(defun evil-lispops-open-right-adjacent-child-beg (&optional count)
  "Open at beginning of right adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (progn
      (evil-lispops-goto-child-beg count nil t)
      (unless (eq (point) point)
        (evil-lispops-open-beg)))))

(defun evil-lispops-open-right-adjacent-child-end (&optional count)
  "Open at end of right adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point)))
    (progn
      (evil-lispops-goto-child-beg count t t)
      (unless (eq (point) point)
        (evil-lispops-open-end)))))

(defun evil-lispops-goto-left-adjacent-child-beg (&optional count)
  "Go to beginning of left adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1))
        (point (point))
        (parentrange (evil-lispops--get-range)))
    (while (> count 0)
      (progn
        (while (and (not (looking-at ")"))
                    (> (point) (car parentrange)))
          (evil-backward-WORD-end))
        (evil-lispops-goto-beg nil t)
        (evil-lispops--dec-var count)
        (if (looking-at "(")
            (evil-lispops-goto-beg)
          (if (> (point) (car parentrange))
              (goto-char point)))))))

(defun evil-lispops-goto-left-adjacent-child-end (&optional count)
  "Go to end of left adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (progn
     (evil-lispops-goto-left-adjacent-child-beg count)
     (evil-lispops-goto-end))))

(defun evil-lispops-open-left-adjacent-child-beg (&optional count)
  "Open at beginning of left adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (progn
     (evil-lispops-goto-left-adjacent-child-end count)
     (evil-lispops-open-beg))))

(defun evil-lispops-open-left-adjacent-child-end (&optional count)
  "Open at end of left adjacent child paren pair.  Accepts `COUNT’."
  (interactive "P")
  (let ((count (or count 1)))
    (progn
     (evil-lispops-goto-left-adjacent-child-beg count)
     (evil-lispops-open-end))))
