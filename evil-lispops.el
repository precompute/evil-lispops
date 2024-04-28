;;; evil-lispops.el --- Operations for editing lisp evilly  -*- lexical-binding: t -*-

;; Copyright (C) 2024 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/evil-lispops
;; Created: April 1, 2024
;; Modified: April 28, 2024
;; Version: 0.10.2
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
;; - The `<’ and `>’ prefixes overwrite some defaults, but I use them
;; very infrequently and I don't mind making a visual selection first.

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
;; | >n       | Open at end of right sibling paren pair        |
;; | <n       | Open at beg of right sibling paren pair        |
;; | >N       | Go to end of right sibling paren pair          |
;; | <N       | Go to beg of right sibling paren pair          |
;; | >p       | Open at end of previous sibling paren pair     |
;; | <p       | Open at beg of previous sibling paren pair     |
;; | >P       | Go to end of previous sibling paren pair       |
;; | <P       | Go to beg of previous sibling paren pair       |

;;; Code:
(require 'evil)

;;;; Variables
(defcustom evil-lispops-open-inside t
  "If non-nil, evil-lispops-open-* commands open inside the bracket.
If nil, they open outside the bracket."
  :type 'boolean
  :group 'evil-lispops)

;;;; Helper Functions
(defun evil-lispops--get-range (&optional n inclusivep)
  "Get the value of points at ends of a paren pair.
Uses `evil-select-paren’. Accepts count N.  INCLUSIVEP determines
whether range is inside the paren block or outside."
  (evil-select-paren ?\( ?\) (point) 0 nil (or n 1) inclusivep))

;;;; Operators:
(defun evil-lispops-goto-beg (&optional n inclusivep)
  "Go to beginning of Nth paren pair.
INCLUSIVEP for cursor placement style."
  (interactive)
  (when (looking-at ")")
    (goto-char (- (point) 1)))
  (goto-char (car (evil-lispops--get-range (or n 1) inclusivep))))

(defun evil-lispops-goto-end (&optional n inclusivep)
  "Go to end of Nth paren pair.
INCLUSIVEP for cursor placement style."
  (interactive)
  (when (looking-at ")")
    (goto-char (- (point) 1)))
  (goto-char (cadr (evil-lispops--get-range (or n 1) inclusivep))))

(defun evil-lispops-open-beg ()
  "Open at beginning of paren pair."
  (interactive)
  (evil-lispops-goto-beg nil (if evil-lispops-open-inside nil t))
  (evil-insert 0))

(defun evil-lispops-open-end ()
  "Open at end of paren pair."
  (interactive)
  (evil-lispops-goto-end nil (if evil-lispops-open-inside nil t))
  (evil-insert 0))

(defun evil-lispops-goto-parent-beg (&optional n inclusivep)
  "Go to beginning of Nth parent paren pair.
INCLUSIVEP for cursor placement style."
  (interactive "P")
  (when (looking-at "(")
    (goto-char (1+ (point))))
  (evil-lispops-goto-beg (1+ (or n 1)) inclusivep))

(defun evil-lispops-goto-parent-end (&optional n inclusivep)
  "Go to end of Nth parent paren pair.
INCLUSIVEP for cursor placement style."
  (interactive "P")
  (evil-lispops-goto-end (1+ (or n 1)) inclusivep))

(defun evil-lispops-open-parent-beg (&optional n)
  "Open at beginning Nth of parent paren pair."
  (interactive "P")
  (evil-lispops-goto-parent-beg (or n 1) (if evil-lispops-open-inside nil t))
  (evil-lispops-open-beg))

(defun evil-lispops-open-parent-end (&optional n)
  "Open at end of Nth parent paren pair."
  (interactive "P")
  (let ((point (point)))
    (evil-lispops-goto-parent-end (or n 1) (if evil-lispops-open-inside nil t))
    (unless (eq (point) point)
      (evil-lispops-open-end))))

(defun evil-lispops-goto-child-beg (&optional n reversep relativep)
  "Go to beginning of Nth child paren pair.  REVERSEP to go to end.
RELATIVEP for relative child."
  (interactive "P")
  (let ((count (or n 1))
        (point (point))
        (parentrange (evil-lispops--get-range)))
    (unless relativep (evil-lispops-goto-beg))
    (while (> count 0)
      (while (and (not (looking-at "("))
                  (> (cadr parentrange) (point)))
        (evil-forward-WORD-begin))
      (evil-lispops-goto-beg nil t)
      (when (not (= count 1))
        (evil-lispops-goto-end)
        (goto-char (+ (point) 1)))
      (cl-decf count))
    (if (looking-at "(")
        (progn (goto-char (+ (point) 1))
               (when reversep (evil-lispops-goto-end)))
      (goto-char point))))

(defun evil-lispops-goto-child-end (&optional n)
  "Go to end of Nth child paren pair."
  (interactive "P")
  (evil-lispops-goto-child-beg (or n 1) t))

(defun evil-lispops-open-child-beg (&optional n)
  "Open at beginning of Nth child paren pair."
  (interactive "P")
  (let ((point (point)))
    (evil-lispops-goto-child-beg (or n 1))
    (unless (eq (point) point)
      (evil-lispops-open-beg))))

(defun evil-lispops-open-child-end (&optional n)
  "Open at end of Nth child paren pair."
  (interactive "P")
  (let ((point (point)))
    (evil-lispops-goto-child-end (or n 1))
    (unless (eq (point) point)
      (evil-lispops-open-end))))

(defun evil-lispops-goto-right-adjacent-child-beg (&optional n)
  "Go to beginning of Nth right adjacent child paren pair."
  (interactive "P")
  (evil-lispops-goto-child-beg (or n 1) nil t))

(defun evil-lispops-goto-right-adjacent-child-end (&optional n)
  "Go to end of Nth right adjacent child paren pair."
  (interactive "P")
  (evil-lispops-goto-child-beg (or n 1) t t))

(defun evil-lispops-open-right-adjacent-child-beg (&optional n)
  "Open at beginning of Nth right adjacent child paren pair."
  (interactive "P")
  (let ((point (point)))
    (evil-lispops-goto-child-beg (or n 1) nil t)
    (unless (eq (point) point)
      (evil-lispops-open-beg))))

(defun evil-lispops-open-right-adjacent-child-end (&optional n)
  "Open at end of Nth right adjacent child paren pair."
  (interactive "P")
  (let ((point (point)))
    (evil-lispops-goto-child-beg (or n 1) t t)
    (unless (eq (point) point)
      (evil-lispops-open-end))))

(defun evil-lispops-goto-left-adjacent-child-beg (&optional n)
  "Go to beginning of Nth left adjacent child paren pair."
  (interactive "P")
  (let ((count (or n 1))
        (point (point))
        (parentrange (evil-lispops--get-range)))
    (while (> count 0)
      (while (and (not (looking-at ")"))
                  (> (point) (car parentrange)))
        (evil-backward-WORD-end))
      (evil-lispops-goto-beg nil t)
      (cl-decf count)
      (if (looking-at "(")
          (evil-lispops-goto-beg)
        (if (> (point) (car parentrange))
            (goto-char point))))))

(defun evil-lispops-goto-left-adjacent-child-end (&optional n)
  "Go to end of Nth left adjacent child paren pair."
  (interactive "P")
  (evil-lispops-goto-left-adjacent-child-beg (or n 1))
  (evil-lispops-goto-end))

(defun evil-lispops-open-left-adjacent-child-beg (&optional n)
  "Open at beginning of Nth left adjacent child paren pair."
  (interactive "P")
  (evil-lispops-goto-left-adjacent-child-end (or n 1))
  (evil-lispops-open-beg))

(defun evil-lispops-open-left-adjacent-child-end (&optional n)
  "Open at end of Nth left adjacent child paren pair."
  (interactive "P")
  (evil-lispops-goto-left-adjacent-child-beg (or n 1))
  (evil-lispops-open-end))

(defun evil-lispops-goto-right-sibling-beg (&optional n reversep)
  "Go to beginning of Nth right sibling paren pair.  REVERSEP to go to end."
  (interactive "P")
  (let ((count (or n 1))
        (point (point)))
    (evil-lispops-goto-end)
    (goto-char (+ (point) 1))
    (if (looking-at ")")
        (goto-char point)
      (if reversep
          (evil-lispops-goto-right-adjacent-child-end count)
        (evil-lispops-goto-right-adjacent-child-beg count)))))

(defun evil-lispops-goto-right-sibling-end (&optional n)
  "Go to end of Nth right sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-right-sibling-beg (or n 1) t))

(defun evil-lispops-open-right-sibling-beg (&optional n)
  "Open at end of Nth right sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-right-sibling-beg (or n 1))
  (evil-lispops-open-beg))

(defun evil-lispops-open-right-sibling-end (&optional n)
  "Go to end of Nth right sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-right-sibling-beg (or n 1) t)
  (evil-lispops-open-end))

(defun evil-lispops-goto-left-sibling-beg (&optional n reversep)
  "Go to beginning of Nth left sibling paren pair.  REVERSEP to go to end."
  (interactive "P")
  (let ((count (or n 1))
        (point (point)))
    (evil-lispops-goto-beg nil t)
    (if (looking-back "(" nil)
        (goto-char point)
      (goto-char (- (point) 1))
      (if reversep
          (evil-lispops-goto-left-adjacent-child-end count)
        (evil-lispops-goto-left-adjacent-child-beg count)))))

(defun evil-lispops-goto-left-sibling-end (&optional n)
  "Go to end of Nth left sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-left-sibling-beg (or n 1) t))

(defun evil-lispops-open-left-sibling-beg (&optional n)
  "Open at end of Nth left sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-left-sibling-beg (or n 1))
  (evil-lispops-open-beg))

(defun evil-lispops-open-left-sibling-end (&optional n)
  "Go to end of Nth left sibling paren pair."
  (interactive "P")
  (evil-lispops-goto-left-sibling-beg (or n 1) t)
  (evil-lispops-open-end))

;;;; Mode
(defvar evil-lispops-mode-map (make-sparse-keymap)
  "Keymap used by `evil-lispops-mode’.")

(defun evil-lispops--bind-keys ()
  "Bind keys for `evil-lispops-mode’."
  (evil-define-key 'normal evil-lispops-mode-map
    (kbd ">>") #'evil-shift-right
    (kbd "<<") #'evil-shift-left
    (kbd ">.") #'evil-lispops-goto-end
    (kbd "<.") #'evil-lispops-goto-beg
    (kbd ">i") #'evil-lispops-open-end
    (kbd "<i") #'evil-lispops-open-beg
    (kbd ">j") #'evil-lispops-open-child-end
    (kbd "<j") #'evil-lispops-open-child-beg
    (kbd ">J") #'evil-lispops-goto-child-end
    (kbd "<J") #'evil-lispops-goto-child-beg
    (kbd ">k") #'evil-lispops-open-parent-end
    (kbd "<k") #'evil-lispops-open-parent-beg
    (kbd ">K") #'evil-lispops-goto-parent-end
    (kbd "<K") #'evil-lispops-goto-parent-beg
    (kbd ">h") #'evil-lispops-open-left-adjacent-child-end
    (kbd "<h") #'evil-lispops-open-left-adjacent-child-beg
    (kbd ">H") #'evil-lispops-goto-left-adjacent-child-end
    (kbd "<H") #'evil-lispops-goto-left-adjacent-child-beg
    (kbd ">l") #'evil-lispops-open-right-adjacent-child-end
    (kbd "<l") #'evil-lispops-open-right-adjacent-child-beg
    (kbd ">L") #'evil-lispops-goto-right-adjacent-child-end
    (kbd "<L") #'evil-lispops-goto-right-adjacent-child-beg
    (kbd ">p") #'evil-lispops-open-left-sibling-end
    (kbd "<p") #'evil-lispops-open-left-sibling-beg
    (kbd ">P") #'evil-lispops-goto-left-sibling-end
    (kbd "<P") #'evil-lispops-goto-left-sibling-beg
    (kbd ">n") #'evil-lispops-open-right-sibling-end
    (kbd "<n") #'evil-lispops-open-right-sibling-beg
    (kbd ">N") #'evil-lispops-goto-right-sibling-end
    (kbd "<N") #'evil-lispops-goto-right-sibling-beg))

(evil-lispops--bind-keys)

;;;###autoload
(define-minor-mode evil-lispops-mode
  "Edit Lisp evilly.  Adds commands to edit and navigate parens."
  :lighter " lispops"
  :group 'evil-lispops)

(provide 'evil-lispops)
;; TODO: new functions for operating on paren-pair that starts / ends at Nth line.  Negative: up Positive: down 1 is 1 down, -1 is 1 up
;;; evil-lispops.el ends here
