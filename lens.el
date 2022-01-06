;;; lens.el --- Mirror a region of text in multiple locations  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Tillou

;; Author: Adam Tillou <qaiviq@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A 'lens' is a region of text which is linked to other lenses
;; displaying the same text, so that a change to one lens updates
;; all other lenses that it is linked to.  Each lens is marked by an
;; overlay surrounding the region taken up by the lens.
;; A 'chain' is a group of lenses that are linked to each other

;;; Code:

;; Creating/deleting lenses and lens chains -----------------------

(defvar lens-chains nil
  "Global list of all active chains.

Format of a chain: (chain (OVERLAYS...) TEXT PROPERTIES...)")

(defun lens-create-chain ()
  "Create a new lens chain."
  (let ((chain (list 'chain nil)))
    (push chain lens-chains)
    chain))

(defun lens-delete-chain (chain)
  "Delete the lens chain CHAIN and every lens in it."
  (dolist (lens (cadr chain))
    (lens-delete-lens lens t))
  (setq lens-chains (remove chain lens-chains)))

(defun lens-create (chain beg end)
  "Add a new lens to the existing lens chain CHAIN.

Lens will be a new lens from BEG to END in the current buffer."
  ;; Make sure there are no overlapping lenses
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'lens)
      (error "Another lens exists in the specified area")))
  (let ((o (make-overlay beg end nil nil t)))
    (push o (cadr chain))
    (overlay-put o 'lens chain)))

(defun lens-delete (lens &optional dont-remove)
  "Delete LENS and remove it from its lens chain.

If DONT-REMOVE is non-nil, don't remove the lens from its chain."
  (let ((chain (overlay-get lens 'lens)))
    (unless dont-remove
      (setf (cadr chain) (remove lens (cadr chain)))))
  (delete-overlay lens))

;; Functions for using lenses -------------------------------------

(defvar lens-inhibit-update nil
  "Used to avoid infinite loops when updating a chain of lenses.")

(defun lens-update-chain (chain text &optional skip)
  "Update each lens in CHAIN to show the text TEXT.
If SKIP is specified, it is a single lens in the chain that will
not be updated, to avoid rewriting the current lens."
  (unless lens-inhibit-update
    (let ((lens-inhibit-update t))
      (dolist (lens (cadr chain))
        (unless (eq lens skip)
          (with-current-buffer (overlay-buffer lens)
            (save-excursion
              (goto-char (overlay-start lens))
              (delete-region (point) (overlay-end lens))
              (insert text))))))))

(provide 'lens)

;;; lens.el ends here
