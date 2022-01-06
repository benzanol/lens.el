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
    (lens-delete lens t))
  (setq lens-chains (remove chain lens-chains)))

(defun lens-create (chain beg end &rest props)
  "Add a new lens from BEG to END to the existing lens chain CHAIN.

The remaining arguments are keyword-value pairs, which can include:
:revert -- Save the buffer with the original text, not lens text"

  ;; Make sure there are no overlapping lenses
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'lens)
      (error "Another lens exists in the specified area")))
  (let ((o (make-overlay beg end nil nil t)))
    (push o (cadr chain))
    (overlay-put o 'lens t)
    (overlay-put o 'lens-chain chain)
    (overlay-put o 'lens-props props)
    (overlay-put o 'lens-original-text (buffer-substring beg end))
    (let ((update-func 'lens-modification-hook))
      (overlay-put o 'modification-hooks (list update-func))
      (overlay-put o 'insert-in-front-hooks (list update-func))
      (overlay-put o 'insert-behind-hooks (list update-func)))))

(defun lens-delete (lens &optional dont-remove)
  "Delete LENS and remove it from its lens chain.

If DONT-REMOVE is non-nil, don't remove the lens from its chain."
  (let ((chain (overlay-get lens 'lens-chain)))
    (unless dont-remove
      (setf (cadr chain) (remove lens (cadr chain)))))
  (delete-overlay lens))

;; Functions for using lenses -------------------------------------

(defun lens-update-chain (chain text &optional skip)
  "Update each lens in CHAIN to show the text TEXT.
If SKIP is specified, it is a single lens in the chain that will
not be updated, to avoid rewriting the current lens."
  (let ((inhibit-modification-hooks t))
    (dolist (lens (cadr chain))
      (unless (eq lens skip)
        (with-current-buffer (overlay-buffer lens)
          (save-excursion
            (goto-char (overlay-start lens))
            (delete-region (point) (overlay-end lens))
            (insert text)))))))

(defun lens-modification-hook (&rest args)
  "Function called whenever an overlay is updated with ARGS."
  (let ((o (car args)) (after (cadr args)))
    (when after
      (with-current-buffer (overlay-buffer o)
        (lens-update-chain
         (overlay-get o 'lens-chain)
         (buffer-substring (overlay-start o) (overlay-end o))
         o)))))

;; Helpful functions ----------------------------------------------
(defun lens-at (&optional pos)
  "Return the lens at the current point, or at POS if non-nil."
  (unless pos (setq pos (point)))
  (let ((overlays (overlays-at pos)) lens)
    (while (and overlays (null lens))
      (if (overlay-get (car overlays) 'lens)
          (setq lens (pop overlays)) (pop overlays)))
    lens))

(defun lens-chain-at (&optional pos)
  "Return the lens chain corresponding to the lens at point or POS."
  (overlay-get (lens-at pos) 'lens-chain))

(defun lens-get (prop &optional lens)
  "Return the property PROP of the lens at point or LENS."
  (setq lens (or lens (lens-at) (error "No lens at point")))
  (plist-get (overlay-get lens 'lens-props) prop))

(defun lens-chain-get (prop &optional chain)
  "Return the property PROP of the chain at point or CHAIN."
  (setq chain (or chain (lens-chain-at) (error "No chain at point")))
  (plist-get (caddr chain) prop))

(provide 'lens)

;;; lens.el ends here
