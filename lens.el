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

(defun lens-create-chain (&rest props)
  "Create a new lens chain.

PROPS is a list of keyword-value pairs, which can include:
:source -- A major mode or lens to serve as the parent lens"
  (let ((chain `(chain nil nil . ,props)))
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
      (overlay-put o 'insert-behind-hooks (list update-func)))
    (if (caddr chain) (lens-set-text o (caddr chain))
      (setf (caddr chain) (buffer-substring beg end)))
    o))

(defun lens-delete (&optional lens dont-remove)
  "Delete LENS and remove it from its lens chain.

If DONT-REMOVE is non-nil, don't remove the lens from its chain."
  (interactive)
  (unless lens (setq lens (lens-at)))
  (let ((chain (overlay-get lens 'lens-chain)))
    (unless dont-remove
      (setf (cadr chain) (remove lens (cadr chain)))))
  (when (lens-get lens :revert)
    (lens-set-text lens (overlay-get lens 'lens-original-text)))
  (delete-overlay lens))

;; Functions for using lenses -------------------------------------

(defun lens-set-text (lens text)
  "Update LENS to display TEXT."
  (let ((inhibit-modification-hooks t)
        (start (overlay-start lens)))
    (with-current-buffer (overlay-buffer lens)
      (save-excursion
        (delete-region start (overlay-end lens))
        (goto-char start) (insert text)
        (move-overlay lens start (+ start (length text)))))))

(defun lens-modification-hook (&rest args)
  "Function called whenever an overlay is updated with ARGS."
  ;; Only run AFTER updating the overlay, not before
  (when (cadr args)
    (let* ((o (car args))
           (text (with-current-buffer (overlay-buffer o)
                   (buffer-substring (overlay-start o) (overlay-end o))))
           (chain (overlay-get o 'lens-chain)))
      (setf (caddr chain) text)
      (dolist (lens (cadr chain))
        (unless (eq lens o)
          (lens-set-text lens text))))))

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
  (when-let ((lens (lens-at pos)))
    (overlay-get lens 'lens-chain)))

(defun lens-get (lens prop)
  "Return the property PROP of the lens at point or LENS."
  (setq lens (or lens (lens-at) (error "No lens at point")))
  (plist-get (overlay-get lens 'lens-props) prop))

(defun lens-set (lens prop val)
  "Set the value of PROP to VAL in LENS."
  (setq lens (or lens (lens-at) (error "No lens at point")))
  (overlay-put lens 'lens-props (plist-put (overlay-get lens 'lens-props) prop val)))

(defun lens-chain-get (chain prop)
  "Return the property PROP of CHAIN."
  (setq chain (or chain (lens-chain-at) (error "No chain at point")))
  (plist-get (caddr chain) prop))

(defun lens-chain-set (chain prop val)
  "Set the value of PROP to VAL in CHAIN."
  (setq chain (or chain (lens-chain-at) (error "No chain at point")))
  (setf (cdddr chain) (plist-put (cdddr chain) prop val)))

;; Integration with emacs -----------------------------------------

(defun lens-before-save-function ()
  "Function run before saving a buffer."
  (let ((os (overlays-in (point-min) (point-max))))
    (dolist (o os)
      (when (lens-get o :revert)
        (lens-set-text o (overlay-get o 'lens-original-text))))))

(defun lens-after-save-function ()
  "Function run after saving a buffer."
  (let ((os (overlays-in (point-min) (point-max))))
    (dolist (o os)
      (when (lens-get o :revert)
        (lens-set-text o (caddr (overlay-get o 'lens-chain)))))))

(add-hook 'before-save-hook 'lens-before-save-function)
(add-hook 'after-save-hook 'lens-after-save-function)

;; Highlighting the current chain ---------------------------------

(defvar lens-current-chain nil
  "The chain of the lens the cursor is currently inside, if any.")

(defface lens-highlight '((default (:inherit highlight)))
  "Face to highlight all lenses in the current chain.")

(define-minor-mode lens-highlight-mode
  "Highlight lenses in the same chain as the current lens."
  :global t
  :init-value nil
  (if lens-highlight-mode
      (add-hook 'post-command-hook 'lens-update-highlight)
    (lens-set-chain-face lens-current-chain nil)
    (remove-hook 'post-command-hook 'lens-update-highlight)))

(defun lens-set-chain-face (chain face)
  "Set the `face` property of all lenses in CHAIN to FACE."
  (dolist (lens (cadr chain))
    (overlay-put lens 'face face)))

(defun lens-update-highlight ()
  "Update highlighting for `lens-highlight-mode`."
  (when lens-highlight-mode
    (let ((chain (lens-chain-at)))
      (unless (eq chain lens-current-chain)
        (when lens-current-chain (lens-set-chain-face lens-current-chain nil))
        (when chain (lens-set-chain-face chain 'lens-highlight))
        (setq lens-current-chain chain)))))

(provide 'lens)

;;; lens.el ends here
