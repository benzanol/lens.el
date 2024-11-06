;;; lens.el -*- lexical-binding: t; -*-

(defvar lens-catch-display-errors nil)


;;; Utilities
;;;; Utils

(defmacro lens-save-position (&rest body)
  `(let* ((line (line-number-at-pos)) (col (current-column)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)))

(defun lens--refresh-buffer (&optional beg end)
  ;; Refresh org indent mode
  (when (and (boundp 'org-indent-mode) org-indent-mode)
    (if (and beg end) (org-indent-add-properties beg end)
      (org-indent-indent-buffer))))

(defmacro lens--inhibit (&rest body)
  `(let ((inhibit-read-only t) (inhibit-modification-hooks t))
     ,@body))

;;;; Dealing with Regions

(defun lens--region-search-forward (beg-prop end-prop &optional val)
  (let* ((beg (text-property-search-forward beg-prop val (when val #'eq)))
         (beg-val (when beg (prop-match-value beg)))
         (end (when beg-val (text-property-search-forward end-prop beg-val #'eq))))
    (when end
      (list beg-val
            (prop-match-beginning beg) (prop-match-end beg)
            (prop-match-beginning end) (prop-match-end end)))))

(defun lens--region-at-point (beg-prop end-prop &optional noerror)
  (let* ((start (point))
         (end-match nil)
         (pred (lambda (n val)
                 (when val
                   (let ((m (save-excursion (setq end-match (text-property-search-forward end-prop val #'eq)))))
                     (and m (>= (prop-match-end m) start))))))
         (beg-match (save-excursion (text-property-search-backward beg-prop nil pred))))
    (if (null beg-match) (unless noerror (error "No lens at point"))
      (list (prop-match-value beg-match)
            (prop-match-beginning beg-match) (prop-match-end beg-match)
            (prop-match-beginning end-match) (prop-match-end end-match)))))

(defmacro lens--edit-region (beg-prop end-prop val var &rest body)
  (declare (indent 4))
  `(save-excursion
     (beginning-of-buffer)
     (pcase-let* ((,var (or (lens--region-search-forward ,beg-prop ,end-prop ,val)
                            (error "Region not found"))))
       (lens--inhibit (atomic-change-group ,@body)))))

;;;; Lens Things

(defmacro lens--edit (lens var &rest body)
  `(let ((lens ,lens))
     (with-current-buffer (if lens (get lens :buffer) (current-buffer))
       (lens--edit-region 'lens-begin 'lens-end lens ,var . ,body))))

(defun lens-at-point (&optional noerror)
  (lens--region-at-point 'lens-begin 'lens-end noerror))


;;; Insert/Remove/Filter
;;;; Inserting

(defun lens--call-source (lens prop)
  (funcall (plist-get (get lens :source) prop) lens))

;; Set the text (internal state) of an existing lens
(defvar lens--being-modified nil)
(defun lens--set-text (lens newtext &rest flags)
  (message "Editing %s %s" lens %s lens--being-modified)
  (unless lens--being-modified
    (run-with-timer 0 nil #'set 'lens--being-modified nil))
  (push lens lens--being-modified)

  (when (get lens :dead) (error "Cannot modify dead lens"))
  (put lens :text newtext)
  (when (memq :save flags) (lens--call-source lens :save))
  (when (memq :refresh flags)
    (lens-save-position
     (lens--edit lens `(_ ,hb ,he ,fb ,fe)
       (goto-char he)
       (delete-region he fb)
       (insert (lens--generate-insert-text lens))
       (lens--refresh-buffer he (point))))))

;; Generate the ui of a lens
(defun lens--generate-insert-text (lens)
  (let ((settext `(lambda (newtext &optional refresh)
                    (lens--set-text ',lens newtext :save (when refresh :refresh))))
        (text (get lens :text))
        (func (get lens :display)))
    (if (not lens-catch-display-errors)
        (funcall func text settext)
      (condition-case err (funcall func text settext)
        (error (propertize (format "%s" err) 'face 'error 'font-lock-face 'error))))))

(defun lens-insert (source display)
  (when (lens-at-point t) (error "Cannot create a lens inside of another lens"))
  (when lens--buffer-lenses (error "Cannot create a lens in a buffer which is viewed by a lens"))

  (let* ((head-ps '(rear-nonsticky t read-only t face lens-head))
         (caption "caption")
         (header (format "#+begin_lens %s" caption))
         (footer (format "#+end_lens" caption))

         (lens (let ((sym (make-symbol (format "lens-%04d" (random 99999)))))
                 (put sym :source source)
                 (put sym :display display)
                 (put sym :buffer (current-buffer))
                 sym))
         (init-text (put lens :text (lens--call-source lens :init)))
         (insert (lens--generate-insert-text lens))
         (start (point)))

    (lens--inhibit
     (atomic-change-group
       (insert (apply #'propertize (format "\n%s\n" header) 'lens-begin lens head-ps))
       (insert insert)
       (insert (apply #'propertize (format "\n%s\n" footer) 'lens-end lens head-ps))))

    ;; Add hooks
    (add-hook #'filter-buffer-substring-functions #'lens--filter-buffer-substring nil 'local)
    (add-hook 'before-save-hook #'lens--before-save nil 'local)

    (lens--refresh-buffer start (point))))


;;;; Removing

(defun lens-remove (lens)
  (interactive (list (car (lens-at-point))))
  (lens--edit lens `(,lens ,hb ,he ,fb ,fe)
    (delete-region hb fe)
    (insert (lens--call-source lens :replace))
    (lens--refresh-buffer)))

(defun lens-remove-all ()
  (interactive)
  (ignore-errors
    (while t
      (lens--edit nil `(,lens ,hb ,he ,fb ,fe)
        (delete-region hb fe)
        (insert (lens--call-source lens :replace)))))
  (lens--refresh-buffer))


;;;; Filters

(defun lens--remove-lenses-from-string (string)
  ;; If the string contains the beginning or end of a lens, then remove lenses
  (if (or (text-property-not-all 0 (length string) 'lens-begin nil string)
          (text-property-not-all 0 (length string) 'lens-end nil string))
      (with-temp-buffer
        (lens--inhibit
         (insert string)
         (lens-remove-all)
         (buffer-string)))

    ;; Either it contains no lenses, or just contains some contents of a lens
    string))

(defvar-local lens-presave nil)

(defun lens--before-save ()
  (when (text-property-not-all (point-min) (point-max) 'lens-begin nil)
    (setq lens-presave (list :text (buffer-string) :pos (point)))
    (lens-remove-all)
    (add-hook 'after-save-hook #'lens--after-save-once nil 'local)))

(defun lens--after-save-once ()
  (when lens-presave
    (lens--inhibit
     (delete-region (point-min) (point-max))
     (insert (plist-get lens-presave :text))
     (goto-char (plist-get lens-presave :pos))

     (remove-hook 'after-save-hook #'lens--after-save-once 'local)
     (setq lens-presave nil)

     (set-buffer-modified-p nil))))




(defun lens--filter-buffer-substring (fun start end delete)
  (lens--remove-lenses-from-string (funcall fun start end delete)))


;;; Some Sources and Displays
;;;; Region Source

(defun lens-replace-source (str)
  (list :init (lambda (lens) str)
        :save (lambda (lens))
        :replace (lambda (lens) (get lens :text))))


;;;; Buffer Source

(defun lens--refresh-buffer-lenses (&optional beg end len)
  (setq lens--buffer-lenses (--filter (and (symbolp it) (not (get it :dead))) lens--buffer-lenses))
  (dolist (lens lens--buffer-lenses)
    ;; Throws if the search fails
    (unless (memq lens lens--being-modified)
      (condition-case err (lens--set-text lens (buffer-substring-no-properties (point-min) (point-max)) :refresh)
        (error (message "Error refreshing lens, deleting: %s" (cadr err))
               (setq lens--buffer-lenses (delq lens lens--buffer-lenses)))))))

(defun lens--assign-lens-to-buffer (buffer lens)
  (with-current-buffer buffer
    (add-to-list 'lens--buffer-lenses lens t)
    (add-hook 'after-change-functions #'lens--refresh-buffer-lenses nil 'local)))

(defun lens--update-buffer-contents (buffer lens)
  (with-current-buffer buffer
    (lens--inhibit
     (atomic-change-group
       (remove-overlays (point-min) (point-max))
       (delete-region (point-min) (point-max))
       (insert (get lens :text)))
     (save-buffer)
     (lens--refresh-buffer-lenses))))

(defvar-local lens--buffer-lenses nil)
(defun lens-buffer-source (buf &optional replaced)
  (list :init
        (lambda (lens)
          (lens--assign-lens-to-buffer buf lens)
          (with-current-buffer buf (buffer-string)))
        :save
        (lambda (lens)
          (lens--assign-lens-to-buffer buf lens)
          (lens--update-buffer-contents buf lens))
        :replace
        (lambda (lens) (or replaced ""))))


;;;; File Source

(defvar-local lens--buffer-lenses nil)
(defun lens-file-source (file &optional replaced)
  (list :init
        (lambda (lens)
          (let ((buf (get-file-buffer file)))
            (if (null buf) (f-read file)
              (lens--assign-lens-to-buffer buf lens)
              (with-current-buffer buf (buffer-string)))))
        :save
        (lambda (lens)
          (let ((buf (get-file-buffer file)))
            (if (null buf) (write-region (get lens :text) nil file)
              (lens--assign-lens-to-buffer buf lens)
              (lens--update-buffer-contents buf lens))))
        :replace
        (lambda (lens) (or replaced ""))))


;;;; Raw display

(bz/face lens-field-header fixed-pitch :w bold :bg bg2 :fg gray3)
(defun lens--field (text settext &optional head foot)
  (setq head (or head "---") foot (or foot "---"))
  (let* ((field (make-symbol (format "field-%s" (random 99999))))
         (body (substring text))
         (head-ps '(read-only t rear-nonsticky t))
         (face-ps `(face lens-field-header font-lock-face lens-field-header . ,head-ps)))

    (fset field (lambda (beg end) (lens--field-modification-hook beg end field settext)))
    (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
      (put-text-property 0 (length body) prop (list field) body))

    (concat (apply #'propertize head 'field-begin field face-ps)
            (apply #'propertize "\n" 'field-begin field 'rear-nonsticky t head-ps)
            body
            (apply #'propertize "\n" 'field-end field head-ps)
            (apply #'propertize foot 'field-end field face-ps))))

(defun lens--field-modification-hook (beg end field settext)
  (lens--inhibit
   (dolist (prop '(modification-hooks insert-in-front-hooks insert-behind-hooks))
     (put-text-property beg end prop (list field)))
   (lens--edit-region 'field-begin 'field-end field `(,f ,hb ,he ,fb ,fe)
     (funcall settext (buffer-substring-no-properties he fb)))))


(defun lens-raw-display ()
  (lambda (text settext)
    (lens--field text settext)))


;;;; Generating Uis

(bz/face lens-button custom-button)
(bz/keys lens-button-keymap
  :sparse t
  "<return>" lens-click)

(defun lens-click ()
  (interactive)
  (let ((click (get-text-property (point) 'lens-click)))
    (if click (funcall click)
      (error "Nothing to click"))))

(defun lens-string-width (str)
  "The normal string-width just doesn't work for some reason."
  (if (boundp #'string-pixel-width)
      (/ (string-pixel-width str) (string-pixel-width "a"))
    (require 'shr)
    (/ (shr-string-pixel-width str) (shr-string-pixel-width "a"))))

(defun lens--join-columns (cols &optional sep)
  (let* ((splits (--map (split-string it "\n") cols))
         (line-ct (apply #'max (-map #'length splits)))
         (lines (make-list line-ct ""))
         max-w cell-text)
    (dolist (col-lines splits)
      (setq max-w (apply #'max (-map #'lens-string-width col-lines)))

      (dotimes (i line-ct)
        (setq cell-text (or (nth i col-lines) ""))
        (setf (nth i lines)
              (concat (nth i lines)
                      (or (unless (eq col-lines (car splits)) sep) "")
                      cell-text
                      (make-string (- max-w (lens-string-width cell-text)) ?\s)))))
    (string-join lines (propertize "\n" 'read-only t))))

(defun lens--generate-ui-element (elem cb)
  (pcase elem
    (`(box ,text ,onchange . ,plist)
     (lens--field text
                  (lambda (new)
                    (funcall (or onchange #'ignore) new)
                    (funcall cb (plist-get plist :refresh)))
                  "[begin box]" "[end box]"))

    (`(row . ,cols)
     (lens--join-columns (--map (lens--generate-ui-element it cb) cols)
                         (propertize " | " 'read-only t)))

    ;; Elements that are also valid columns

    ((or `(string ,str) (and (pred stringp) str))
     (propertize (if (stringp str) str (string-join str "\n"))
                 'read-only t))

    (`(button ,label ,onclick . ,plist)
     (propertize (format " %s " label) 'lens-click
                 (lambda ()
                   (funcall (or onclick #'ignore))
                   (funcall cb (not (plist-get plist :norefresh))))
                 'local-map (list 'keymap lens-button-keymap (current-local-map))
                 'font-lock-face (or (plist-get plist :face) 'lens-button)
                 'read-only t))))

(defun lens--generate-ui (rows cb)
  (string-join (--map (lens--generate-ui-element it cb) rows)
               (propertize "\n" 'read-only t)))

;;;; Ui Display

(defun lens-ui-display (ui)
  ;; Inited stores whether tostate has been called (since nil is a valid state value)
  (-let (((&plist :tostate tostate :totext totext :toui toui) ui)
         cur-text state)
    (lambda (text settext)
      (unless (eq cur-text text)
        (message "Creating state")
        (setq cur-text text state (funcall tostate text)))
      (lens--generate-ui
       (funcall toui state)
       (lambda (&optional refresh)
         (setq cur-text (funcall totext state))
         (funcall settext cur-text refresh))))))


;;; Usable functions
;;;; Ui Functions

(defvar lens-ui-alist nil)

(defmacro lens-defui (name &rest body)
  (declare (indent 1) (doc-string 2))
  (let ((docstring (when (stringp (car body)) (pop body))))
    (list #'setf (list #'alist-get (list 'quote name) 'lens-ui-alist)
          (cons #'list (append (list :docstring docstring) body)))))

(defun lens-read-ui (&optional prompt)
  (let ((name (completing-read (or prompt "Ui: ") (mapcar #'car lens-ui-alist))))
    (alist-get (intern name) lens-ui-alist)))

(defun lens-insert-ui (beg end ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (or (lens-read-ui) (error "No ui selected"))))
  (let ((str (buffer-substring-no-properties beg end)))
    (lens--inhibit
     (delete-region beg end)
     (lens-insert (lens-replace-source str) (lens-ui-display ui)))))

(defun lens-insert-buffer-ui (beg end buffer ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (read-buffer "Buffer") (lens-read-ui)))
  (let ((str (buffer-substring-no-properties beg end)))
    (lens--inhibit
     (delete-region beg end)
     (lens-insert (lens-buffer-source buffer str) (lens-ui-display ui)))))


;;; UIs
(lens-defui test
  :tostate
  (lambda (text)
    (list :count (length (car (split-string text "\n")))
          :rest (s-join "\n" (cdr (split-string text "\n")))))
  :totext
  (lambda (st)
    (concat (make-string (plist-get st :count) ?-) "\n" (plist-get st :rest)))
  :toui
  (lambda (st)
    `("hello"
      (row
       ,(format ">>#%s<<" (plist-get st :count))
       (button "+" ,(@0 (=> (plist-get st :count) + 1)))
       (button "-" ,(@0 (=> (plist-get st :count) - 1))))
      (box ,(plist-get st :rest)
           ,(@1 (plist-put st :rest (upcase @1)))))))

;; (lens-insert (lens-buffer-source (get-buffer "temp2.org")) (lens-raw-display))
