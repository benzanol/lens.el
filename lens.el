;;; lens.el -*- lexical-binding: t; -*-

(require 'f)
(require 'dash)
(require 'org)
(require 'text-property-search)

(defvar lens-catch-display-errors t)

(defvar lens-idle-save-delay 1
  "When nil, don't save source buffers after modifying a lens. When
non-nil, wait for however many seconds of idle time are specified
before saving a lens' source buffer.")

(defvar lens-save-lenses-on-save t)


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

(defmacro lens-perform-edit (&rest body)
  `(let ((inhibit-read-only t) (inhibit-modification-hooks t))
     (save-excursion (atomic-change-group ,@body))))

(defun lens--make-symbol (name &rest props)
  (let ((sym (make-symbol (format "%s-%05d" name (random 99999)))))
    (while props
      (put sym (pop props) (pop props)))
    sym))

(defun lens--make-sticky (str &optional beg end)
  (put-text-property 0 1 'front-sticky beg str)
  (put-text-property (1- (length str)) (length str) 'rear-nonsticky (not end) str)
  str)

(defun lens--append-face (beg end face &optional object)
  (when face
    (let ((fn `(lambda (f)
                 (if (or (not (listp f)) (keywordp (car f)))
                     (list f ',face) (append f (list ',face))))))
      (alter-text-property beg end 'face fn object)
      (alter-text-property beg end 'font-lock-face fn object))))


;;;; Regions

(defun lens--region-search-forward (beg-prop end-prop &optional val pred)
  (let* ((beg (text-property-search-forward beg-prop val pred))
         (beg-val (when beg (prop-match-value beg)))
         (end (when beg-val (text-property-search-forward end-prop beg-val #'eq))))
    (when end
      (list beg-val
            (prop-match-beginning beg) (prop-match-end beg)
            (prop-match-beginning end) (prop-match-end end)))))

(defun lens--region-at-point (beg-prop end-prop)
  (let* ((start (point))
         (hb nil) (he nil)
         (pred (lambda (_nil val)
                 (and val
                      (setq he (previous-single-property-change (1+ (point)) beg-prop))
                      (setq hb (or (previous-single-property-change he beg-prop) (point-min)))
                      (<= hb start)
                      (eq (get-text-property hb beg-prop) val))))
         (end-match (save-excursion (text-property-search-forward end-prop nil pred))))
    (when end-match
      (list (prop-match-value end-match) hb he
            (prop-match-beginning end-match) (prop-match-end end-match)))))


(defun lens-search-forward (&optional val pred)
  (lens--region-search-forward 'lens-begin 'lens-end val pred))

(defun lens-at-point (&optional noerror)
  (or (lens--region-at-point 'lens-begin 'lens-end)
      (unless noerror (error "No lens at point"))))


;;; Lens Lifecycle
;;;; Creating

(bz/face lens-header fixed-pitch :fg blue :w bold :h 0.9 :bg bg4 :x t :o "lightskyblue4")
(bz/face lens-footer lens-header :bg bg4 :o nil :u "lightskyblue4" :h 1)
(bz/face lens-body :bg bg4 :x t)
(bz/face lens-field-header fixed-pitch :fg gray3 :h 0.9)

(defvar lens-default-style nil)

(defun lens--generate-headers (lens)
  (-let* (((&plist :head-props hps :foot-props fps :head-face hf :foot-face ff) (get (car lens) :style))
          (rand (format "%05d" (random 99999)))
          (title (funcall (or (plist-get (get (car lens) :source) :title) #'ignore)))
          (h-text (format "[%s] %s" rand (or title "")))
          (h (apply #'propertize h-text 'lens-begin lens 'read-only t hps))
          (f (apply #'propertize (format "[%s]\n" rand) 'lens-end lens 'read-only t fps)))
    (lens--append-face 0 (length h) hf h)
    (lens--append-face 0 (length f) ff f)
    (cons (lens--make-sticky h) (lens--make-sticky f))))

(defun lens--generate-insert-text (lens)
  (let* ((insert-fn (plist-get (get (car lens) :display) :insert))
         (inside (condition-case err (funcall insert-fn (car lens) (copy-tree (caddr lens)))
                   (error (unless lens-catch-display-errors (error err))
                          (propertize (format "\n%s\n" err) 'face 'error 'font-lock-face 'error))))
         (hs (lens--generate-headers lens))
         (style (get (car lens) :style)))

    ;; The first character of the insert text should be a newline,
    ;; which is styled as part of the header
    (add-text-properties 0 1 (plist-get style :head-props) inside)
    (lens--append-face 0 1 (plist-get style :head-face) inside)

    (add-text-properties 1 (length inside) (plist-get style :props) inside)
    (lens--append-face 1 (length inside) (plist-get style :face) inside)

    (concat (car hs) inside (cdr hs))))

(defun lens-create (source display &optional style)
  (when (lens-at-point t) (error "Cannot create a lens inside of another lens"))
  (when lens--buffer-referencers (error "Cannot create a lens in a buffer which is viewed by a lens"))

  (unless style (setq style lens-default-style))
  (let* ((spec (lens--make-symbol "spec" :source source :display display :style style))
         (text (funcall (plist-get source :init)))
         (state (funcall (plist-get display :tostate) text))
         (lens (list spec text state))

         (insert (lens--generate-insert-text lens)))

    (lens-perform-edit (insert insert))

    ;; Add hooks
    (add-hook #'filter-buffer-substring-functions #'lens--filter-buffer-substring nil 'local)
    (add-hook 'before-save-hook #'lens--before-save nil 'local)

    (lens--refresh-buffer (point) (+ (point) (length insert)))))


;;;; Modifying

(defun lens-modify (region new &optional external norefresh)
  "External specifies if the new value provided was a new text
value, rather than a new state value. If a new text value was
provided, then don't save (the new text is already saved)."
  (pcase-let* ((`((,spec ,oldtext ,oldstate) ,hb ,he ,fb ,fe) region)
               (display (get spec :display))
               (source (get spec :source))

               (`(,newtext ,newstate)
                (if external (list new (funcall (plist-get display :tostate) new))
                  (list (funcall (plist-get display :totext) new) new)))

               (newlens (list spec newtext newstate))
               (title (funcall (or (plist-get (get spec :source) :title) #'ignore))))

    (unless (and (string= oldtext newtext) (equal oldstate newstate))
      (lens-save-position
       (lens-perform-edit
        (add-text-properties (1+ he) fb (plist-get (get spec :style) :props))
        (lens--append-face (1+ he) fb (plist-get (get spec :style) :face))

        (if norefresh
            (let ((strs (lens--generate-headers newlens)))
              (goto-char fb) (delete-region fb fe) (insert (cdr strs))
              (lens--refresh-buffer fb (point))
              (goto-char hb) (delete-region hb he) (insert (car strs))
              (lens--refresh-buffer hb (point)))

          (let ((insert (lens--generate-insert-text newlens)))
            (delete-region hb fe)
            (insert insert)
            (lens--refresh-buffer hb (point))))))

      (unless external (funcall (plist-get source :update) newtext)))))


;;;; Removing

(defun lens-remove (region)
  (interactive (list (lens-at-point)))
  (pcase-let* ((`((,spec ,text ,state) ,hb ,he ,fb ,fe) region)
               (replace (funcall (plist-get (get spec :source) :replace) text)))
    (lens-perform-edit
     (delete-region hb fe)
     (remove-overlays (point-min) (point-max) 'lens-box-id spec)
     (insert replace))

    (lens--refresh-buffer hb (+ hb (length replace)))))

(defun lens-remove-all (&optional before)
  (interactive)
  (let (region)
    (save-excursion
      (goto-char (point-min))
      (while (setq region (lens-search-forward))
        (condition-case err (when before (funcall before (car region)))
          (error (message "Error in remove lens hook: %s" (cadr err))))
        (lens-remove region)))))


;;;; Filters

(defun lens--remove-lenses-from-string (string)
  ;; If the string contains the beginning or end of a lens, then remove lenses
  (if (or (text-property-not-all 0 (length string) 'lens-begin nil string)
          (text-property-not-all 0 (length string) 'lens-end nil string))
      (with-temp-buffer
        (lens-perform-edit
         (insert string)
         (lens-remove-all)
         (buffer-string)))

    ;; Either it contains no lenses, or just contains some contents of a lens
    string))

(defun lens--filter-buffer-substring (fun start end delete)
  (lens--remove-lenses-from-string (funcall fun start end delete)))


;;;; Saving

(defvar-local lens--presave nil)

(defun lens--before-save ()
  (when (text-property-not-all (point-min) (point-max) 'lens-begin nil)
    (setq lens--presave (list :text (buffer-string) :pos (point)))
    (add-hook 'after-save-hook #'lens--after-save-once nil 'local)
    (lens-remove-all
     '(lambda (lens)
        (let ((text (cadr lens)) (src (get (car lens) :source)))
          (funcall (plist-get src :update) text)
          (funcall (or (plist-get src :save) #'ignore) text))))))

(defun lens--after-save-once ()
  (when lens--presave
    (lens-perform-edit
     (remove-overlays (point-min) (point-max))
     (delete-region (point-min) (point-max))
     (insert (plist-get lens--presave :text)))

    (goto-char (plist-get lens--presave :pos))

    (remove-hook 'after-save-hook #'lens--after-save-once 'local)
    (setq lens--presave nil)

    (set-buffer-modified-p nil)

    (when bz/latex-enabled (org-latex-preview '(16)))))


;;; Some Sources and Displays
;;;; Region Source

(defun lens-replace-source (str &optional above below)
  (setq above (or above "") below (or below ""))
  (list :init (lambda () str)
        :update (lambda (_text))
        :replace (lambda (text) (concat above text below))))


;;;; Buffer Source

(defvar-local lens--buffer-referencers nil "List of lens buffers that reference the current buffer")
(defun lens--add-buffer-referencer (source-buf lens-buf)
  (with-current-buffer source-buf
    (add-to-list 'lens--buffer-referencers lens-buf t)
    (add-hook 'after-change-functions #'lens--update-buffer-referencers nil 'local)))

(defun lens--update-buffer-referencers (&rest _after-change-args)
  (setq lens--buffer-referencers (-filter #'buffer-live-p lens--buffer-referencers))

  (let* ((str (string-trim (buffer-substring-no-properties (point-min) (point-max)) "\n*" "\n*"))
         (src-buf (current-buffer)) (src-file buffer-file-name)
         (lens-pred (lambda (_ lens)
                      (or (eq src-buf (plist-get (get (car lens) :source) :source-buffer))
                          (and src-file (string= src-file (plist-get (get (car lens) :source) :source-file))))))
         position)

    (dolist (lens-buf lens--buffer-referencers)
      (with-current-buffer lens-buf
        (save-excursion
          (goto-char (point-min))
          (while (setq position (lens-search-forward nil lens-pred))
            (unless (string= (string-trim (cadr (car position)) "\n*" "\n*") str)
              (lens-modify position str :external))))))))

(defvar-local lens--idle-save-timer nil "Timer to save the current buffer")
(defun lens--update-source-buffer (buf text)
  (with-current-buffer buf
    (unless (string= text (buffer-string))
      ;; Update the buffer
      (lens-perform-edit
       (remove-overlays (point-min) (point-max))
       (delete-region (point-min) (point-max))
       (insert text))
      ;; Update other lenses
      (lens--update-buffer-referencers))))

(defun lens--save-source-buffer (buf)
  (with-current-buffer buf
    (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
    (setq lens--idle-save-timer nil)))


(defun lens-buffer-source (buf &optional replaced)
  (setq buf (get-buffer buf))
  (list :source-buffer buf
        :init
        (lambda ()
          (lens--add-buffer-referencer buf (current-buffer))
          (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
        :update
        (lambda (text)
          (lens--add-buffer-referencer buf (current-buffer))
          (lens--update-source-buffer buf text))
        :replace
        (lambda (_text) (or replaced ""))
        :save
        (lambda (text)
          (with-current-buffer buf (when buffer-file-name (save-buffer))))
        :title (lambda () (buffer-name buf))))


;;;; File Source

(defvar lens--file-referencers nil "Alist of files to buffers with a file source.")
(defun lens--add-file-referencer (file lens-buf)
  (setq file (expand-file-name file) lens-buf (get-buffer lens-buf))
  (let ((assoc (assoc file lens--file-referencers))
        (file-buf (get-file-buffer file)))

    (cond ((null assoc) (push (list file lens-buf) lens--file-referencers))
          ((not (memq lens-buf (cdr assoc))) (push file (cdr assoc))))

    (when file-buf (lens--add-buffer-referencer file-buf lens-buf))))

(add-hook 'find-file-hook 'lens-find-file-hook)
(defun lens-find-file-hook ()
  (let ((pair (assoc (expand-file-name buffer-file-name) lens--file-referencers)))
    (dolist (lens-buf (cdr pair))
      (lens--add-buffer-referencer (current-buffer) lens-buf))))

(defun lens-file-source (file &optional replaced)
  (setq file (expand-file-name file))
  (list :source-file file
        :init
        (lambda ()
          (lens--add-file-referencer file (current-buffer))
          (let ((buf (get-file-buffer file)))
            (if (null buf) (f-read file)
              (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))))
        :update
        (lambda (text)
          (lens--add-file-referencer file (current-buffer))
          (when (get-file-buffer file) (lens--update-source-buffer (get-file-buffer file) text)))
        :replace
        (lambda (_plist) (or replaced ""))
        :save
        (lambda (text)
          (if (null (get-file-buffer file)) (write-region text nil file)
            (with-current-buffer (get-file-buffer file) (save-buffer))))
        :title (lambda () (f-relative file default-directory))))


;;;; Raw display

(defun lens--field (text onchange &optional head foot)
  (setq head (or head "---\n") foot (or foot "\n---"))
  (let* ((field (lens--make-symbol "field"))
         (hooks '(lens--field-modification-hook)))
    (fset field onchange)
    (put-text-property (1- (length head)) (length head) 'insert-behind-hooks hooks head)

    (dolist (prop '(face font-lock-face))
      (setq head (propertize head prop 'lens-field-header)
            foot (propertize foot prop 'lens-field-header)))

    (concat (lens--make-sticky (propertize head 'field-begin field 'read-only t))
            (propertize text 'modification-hooks hooks 'insert-behind-hooks hooks)
            (lens--make-sticky (propertize foot 'field-end field 'read-only t)))))

(defvar lens--modified-fields nil "List of (:buffer BUF :pos MARKER :undo? BOOL)")
(defun lens--field-modification-hook (beg _end)
  ;; Don't run the modification hook if inside of a primitive-undo
  (push (list :buffer (current-buffer) :pos (set-marker (make-marker) beg)
              :undo (when (--find (eq (cadr it) #'primitive-undo) (backtrace-frames)) t))
        lens--modified-fields)
  (add-hook 'post-command-hook #'lens--field-modification-callback))

(defun lens--field-modification-callback ()
  (remove-hook 'post-command-hook #'lens--field-modification-callback)
  (let (saved-lenses)
    (dolist (plist (prog1 lens--modified-fields (setq lens--modified-fields nil)))
      (with-current-buffer (plist-get plist :buffer)
        (lens-perform-edit
         (goto-char (plist-get plist :pos))

         (if (plist-get plist :undo)
             ;; If it was an undo action, we only need to update the restored lens
             (pcase-let ((`(,lens ,hb ,he ,fb ,fe) (lens-at-point t)))
               (when (and lens (not (memq lens saved-lenses)))
                 (push lens saved-lenses)
                 (funcall (plist-get (get (car lens) :source) :update) (cadr lens))))

           ;; Perform the procedure in case there was a proper modification
           (pcase-let ((`(,field ,hb ,he ,fb ,fe) (lens--region-at-point 'field-begin 'field-end)))
             (when field
               (dolist (prop '(modification-hooks insert-behind-hooks))
                 (put-text-property he fb prop '(lens--field-modification-hook)))
               (goto-char he)
               (funcall field (buffer-substring-no-properties he fb))

               (lens--refresh-buffer hb fe)))))))))


(defun lens--raw-display-onchange (text)
  (let ((region (lens-at-point)))
    (lens-modify region text nil :norefresh)))

(defun lens--raw-display-insert (_spec state)
  (let ((field (lens--field state #'lens--raw-display-onchange "\n" "\n")))
    (lens--make-sticky field :before :after)))

(defun lens-raw-display (&optional forward-filter backward-filter)
  (list :tostate
        (lambda (text)
          (funcall (or forward-filter #'identity) (string-trim text "\n+" "\n+")))
        :totext (or backward-filter #'identity)
        :insert #'lens--raw-display-insert))


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

(defun lens--ui-element-to-string (elem cb)
  (pcase elem
    (`(box ,text ,onchange . ,plist)
     (lens--field text
                  (lambda (new)
                    (funcall (or onchange #'ignore) new)
                    (funcall cb (plist-get plist :refresh)))
                  "[begin box]\n" "\n[end box]"))

    (`(row . ,cols)
     (lens--join-columns (--map (lens--ui-element-to-string it cb) cols)
                         (propertize " " 'read-only t)))

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

(defun lens--ui-to-string (rows cb)
  (let ((row-strs (--map (lens--ui-element-to-string it cb) rows))
        (newline (propertize "\n" 'read-only t)))
    (concat newline (string-join row-strs newline) newline)))


;;;; Ui Display

(defun lens-ui-display (ui)
  (-let [(&plist :tostate tostate :totext totext :toui toui) ui]
    (list :tostate tostate
          :totext totext
          :insert
          (lambda (spec state-copy)
            (lens--ui-to-string
             (funcall toui state-copy)
             (lambda (&optional refresh)
               (let ((region (lens-at-point)))
                 (unless (eq (caar region) spec) (error "Incorrect lens at point"))
                 (lens-modify region state-copy nil (not refresh)))))))))


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


(defun lens--replace-create (beg end src-func display)
  (let ((str (buffer-substring-no-properties beg end)))
    (lens-perform-edit
     (delete-region beg end)
     (lens-create (funcall src-func str) display))))

(defun lens--wrapped-create (hb he fb fe src-func display)
  (let ((head (buffer-substring-no-properties hb he))
        (body (buffer-substring-no-properties he fb))
        (foot (buffer-substring-no-properties fb fe)))
    (lens-perform-edit
     (delete-region hb fe)
     (lens-create (funcall src-func body head foot) display))))


;;;; Creation Functions

(defun lens-create-buffer (beg end buffer)
  (interactive (list (point) (if mark-active (mark) (point)) (read-buffer "Buffer: ")))
  (lens--replace-create beg end (lambda (str) (lens-buffer-source buffer str)) (lens-raw-display)))

(defun lens-create-file (beg end file)
  (interactive (list (point) (if mark-active (mark) (point)) (read-file-name "File: ")))
  (lens--replace-create beg end (lambda (str) (lens-file-source file str)) (lens-raw-display)))

(defun lens-create-ui (beg end ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (or (lens-read-ui) (error "No ui selected"))))
  (lens--replace-create beg end #'lens-replace-source (lens-ui-display ui)))

(defun lens-create-buffer-ui (beg end buffer ui)
  (interactive (list (point) (if mark-active (mark) (point))
                     (read-buffer "Buffer: ") (lens-read-ui)))
  (lens--replace-create beg end (lambda (str) (lens-buffer-source buffer str)) (lens-ui-display ui)))

(defun lens-create-new-file (path)
  (interactive (list (read-file-name "Create Lens: " "lenses/")))

  (let* ((rel1 (f-relative path (f-parent buffer-file-name)))
         (rel (if (string-match-p "\\`[/~]" rel1) rel1 (concat "./" rel1)))
         (link (format "[[%s]]" rel)))
    (mkdir (f-parent path) t)
    (f-touch path)
    (find-file-noselect path t)
    (lens-create (lens-file-source path link) (lens-raw-display))))


;;; Auto Mode
;;;; Mode

(defvar-local lens-auto-matchers nil
  "Alist of a regexp to a function")

(define-minor-mode lens-auto-mode
  "Automatically create lenses based on regexps."
  :global nil
  :init-value nil
  (if (not lens-auto-mode) (lens-remove-all)
    (setq lens-auto-matchers (alist-get major-mode lens-auto-mode-alist))
    (lens-auto-insert-all)))

(defun lens-auto-search-forward ()
  (let ((init (point))
        match-pos match-assoc)
    (dolist (assoc lens-auto-matchers)
      (goto-char init)
      (and (re-search-forward (car assoc) nil :noerror)
           (or (null match-pos) (< (match-beginning 0) match-pos))
           (setq match-pos (match-beginning 0) match-assoc assoc)))
    (when match-pos
      (goto-char match-pos)
      (looking-at (car match-assoc)) ;; Set the match data
      match-assoc)))

(defun lens-auto-insert-all ()
  (interactive)
  (let (assoc end)
    (save-excursion
      (lens-remove-all)
      (goto-char (point-min))
      (while (setq assoc (lens-auto-search-forward))
        (setq end (match-end 0))
        (funcall (cadr assoc))
        (goto-char end)))))

(defun lens-auto-at-point ()
  (interactive)
  (let ((target (point))
        done assoc)
    (save-excursion
      (goto-char (point-min))
      (while (not done)
        (setq assoc (lens-auto-search-forward))
        (cond
         ;; If we have passed the target position
         ((or (null assoc) (> (match-beginning 0) target)) (error "No auto lens at point"))
         ;; If we have not yet reached the target position
         ((< (match-end 0) target) (goto-char (match-end 0)))
         ;; If we have found a lens containing the target position
         (t (funcall (cadr assoc)) (setq done t)))))))


;;;; Org Mode

(setq lens-auto--org-block-re "^#\\+begin_lens \\([-_a-zA-Z0-9]+\\).*\n\\([^z-a]*?\\)\n#\\+end_lens\n")
(defun lens-auto--org-block ()
  (let ((ui (alist-get (intern (match-string 1)) lens-ui-alist))
        (hb (match-beginning 0)) (he (match-beginning 2))
        (fb (match-end 2)) (fe (match-end 0)))
    (when ui (lens--wrapped-create hb he fb fe #'lens-replace-source (lens-ui-display ui)))))


(defun lens--org-forward-filter (text)
  (replace-regexp-in-string "^\\*+ .*$" "_\\&_" text))

(defun lens--org-backward-filter (text)
  (replace-regexp-in-string "^_\\(\\*+ .*\\)_$" "\\1" text))

(defvar lens-auto--org-link-re (format "^\\(?:%s\\)\n" org-any-link-re))
(defun lens-auto--org-file-link ()
  (let* ((beg (match-beginning 0)) (end (match-end 0))
         (context (org-element-context))
         (link-type (and (eq (car context) 'link) (org-element-property :type context)))
         (path (and link-type (string= link-type "file") (org-element-property :path context))))
    (when path
      (lens--replace-create beg end (lambda (str) (lens-file-source path str))
                            (lens-raw-display #'lens--org-forward-filter
                                              #'lens--org-backward-filter)))))


(setq lens-auto-matchers:org-mode
      `((,lens-auto--org-link-re lens-auto--org-file-link)
        (,lens-auto--org-block-re lens-auto--org-block)))


;;;; Mode Alist

(setq lens-auto-mode-alist
      `((org-mode . ,lens-auto-matchers:org-mode)))


;;; Boxes

(bz/face lens-box-overline :o "lightskyblue4")
(bz/face lens-box-underline :u (:color "lightskyblue4"))

(bz/face lens-box-body :bg bg4 :x t)
(bz/face lens-box-head (lens-box-overline fixed-pitch) :x t :h 0.9 :fg blue :bg bg4 :w bold)
(bz/face lens-box-foot (lens-box-underline fixed-pitch) :x t :h 1 :fg bg :bg bg4)

(bz/face lens-box-vert :bg "lightskyblue4")
(bz/face lens-box-padding :bg bg4)

(setq lens-box-margin 5)
(setq lens-box-padding 8)
(setq lens-line-prefix
      (let* ((space (lambda (w) (propertize " " 'display `(space :width (,w))))))
        (concat (funcall space lens-box-margin)
                (propertize (funcall space 1) 'face 'lens-box-vert)
                (propertize (funcall space lens-box-padding) 'face 'lens-box-padding))))
(setq lens-head-prefix (let ((s (substring lens-line-prefix)))
                         (prog1 s (add-face-text-property 2 3 'lens-box-overline nil s)
                                (add-face-text-property 2 3 '(:height 1.2) nil s))))
(setq lens-foot-prefix (let ((s (substring lens-line-prefix)))
                         (prog1 s (add-face-text-property 2 3 'lens-box-underline nil s)
                                (add-face-text-property 0 3 '(:height 1) nil s))))

(setq lens-box-style
      (list :props (list 'bz/line-prefix lens-line-prefix 'bz/wrap-prefix lens-line-prefix)
            :face 'lens-box-body
            :head-props (list 'bz/line-prefix lens-head-prefix 'bz/wrap-prefix lens-head-prefix)
            :head-face 'lens-box-head
            :foot-props (list 'bz/line-prefix lens-foot-prefix 'bz/wrap-prefix lens-foot-prefix)
            :foot-face 'lens-box-foot))

(setq lens-default-style lens-box-style)
;; (setq lens-default-style nil)

;;; UIs
(lens-defui test
  :tostate
  (lambda (text)
    (list :count (length (car (split-string text "\n")))
          :rest (s-join "\n" (cdr (split-string text "\n")))))
  :totext
  (lambda (st)
    (concat (make-string (max 0 (plist-get st :count)) ?-) "\n" (plist-get st :rest)))
  :toui
  (lambda (st)
    `((row
       (button "/2" ,(@0 (=> (plist-get st :count) / 2)))
       (button "-" ,(@0 (=> (plist-get st :count) - 1)))
       ,(format "#%s" (plist-get st :count))
       (button "+" ,(@0 (=> (plist-get st :count) + 1)))
       (button "x2" ,(@0 (=> (plist-get st :count) * 2))))
      (box ,(plist-get st :rest)
           ,(@1 (plist-put st :rest @1))))))

;; (lens-insert (lens-buffer-source (get-buffer "temp2.org")) (lens-raw-display))
