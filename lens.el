;;; lens.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'f)
(require 'org)
(require 'text-property-search)


;;; Variables
;;;; User Variables

(defvar lens-catch-display-errors t
  "Non-nil if display errors should be caught.

If a display function throws an error, the lens body will show
the error text instead of the normal contents of the lens.

If nil, the error will be thrown as normal, and the lens
redisplay will be cancelled.")

(defvar lens-save-lenses-on-save t
  "If non-nil, save lens source buffers when saving a buffer.")


;;;; Internal Variables

(defvar-local lens--buffer-referencers nil
  "List of lens buffers that reference the current buffer.")

(defvar lens--modified-fields nil
  "List of (:buffer BUF :pos MARKER :undo? BOOL).")


;;; Utilities
;;;; Utils

(defmacro lens-save-position (&rest body)
  "Save the line and column of the cursor when executing BODY."
  `(let* ((line (line-number-at-pos)) (col (current-column))
          (ui-elem (lens--ui-element-at-point)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char col)))

(defmacro lens-save-position-in-ui (&rest body)
  "Save the current position relative to the containing ui element."
  `(let* ((line (line-number-at-pos)) (col (current-column))
          mb me id ui-key ui-line region)
     (save-excursion
       (and
        (setq me (text-property-search-forward 'lens-element-key))
        ;; If the next prop match is a start, then we weren't inside of an element
        (not (eq (prop-match-value me) 'START))
        (setq mb (progn (forward-char -1)
                        (text-property-search-backward 'lens-element-key)))
        ;; We are inside of a component, so set the relevant values
        (setq ui-line (- line (line-number-at-pos))
              ui-key (prop-match-value me))))
     ,@body
     (if (and ui-key (setq region (lens-at-point))
              (progn (goto-char (caddr region)) ; Go to the end of the lens header
                     ;; Go to the end of the key region
                     (text-property-search-forward 'lens-element-key ui-key #'eq))
              ;; Go to the beginning of the key region
              (progn (forward-char -1) (text-property-search-backward 'lens-element-key)))
         (progn (forward-line ui-line)
                (forward-char col))
       (goto-char (point-min))
       (forward-line (1- line))
       (forward-char col))))

(defun lens--refresh-buffer (&optional beg end)
  "Called after modifying a buffer region between BEG and END."

  ;; Refresh org indent mode
  (when (and (boundp 'org-indent-mode) org-indent-mode)
    (if (and beg end) (org-indent-add-properties beg end)
      (org-indent-indent-buffer))))

(defmacro lens-silent-edit (&rest body)
  "Perform the changes in BODY without any consequences."
  `(let ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t))
     (save-excursion (atomic-change-group ,@body))))

(defun lens--make-symbol (name &rest props)
  "Create a custom symbol with the base name NAME.

PROPS is a plist of properties to apply to the symbol."
  (let ((sym (make-symbol (format "%s-%05d" name (random 99999)))))
    (while props
      (put sym (pop props) (pop props)))
    sym))

(defun lens--make-sticky (str &optional beg end)
  "Set the stickiness properties of the edges of STR.

The beginning will be sticky if BEG is non-nil, and the end will
be sticky if END is non-nil. Thus, the default behavior is
actually to make both ends of STR not sticky."
  (put-text-property 0 1 'front-sticky beg str)
  (put-text-property (1- (length str)) (length str) 'rear-nonsticky (not end) str)
  str)

(defun lens--append-face (beg end face &optional object)
  "Append FACE to both the `face' and `font-lock-face' properties.

If the face already exists, it will not be added.

BEG and END are the beginning and end of the region the face is
added to. If OBJECT is a string, add the properties to it instead
of the buffer."
  (when face
    (let ((fn `(lambda (f)
                 (cond ((eq ',face f) f)
                       ((or (not (listp f)) (keywordp (car f))) (list f ',face))
                       ((memq ',face f) f)
                       ((append f (list ',face)))))))
      (alter-text-property beg end 'face fn object)
      (alter-text-property beg end 'font-lock-face fn object))))


;;;; Regions

(defun lens--region-search-forward (beg-prop end-prop &optional val pred)
  "Search forward for a region, and return its value and range.

A region consists of a header, a body, and a footer. The header
and footer are regions which have non-nil and equal values of
BEG-PROP and END-PROP respectively. The body is the area in
between the header and footer.

Returns (val, head-start, head-end, foot-start, foot-end), where
val is the value of the region, and the rest are integers
representing the bounds of the region.

VAL and PRED can be used to search for a particular region,
following the same convention as `text-property-search-forward'."

  (let* ((inhibit-point-motion-hooks t)
         (beg (text-property-search-forward beg-prop val pred))
         (beg-val (when beg (prop-match-value beg)))
         (end (when beg-val (text-property-search-forward end-prop beg-val #'eq))))
    (when end
      (list beg-val
            (prop-match-beginning beg) (prop-match-end beg)
            (prop-match-beginning end) (prop-match-end end)))))

(defun lens--region-at-point (beg-prop end-prop)
  "Search for a region containing the point.

The bounds of the region are specified by BEG-PROP and END-PROP,
as described by `lens--region-search-forward'."
  (let* ((inhibit-point-motion-hooks t)
         (start (point))
         (hb nil) (he nil)
         (pred (lambda (_nil val)
                 (and val
                      ;; Check if the previous region of BEG-PROP
                      ;; matches the value of END-PROP
                      (setq he (previous-single-property-change (1+ (point)) beg-prop))
                      (setq hb (or (previous-single-property-change he beg-prop) (point-min)))
                      (<= hb start)
                      (eq (get-text-property hb beg-prop) val))))
         ;; Search forward for values of END-PROP, and at each match,
         ;; check if it is the end of a region containing the point
         (end-match (save-excursion (text-property-search-forward end-prop nil pred))))
    (when end-match
      (list (prop-match-value end-match) hb he
            (prop-match-beginning end-match) (prop-match-end end-match)))))


(defun lens-search-forward (&optional val pred)
  "Search forward for a lens region.

VAL and PRED can be used to search for a particular region,
following the same convention as `text-property-search-forward'."
  (lens--region-search-forward 'lens-begin 'lens-end val pred))

(defun lens-at-point (&optional noerror)
  "Return the region of the lens containing the point.

If NOERROR is nil, throw an error if no lens is found."
  (or (lens--region-at-point 'lens-begin 'lens-end)
      (unless noerror (error "No lens at point"))))


;;; Lens Lifecycle
;;;; Creating

(defvar lens-default-style nil
  "Style to use for lenses if no other style is specified.")

(defun lens--generate-headers (lens)
  "Generate the header and footer strings for LENS.

Returns (HEAD-STRING . FOOT-STRING).

The footer contains a trailing newline, but the header does not.

To ensure that the undo-history stores the begin-lens and
end-lens properties, the header and footer contain a random
number. This ensures that the undo system knows that it has
changed, ensuring that it stores the new lens value."
  (-let* (((&plist :head-props hps :foot-props fps :head-face hf :foot-face ff) (get (car lens) :style))
          (rand (format "%05d" (random 99999)))
          (title (funcall (or (plist-get (get (car lens) :source) :title) #'ignore)))
          (h-text (format "[%s] %s" rand (or title "")))
          (h (apply #'propertize h-text 'lens-begin lens 'read-only t hps))
          (f (apply #'propertize (format "[%s]\n" rand) 'lens-end lens 'read-only t fps)))
    ;; Add the head-face and foot-face specified in the style
    (lens--append-face 0 (length h) hf h)
    (lens--append-face 0 (length f) ff f)
    ;; Make the header and footer non-sticky.
    (cons (lens--make-sticky h) (lens--make-sticky f))))

(defun lens--generate-insert-text (lens)
  "Generate the full insert string for LENS."

  (let* ((insert-fn (plist-get (get (car lens) :display) :insert))
         ;; Catch display errors if `lens-catch-display-errors' is non-nil
         (inside (if (not lens-catch-display-errors)
                     (funcall insert-fn (caddr lens))
                   (condition-case err (funcall insert-fn (caddr lens))
                     (error (propertize (format "\nError on insert: %s\n" err)
                                        'face 'error 'font-lock-face 'error)))))
         (hs (lens--generate-headers lens))
         (style (get (car lens) :style)))

    ;; The first character of the insert text should be a newline,
    ;; which is styled as part of the header
    (add-text-properties 0 1 (plist-get style :head-props) inside)
    (lens--append-face 0 1 (plist-get style :head-face) inside)

    ;; Style the body of the insert text
    (add-text-properties 1 (length inside) (plist-get style :props) inside)
    (lens--append-face 1 (length inside) (plist-get style :face) inside)

    (concat (car hs) inside (cdr hs))))

(defun lens-create (source display &optional style)
  "Insert a new lens into the current buffer.

A lens is a way to display and interact with text data from some
source location (e.g. a buffer, file, region, etc).

A lens object is a 3-tuple: (SPEC, TEXT, STATE). SPEC stores the
SOURCE, DISPLAY, and STYLE objects. TEXT is the raw textual data from the
source location. STATE is parsed from the text data.


SOURCE describes how to keep the lens text in sync with the
source of the data (the buffer/file/etc). It is a plist
containing the following functions:

:init() -> STRING - Return the initial text data from the source.
This is only called when the lens is first created.

:update(TEXT) - Update the source location with the new lens content.
Called with the new lens text whenever the lens is modified.

:replace(TEXT) -> STRING - When the lens is removed, the output
will be inserted where the lens used to be.

:save?(TEXT) - When non-nil, this will be called whenever a save
signal is sent to the lens (e.g. by saving the buffer the lens is
in, if `lens-save-lenses-on-save' is non-nil.)

:title?() -> STRING - Returns a title to include in the lens header.


DISPLAY describes how to translate between the lens text, lens
state, and the output actually displayed by the lens. It is a
plist with the following functions:

:tostate(TEXT) -> STATE - Generate the lens state from the text.
This is only called when the lens is first created, or when a
modification to the data source updates the lens.

:totext(STATE) -> TEXT - Generate the lens text from the state.
This is called whenever the lens is modified.

:insert(STATE) -> STRING - Generate the actual string to display
in the buffer. This is called whenever the lens is redisplayed.


STYLE describes custom formatting for the lens. If STYLE is nil,
the value of `lens-default-style' will be used in its place.
The following are valid style properties:

:face, :head-face, :foot-face - Specify a face to be appended to
the `face' and `font-lock-face' properties.

:props, :head-props, :foot-props - A plist of text properties to
be set on the corresponding regions. Avoid using this to set the
face, as it will trample all other faces in the region."

  (when (lens-at-point t) (error "Cannot create a lens inside of another lens"))
  ;; If `lens--buffer-referencers' is non-nil, then there is a lens
  ;; somewhere referencing the current buffer
  (when lens--buffer-referencers (error "Cannot create a lens in a buffer which is viewed by a lens"))

  (unless style (setq style lens-default-style))
  (let* ((spec (lens--make-symbol "spec" :source source :display display :style style))
         (text (funcall (plist-get source :init)))
         (state (funcall (plist-get display :tostate) text))
         (lens (list spec text state))

         (insert (lens--generate-insert-text lens)))

    (lens-silent-edit (insert insert))

    ;; Before save
    (add-hook 'before-save-hook #'lens--before-save nil 'local)
    ;; Buffer substring filter
    (make-local-variable 'filter-buffer-substring-functions)
    (add-to-list 'filter-buffer-substring-functions #'lens--filter-buffer-substring-function)

    (lens--refresh-buffer (point) (+ (point) (length insert)))))

(defun lens--filter-buffer-substring-function (func beg end &optional delete)
  "Remove lenses from the region"
  (let ((str (funcall func beg end delete)))
    (or (ignore-errors (lens-remove-lenses-from-string str)) str)))


;;;; Modifying

(defun lens-modify (region new &optional external norefresh)
  "Update the lens currently at REGION with the new data NEW.

EXTERNAL specifies if the new value provided was a new text
value, rather than a new state value. In this case, don't call
the update function (the new text is already saved).

If NOREFRESH is t, only the header and footer should be reinserted. If
NOREFRESH is a (lambda (OLDSTATE NEWSTATE HE FB)), then use that function to
reinsert the lens content."
  (pcase-let* ((`((,spec ,oldtext ,oldstate) ,hb ,he ,fb ,fe) region)
               (display (get spec :display))
               (source (get spec :source))

               ;; Generate the new lens to replace the old lens with
               (`(,newtext ,newstate)
                (if external (list new (funcall (plist-get display :tostate) new))
                  (list (funcall (plist-get display :totext) new) new)))
               (newlens (list spec newtext newstate)))

    ;; If nothing was changed, then leave the current lens as is.
    ;; Replacing the lens when it hasn't changed can lead to issues
    ;; with the undo history
    (unless (and (string= oldtext newtext) (equal oldstate newstate))
      (lens-save-position-in-ui
       (lens-silent-edit
        ;; Refresh the style properties
        (add-text-properties (1+ he) fb (plist-get (get spec :style) :props))
        (lens--append-face (1+ he) fb (plist-get (get spec :style) :face))

        (if norefresh
            ;; Reinsert only the header and footer
            (let ((strs (lens--generate-headers newlens)))
              (goto-char fb) (delete-region fb fe) (insert (cdr strs))
              (lens--refresh-buffer fb (point))
              (goto-char hb) (delete-region hb he) (insert (car strs))
              (lens--refresh-buffer hb (point))
              (when (functionp norefresh)
                (funcall norefresh oldstate newstate he fb)))

          ;; Reinsert the entire lens
          (let ((insert (lens--generate-insert-text newlens)))
            (delete-region hb fe)
            (insert insert)
            (lens--refresh-buffer hb (point))))))

      ;; Update the data source if this was a direct modification to the lens
      (unless external (funcall (plist-get source :update) newtext)))))


;;;; Removing

(defun lens-remove (region)
  "Remove the lens at REGION."
  (interactive (list (lens-at-point)))
  (lens-unfold (car region))

  (pcase-let* ((`((,spec ,text ,state) ,hb ,he ,fb ,fe) region)
               (replace (funcall (plist-get (get spec :source) :replace) text)))
    (lens-silent-edit
     (delete-region hb fe)
     (remove-overlays (point-min) (point-max) 'lens-box-id spec)
     (insert replace))

    (lens--refresh-buffer hb (+ hb (length replace)))))

(defun lens-run-on-all (func &optional noerror)
  "Run FUNC on all lenses in the buffer.

FUNC will be called with one argument, the region of the lens. If
NOERROR is non-nil, demote errors thrown by FUNC to messages."
  (let (region)
    (save-excursion
      (goto-char (point-min))
      (while (setq region (lens-search-forward))
        (condition-case err (funcall func region)
          (error (unless noerror (message "Error in `lens-run-on-all': %s" (cadr err)))))))))

(defun lens-remove-all ()
  "Remove all lenses in the current buffer."
  (interactive)
  (lens-run-on-all #'lens-remove t))

(defun lens-remove-lenses-from-string (string)
  "Return STRING with all lenses removed."

  ;; If the string contains the beginning or end of a lens, then remove lenses
  (if (or (text-property-not-all 0 (length string) 'lens-begin nil string)
          (text-property-not-all 0 (length string) 'lens-end nil string))
      (with-temp-buffer
        (lens-silent-edit
         (insert string)
         (lens-remove-all)
         (buffer-string)))

    ;; Either it contains no lenses, or just contains some contents of a lens
    string))


;;;; Saving

(defvar-local lens--presave nil
  "Stores the buffer contents before removing the lenses.
Should be nil or a plist of the form:
\(:text TEXT :pos POINT :overlays OVERLAYS)")

(defun lens--before-save ()
  "Remove lenses from the buffer before saving."
  (when (text-property-not-all (point-min) (point-max) 'lens-begin nil)
    (setq lens--presave
          (list :text (buffer-string) :pos (point)
                :overlays (--map (list (overlay-start it) (overlay-end it) (overlay-properties it))
                                 (overlays-in (point-min) (point-max)))))
    (add-hook 'after-save-hook #'lens--after-save-once nil 'local)

    (when lens-save-lenses-on-save
      (lens-run-on-all
       (lambda (region)
         (let* ((lens (car region))
                (text (cadr lens))
                (src (get (car lens) :source)))
           (funcall (plist-get src :update) text)
           (funcall (or (plist-get src :save) #'ignore) text)))
       :noerror))
    (lens-remove-all)))

(defun lens--after-save-once ()
  "After saving, restore the lenses to the buffer."
  (when lens--presave
    (lens-silent-edit
     (remove-overlays (point-min) (point-max))
     (delete-region (point-min) (point-max))
     (insert (plist-get lens--presave :text))
     (dolist (old (plist-get lens--presave :overlays))
       (let ((o (make-overlay (car old) (cadr old)))
             (ps (caddr old)))
         (while ps (overlay-put o (pop ps) (pop ps))))))
    (goto-char (plist-get lens--presave :pos))

    (remove-hook 'after-save-hook #'lens--after-save-once 'local)
    (setq lens--presave nil)

    (set-buffer-modified-p nil)))


;;; Some Sources and Displays
;;;; Region Source

(defun lens-replace-source (str &optional before after)
  "A lens source replacing text STR in the current buffer.

BEFORE and AFTER are strings which were before/after STR in the
buffer, but should not be included in the lens text."
  (setq before (or before "") after (or after ""))
  (list :init (lambda () str)
        :update (lambda (_text))
        :replace (lambda (text) (concat before text after))))


;;;; Buffer Source

(defun lens--add-buffer-referencer (source-buf lens-buf)
  "Mark LENS-BUF as having a lens which references SOURCE-BUF."
  (with-current-buffer source-buf
    (add-to-list 'lens--buffer-referencers lens-buf t)
    (add-hook 'after-change-functions #'lens--refresh-buffer-referencers nil 'local)))

(defun lens--refresh-buffer-referencers (&rest _after-change-args)
  "Refresh all lenses which reference the current buffer.

This function is designed to be set up as an `after-change-hook',
hance _AFTER-CHANGE-ARGS, although the args are ignored."
  ;; Remove killed buffers
  (setq lens--buffer-referencers (-filter #'buffer-live-p lens--buffer-referencers))

  ;; Save information from the source buffer, since we will be moving between buffers
  (let* ((str (string-trim (buffer-substring-no-properties (point-min) (point-max)) "\n*" "\n*"))
         (src-buf (current-buffer)) (src-file buffer-file-name)
         ;; Preicate to check if a lens referenced the source buffer/file
         (lens-pred (lambda (_ lens)
                      (or (eq src-buf (plist-get (get (car lens) :source) :source-buffer))
                          (and src-file (string= src-file (plist-get (get (car lens) :source) :source-file))))))
         position)

    (dolist (lens-buf lens--buffer-referencers)
      (with-current-buffer lens-buf
        (save-excursion
          ;; Search for all matching lenses in the buffer
          (goto-char (point-min))
          (while (setq position (lens-search-forward nil lens-pred))
            (unless (string= (string-trim (cadr (car position)) "\n*" "\n*") str)
              (lens-modify position str :external))))))))

(defun lens--update-source-buffer (buf text)
  "Update a source buffer BUF with modified lens content TEXT."
  (with-current-buffer buf
    (unless (string= text (buffer-string))
      ;; Update the buffer
      (lens-silent-edit
       (remove-overlays (point-min) (point-max))
       (delete-region (point-min) (point-max))
       (insert text))
      ;; Update other lenses
      (lens--refresh-buffer-referencers))))


(defun lens-buffer-source (buf &optional replaced)
  "Create a source referencing buffer BUF and replacing REPLACED."
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
        (lambda (_text)
          (with-current-buffer buf (when buffer-file-name (save-buffer))))
        :title (lambda () (buffer-name buf))))


;;;; File Source

(defvar lens--file-referencers nil "Alist of files to buffers with a file source.")
(defun lens--add-file-referencer (file lens-buf)
  "Mark LENS-BUF as referencing the file FILE."
  (setq file (expand-file-name file) lens-buf (get-buffer lens-buf))
  (let ((assoc (assoc file lens--file-referencers))
        (file-buf (get-file-buffer file)))

    (cond ((null assoc) (push (list file lens-buf) lens--file-referencers))
          ((not (memq lens-buf (cdr assoc))) (push file (cdr assoc))))

    (when file-buf (lens--add-buffer-referencer file-buf lens-buf))))

(add-hook 'find-file-hook 'lens-find-file-hook)
(defun lens-find-file-hook ()
  "After finding a file, determine if any lenses reference it."
  (let ((pair (assoc (expand-file-name buffer-file-name) lens--file-referencers)))
    (dolist (lens-buf (cdr pair))
      (lens--add-buffer-referencer (current-buffer) lens-buf))))

(defun lens-file-source (file &optional replaced)
  "Create a source for referencing FILE, and replacing REPLACED."
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


;;;; Fields

(bz/face lens-field-header fixed-pitch :fg gray3 :h 0.9)
(defun lens--field (text onchange &optional head foot)
  "Create a controlled, modifyable region of text.

TEXT is the initial content of the region. ONCHANGE is a function
to be called with the new content every time the region is
modified.

The region will be bounded before and after by a read-only header
and footer. HEAD and FOOT can indicate the text of the header and
footer.

Every time the field is modified, it will be marked as modified
in `lens--modified-fields'. Then, in the `post-command-hook', the
new region content will be edited to have the correct properties,
and then the ONCHANGE function will be called."
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

(defun lens--field-modification-hook (beg _end)
  "Mark the field starting at BEG as modified."

  (push (list :buffer (current-buffer) :pos (set-marker (make-marker) beg)
              ;; If it was modified by an undo action, mark it as such
              :undo (when (--find (eq (cadr it) #'primitive-undo) (backtrace-frames)) t))
        lens--modified-fields)
  (add-hook 'post-command-hook #'lens--field-modification-callback))

(defun lens--field-modification-callback ()
  "Perform all necessary actions to update the modified fields.

This function is expected to be called in the
`post-command-hook', after one or more fields were modified by
the previous command, as described in `lens--field'."

  (remove-hook 'post-command-hook #'lens--field-modification-callback)
  (let (saved-lenses)
    (dolist (plist (prog1 lens--modified-fields (setq lens--modified-fields nil)))
      (with-current-buffer (plist-get plist :buffer)
        (lens-silent-edit
         (goto-char (plist-get plist :pos))

         (if (plist-get plist :undo)
             ;; If it was an undo action, we only need to update the restored lens
             (pcase-let ((`(,lens ,_hb ,_he ,_fb ,_fe) (lens-at-point t)))
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


;;;; Raw display

(defun lens--raw-display-onchange (text)
  "Value of onchange for `lens--field' used by `lens-raw-display'.

TEXT is the new text of the field."
  (let ((region (lens-at-point)))
    (lens-modify region text nil :norefresh)))

(defun lens--raw-display-insert (state)
  "Generate the content for a raw display with state STATE."
  (let ((field (lens--field state #'lens--raw-display-onchange "\n" "\n")))
    (lens--make-sticky field :before :after)))

(defun lens-raw-display (&optional forward-filter backward-filter)
  "Display spec to display text data verbatim.

FORWARD-FILTER is called on the text data and returns the
displayed text. BACKWARD-FILTER is called on the displayed text
and returns the new value of the text data."
  (list :tostate
        (lambda (text)
          (funcall (or forward-filter #'identity) (string-trim text "\n+" "\n+")))
        :totext (or backward-filter #'identity)
        :insert #'lens--raw-display-insert))


;;; Ui
;;;; Diffing algorithm

(defun lens--ui-diff (old-list new-list)
  "Diff OLD-LIST and NEW-LIST of UI elements using Myers algorithm.

Each element is (ELEM . VALUE) where ELEM is what is being
compared (using #'equal), and VALUE is what is returned.

Returns a list of (:keep/:insert/:delete OLD NEW), where OLD is the old
VALUE and NEW is the new VALUE. For :insert and :delete, one of these
will be nil."
  (let* ((n (length old-list))
         (m (length new-list))
         (max-d (+ n m))
         (old-vec (vconcat old-list))
         (new-vec (vconcat new-list))
         (v-offset (1+ max-d))
         (v (make-vector (+ (* 2 max-d) 3) 0))
         (trace '())
         (final-d 0))

    (cond
     ((and (= n 0) (= m 0)) '())
     ((= n 0) (mapcar (lambda (e) (list :insert nil (cdr e))) new-list))
     ((= m 0) (mapcar (lambda (e) (cons :delete (cdr e) nil)) old-list))
     (t
      ;; Forward pass
      (catch 'found
        (dotimes (d (1+ max-d))
          (push (copy-sequence v) trace)
          (let ((k (- d)))
            (while (<= k d)
              (let* ((go-down (or (= k (- d))
                                  (and (/= k d)
                                       (< (aref v (+ v-offset k -1))
                                          (aref v (+ v-offset k 1))))))
                     (x (if go-down
                            (aref v (+ v-offset k 1))
                          (1+ (aref v (+ v-offset k -1)))))
                     (y (- x k)))
                (while (and (< x n) (< y m)
                            (equal (car (aref old-vec x))
                                   (car (aref new-vec y))))
                  (setq x (1+ x) y (1+ y)))
                (aset v (+ v-offset k) x)
                (when (and (>= x n) (>= y m))
                  (setq final-d d)
                  (throw 'found t)))
              (setq k (+ k 2))))))

      ;; Backtrack
      (let ((x n)
            (y m)
            (edits '()))
        ;; Walk back through each edit step
        (let ((d final-d))
          (while (> d 0)
            (let* ((v-prev (nth (- final-d d) trace))
                   (k (- x y))
                   (go-down (or (= k (- d))
                                (and (/= k d)
                                     (< (aref v-prev (+ v-offset k -1))
                                        (aref v-prev (+ v-offset k 1))))))
                   (prev-k (if go-down (1+ k) (1- k)))
                   (prev-x (aref v-prev (+ v-offset prev-k)))
                   (prev-y (- prev-x prev-k)))
              ;; Record diagonal matches (in reverse)
              (while (and (> x prev-x) (> y prev-y))
                (setq x (1- x) y (1- y))
                (push (list :keep (cdr (aref old-vec x)) (cdr (aref new-vec y))) edits))
              ;; Record the edit
              (if go-down
                  (progn (setq y (1- y))
                         (push (list :insert nil (cdr (aref new-vec y))) edits))
                (setq x (1- x))
                (push (list :delete (cdr (aref old-vec x)) nil) edits)))
            (setq d (1- d))))
        ;; Handle initial diagonal (d=0 matches)
        (while (and (> x 0) (> y 0))
          (setq x (1- x) y (1- y))
          (push (list :keep (cdr (aref old-vec x)) (cdr (aref new-vec y))) edits))
        edits)))))


;;;; Generating Uis

(defun lens--generate-ui-element (elem cb)
  "Generates a single ui element by applying the component to the args.

ELEM is (COMPONENT KEY ARGS...). CB is the update callback.

Returns (ELEM STRING KEY)"

  ;; Use a string as a shorthand for the 'string component
  (let* ((component-fn (or (get (car elem) 'lens-component)
                           (error "Not a component: %s" (car elem))))
         (key (cadr elem))
         (str (apply component-fn cb (cddr elem))))
    ;; An empty string WILL break diffing and detection of the current element
    (when (string-empty-p str) (setq str "-"))

    (unless (keywordp key) (error "Element key must be a keyword: %s" elem))
    (list elem str key)))

(defun lens--ui-renderer (old-state new-state he fb)
  (let* ((old-ui (plist-get old-state :ui))
         (new-ui (plist-get new-state :ui))
         (diff (lens--ui-diff old-ui new-ui)))
    (save-excursion
      (goto-char (1+ he))
      (pcase-dolist (`(,action ,old ,new) diff)
        (if (eq action :insert)
            (insert (car new) (propertize "\n" 'read-only t
                                          'lens-element-key (cadr new)
                                          'rear-nonsticky t))
          (let ((start (point))
                ;; The match is the newline at the end of the old element
                (match (text-property-search-forward 'lens-element-key))
                m)
            (cl-assert match)
            (cl-assert (eq (prop-match-value match) (cadr old)))
            (when (eq action :delete) (delete-region start (point)))))))))

(defun lens--ui-callback-for-lens (ui-id toui mutator mutator-args)
  (let* ((region (lens-at-point))
         (state (caddr (car region)))
         (new-state (apply #'list state))
         generated)

    (unless (eq (plist-get (caddr (car region)) :ui-id) ui-id) (error "Incorrect lens at point"))

    ;; Copy the internal state
    (plist-put new-state :state (copy-tree (plist-get new-state :state)))
    ;; Apply the mutator
    (apply mutator (plist-get new-state :state) mutator-args)
    ;; Generate the new ui
    (let* ((cb (lambda (mut &rest mut-args) (lens--ui-callback-for-lens ui-id toui mut mut-args)))
           (ui (funcall toui (plist-get new-state :state)))
           (generated (--map (lens--generate-ui-element it cb) ui)))
      (plist-put new-state :ui generated)

      ;; Apply modifications
      (lens-modify region new-state nil #'lens--ui-renderer))))

(defun lens-ui-display (ui)
  "Display spec for custom ui definitions.

UI is a plist with properties :tostate, :totext, and :toui."
  (-let (((&plist :tostate tostate :totext totext :toui toui) ui)
         (ui-id (abs (random))))

    (list :tostate
          (lambda (text)
            (let* ((state (funcall tostate text))
                   (ui (funcall toui state))
                   (cb (lambda (mut &rest mut-args) (lens--ui-callback-for-lens ui-id toui mut mut-args)))
                   (generated (--map (lens--generate-ui-element it cb) ui)))
              (list :state state :ui-id ui-id :ui generated)))
          :totext (lambda (state) (funcall totext (plist-get state :state)))
          :insert
          (lambda (state)
            (string-join
             (cons (propertize "\n" 'lens-element-key 'START 'rear-nonsticky t)
                   (--map (concat (cadr it)
                                  (propertize "\n" 'lens-element-key (caddr it)))
                          (plist-get state :ui))))))))


;;;; Components

(defmacro lens-defcomponent (name arglist &rest body)
  "Define a UI component"
  (declare (indent 2))

  ;; Parse the beginning of the body as properties
  (let ((props nil))
    (while (keywordp (car body))
      (push (cons (pop body) (pop body)) props))

    (apply #'list #'prog1
           `(put ',name 'lens-component (lambda ,arglist . ,body))
           (--map `(put ',name
                        ',(intern (concat "lens-component-"
                                          (substring (symbol-name (car it)) 1)))
                        ,(cdr it))
                  props))))

(lens-defcomponent box (cb text onchange &rest plist)
  (lens--field text
               (lambda (new)
                 (funcall cb (or onchange #'ignore) new))
               "[begin box]\n" "\n[end box]"))

(lens-defcomponent string (cb str)
  :can-be-column t
  (propertize (if (stringp str) str (string-join str "\n"))
              'read-only t))


;;;; Button

(bz/face lens-button custom-button)
(bz/keys lens-button-keymap
  :sparse t
  "<return>" lens-click)

(defun lens-click ()
  "If there is a ui button at point, call its onclick function."
  (interactive)
  (let ((click (get-text-property (point) 'lens-click)))
    (if click (funcall click)
      (error "Nothing to click"))))

(lens-defcomponent button (cb label onclick &rest plist)
  :can-be-column t
  (propertize (format " %s " label) 'lens-click
              (lambda ()
                (funcall cb (or onclick #'ignore)))
              'local-map (list 'keymap lens-button-keymap (current-local-map))
              'font-lock-face (or (plist-get plist :face) 'lens-button)
              'read-only t))


;;;; Column

(defun lens-string-width (str)
  "Calculate the pixel width of a string STR."
  (let ((inhibit-read-only t))
    (if (boundp #'string-pixel-width)
        (/ (string-pixel-width str) (string-pixel-width "a"))
      (require 'shr)
      (/ (shr-string-pixel-width str) (shr-string-pixel-width "a")))))

(defun lens--join-columns (cols &optional sep)
  "Vertically align a list of strings as columns.

COLS is the list of strings to align, and SEP is an additional
string to insert between the columns."
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

(lens-defcomponent row (cb &rest cols)
  ;; Ensure that all are valid columns
  (dolist (elem cols)
    (unless (or (stringp elem)
                (get (car elem) 'lens-component-can-be-column))
      (error "Invalid column component: %s" (car elem))))
  (lens--join-columns (--map (lens--ui-element-to-string it cb) cols)
                      (propertize " " 'read-only t)))


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
    (lens-silent-edit
     (delete-region beg end)
     (lens-create (funcall src-func str) display))))

(defun lens--wrapped-create (hb he fb fe src-func display)
  (let ((head (buffer-substring-no-properties hb he))
        (body (buffer-substring-no-properties he fb))
        (foot (buffer-substring-no-properties fb fe)))
    (lens-silent-edit
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
         (link (format "[[%s]]\n" rel)))
    (mkdir (f-parent path) t)
    (f-touch path)
    (find-file-noselect path t)
    (lens-create (lens-file-source path link) (lens-raw-display))))


;;; Folding

(defun lens-fold (region)
  "Fold the lens at REGION, or at point."
  (interactive (list (lens-at-point)))
  (remove-overlays nil nil 'lens-fold (car region))

  (pcase-let* ((`(,lens ,_hb ,he ,fb ,fe) region)
               (face (plist-get (get (car lens) :style) :head-face))
               (o1 (make-overlay he (1- fb)))
               (o2 (make-overlay he fe))
               (o3 (when face (make-overlay (1- fb) fb))))
    (overlay-put o1 'lens-fold lens)
    (overlay-put o1 'invisible t)
    (overlay-put o1 'before-string (propertize "..." 'face face))

    (overlay-put o2 'lens-fold lens)
    (overlay-put o2 'intangible t)

    (when o3
      (overlay-put o3 'lens-fold lens)
      (overlay-put o3 'face face))))

(defun lens-unfold (lens)
  "Unfold LENS, or the lens at point."
  (interactive (list (car (lens-at-point))))
  (remove-overlays (point-min) (point-max) 'lens-fold lens))

(defun lens-fold-all ()
  "Fold all lenses in the current buffer."
  (interactive)
  (lens-run-on-all #'lens-fold))

(defun lens-unfold-all ()
  "Unfold all lenses in the current buffer."
  (interactive)
  (dolist (o (overlays-in (point-min) (point-max)))
    (when (overlay-get o 'lens-fold)
      (delete-overlay o))))

(defun lens-fold-toggle (region)
  "Toggle whether the lens at REGION, or at point, is folded."
  (interactive (list (lens-at-point)))
  (if (--find (eq (car region) (overlay-get it 'lens-fold))
              (overlays-in (point-min) (point-max)))
      (lens-unfold (car region))
    (lens-fold region)))

(defun lens-fold-toggle-all ()
  "Toggle whether all overlays in the current buffer are folded.

If all overlays are folded, unfold all overlays. Otherwise, fold
all overlays."
  (interactive)
  (let* ((os (overlays-in (point-min) (point-max)))
         (lenses (-uniq (--map (overlay-get it 'lens-fold) os)))
         (all-folded t))
    (lens-run-on-all
     (lambda (region)
       (unless (memq (car region) lenses)
         (setq all-folded nil))))
    (if all-folded
        (lens-unfold-all)
      (lens-fold-all))))


;;; Auto Mode
;;;; Org Matchers

(defvar lens-auto--org-block-re "^#\\+begin_lens \\([-_a-zA-Z0-9]+\\).*\n\\([^z-a]*?\\)\n#\\+end_lens\n")
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


(defvar lens-auto-matchers:org-mode
  `((,lens-auto--org-link-re lens-auto--org-file-link)
    (,lens-auto--org-block-re lens-auto--org-block)))


;;;; Mode

(defvar-local lens-auto-matchers nil
  "Alist of a regexp to a function.")

(defvar lens-auto-mode-alist
  `((org-mode . ,lens-auto-matchers:org-mode))
  "Alist of major modes to values of `lens-auto-matchers'.")

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


;;; Boxes

(bz/face lens-box-overline :o "lightskyblue4")
(bz/face lens-box-underline :u (:color "lightskyblue4"))

(bz/face lens-box-body :bg bg :x t)
(bz/face lens-box-head (lens-box-body lens-box-overline fixed-pitch) :h 0.9 :fg blue :w bold)
(bz/face lens-box-foot (lens-box-body lens-box-underline fixed-pitch) :h 1)

(bz/face lens-box-vert :bg "lightskyblue4")
(bz/face lens-box-padding lens-box-body :bg bg)

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

(bz/keys lens-header-map
  :sparse t
  :parent org-mode-map
  [remap bz/fold-toggle] lens-fold-toggle
  )

(setq lens-box-style
      (list :props (list 'bz/line-prefix lens-line-prefix 'bz/wrap-prefix lens-line-prefix)
            :face 'lens-box-body
            :head-props (list 'bz/line-prefix lens-head-prefix 'bz/wrap-prefix lens-head-prefix
                              'local-map lens-header-map 'keymap lens-header-map)
            :head-face 'lens-box-head
            :foot-props (list 'bz/line-prefix lens-foot-prefix 'bz/wrap-prefix lens-foot-prefix)
            :foot-face 'lens-box-foot)
      lens-default-style lens-box-style)

;; (setq lens-default-style nil)


