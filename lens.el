;;; lens.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'f)
(require 'org)
(require 'text-property-search)


(defgroup lens nil
  "Customization group for lens."
  :prefix "lens-"
  :group 'tools)


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


;;; Utilities
;;;; Let macro

(defvar lens--let-keywords '((:let . let)
                             (:flet . cl-flet)
                             (:when-let . when-let)
                             (:pcase-let . pcase-let)
                             (:-let . -let))
  "An alist mapping let keywords to their corresponding functions.")


(defmacro lens-let-body (&rest body)
  "A macro designed to make ui component bodies simpler.

If any of body is (LET-KEYWORD VAR VAL), then this will be compiled to a
let expression containing the rest of the body. LET-KEYWORD can be any
of `:let', `:flet', `:when-let', `:pcase-let', or `:-let'.

So,
  (:let a 4)
  (:when-let b (1+ a))
  (+ a b)
will be converted to
  (let ((a 4))
    (when-let ((b (1+ a)))
      (+ a b)))

Furthermore, the car of body can be a vector containing custom symbols
to convert to special forms. Each entry should be a cons cell mapping
the desired symbol to a function. When the symbol is used as a function
call in the top level of BODY, this function will be called at compile
time to generate the new expression.

The first argument is a list of Lisp expressions, which is the result of
applying `lens-let-body' to the remaining body. The remaining arguments
are the list of arguments passed to the call. The return value is a list
of Lisp expressions to replace the original body.

For example, the following is expression used for use state:
  [(:use-state
    (lambda (body state-var set-var)
      `((let* ((=use-state= (lens--use-state))
               (,state-var (car =use-state=)))
          (cl-flet ((,set-var (cdr =use-state=)))
            ,@body)))))]

This allows for the following syntax:
  (:use-state count set-count)
  (set-count (1+ count))"

  (let ((custom (when (vectorp (car body)) (append (pop body) nil)))
        exprs func)
    ;; Go through the expressions in reverse
    (dolist (line (reverse body))
      (cond ((not (listp line)) (push line exprs))
            ((setq func (alist-get (car line) lens--let-keywords))
             (setq exprs `((,func (,(cdr line)) ,@exprs))))
            ((setq func (alist-get (car line) custom))
             (setq exprs (apply func exprs (cdr line))))
            (t (push line exprs))))
    (if (> (length exprs) 1) (cons #'progn exprs) (car exprs))))


;;;; Utils

(defmacro lens-save-position (&rest body)
  "Save the line and column of the cursor when executing BODY."
  `(let* ((line (line-number-at-pos)) (col (current-column))
          (ui-elem (lens--ui-element-at-point)))
     ,@body
     (goto-char (point-min))
     (forward-line (1- line))
     (forward-char (min col (- (pos-eol) (point))))))

(defmacro lens-save-position-in-ui (&rest body)
  "Save the current position relative to the containing ui element."
  `(let* ((line (line-number-at-pos)) (col (current-column))
          me ui-key ui-line region)
     (save-excursion
       (and
        ;; Find the prop match at the end of the component
        (setq me (text-property-search-forward 'lens-element-key))
        ;; If the next prop match is a start, then we weren't inside of an element
        (not (eq (prop-match-value me) 'START))
        ;; Go to the prop match at the beginning of the component
        (progn (forward-char -1)
               (text-property-search-backward 'lens-element-key))
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
                (forward-char (min col (- (pos-eol) (point)))))
       (goto-char (point-min))
       (forward-line (1- line))
       (forward-char (min col (- (pos-eol) (point)))))))

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
         (cursor-sensor-inhibit t))
     (atomic-change-group ,@body)))

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

  (let* ((cursor-sensor-inhibit t)
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
  (let* ((cursor-sensor-inhibit t)
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


;;;; Batch post command

(defvar lens--batch-post-commands nil
  "An alist mapping functions to lists of arguments.

Each call to `lens-batch-post-command' with a particular command and arg
is associated with a single entry in the cdr of command. So, if 3 calls
are made with the same command, the cdr will be a list of 3 elements,
the 3 arguments (in REVERSE order).

Each command will be called with the entire list of commands, (in the
correct order, the reverse of what was stored.)")

(defun lens-batch-post-command (command arg)
  (push arg (alist-get command lens--batch-post-commands))
  (add-hook 'post-command-hook #'lens--batch-post-command-callback))

(defun lens--batch-post-command-callback ()
  (remove-hook 'post-command-hook #'lens--batch-post-command-callback)
  (let ((cmds (nreverse lens--batch-post-commands)))
    (setq lens--batch-post-commands nil)

    (dolist (command cmds)
      (with-demoted-errors "Error in batch command: %S"
        (funcall (car command) (nreverse (cdr command)))))))


;;;; Fields

(defvar lens--modified-fields nil
  "List of (:buffer BUF :pos MARKER :undo? BOOL).")

(bz/face lens-field-header fixed-pitch :fg gray3 :h 0.9)
(defun lens--field (text onchange &optional head foot props)
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
and then the ONCHANGE function will be called.

PROPS is a plist of text properties that will be applied to the text of
the field."
  (setq head (or head (propertize "---\n" 'face 'lens-field-header 'font-lock-face 'lens-field-header))
        foot (or foot (propertize "\n---" 'face 'lens-field-header 'font-lock-face 'lens-field-header)))
  (let* ((field (lens--make-symbol "field"))
         (hooks '(lens--field-modification-hook)))
    (fset field onchange)
    (put field 'lens-field-props props)
    (put-text-property (1- (length head)) (length head) 'insert-behind-hooks hooks head)

    (concat (lens--make-sticky (propertize head 'field-begin field 'read-only t))
            (apply #'propertize text
                   'modification-hooks hooks 'insert-behind-hooks hooks
                   props)
            (lens--make-sticky (propertize foot 'field-end field 'read-only t)))))

(defun lens--field-modification-hook (beg _end)
  "Mark the field starting at BEG as modified."

  (push (list :buffer (current-buffer)
              :pos (set-marker (make-marker) beg)
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
         (if (plist-get plist :undo)
             ;; If it was an undo action, we only need to update the restored lens
             (pcase-let ((`(,lens ,_hb ,_he ,_fb ,_fe) (lens-at-point t)))
               (when (and lens (not (memq lens saved-lenses)))
                 (push lens saved-lenses)
                 (funcall (plist-get (get (car lens) :source) :update) (cadr lens))))

           ;; Perform the procedure in case there was a non-undo modification
           (pcase-let ((`(,field ,hb ,he ,fb ,fe)
                        (save-excursion
                          (goto-char (plist-get plist :pos))
                          (lens--region-at-point 'field-begin 'field-end))))
             (when field
               ;; Make sure the entire text has the correct properties
               (dolist (prop '(modification-hooks insert-behind-hooks))
                 (put-text-property he fb prop '(lens--field-modification-hook)))
               (let ((props (get field 'lens-field-props)))
                 (while props (put-text-property he fb (pop props) (pop props))))

               ;; Determine whether the point was inside of the field content
               (let (cursor)
                 (when (and (>= (point) he) (<= (point) fb))
                   (setq cursor (- (point) he)))
                 (funcall field (buffer-substring he fb) cursor))

               (lens--refresh-buffer hb fe)))))))))


;;;; Box helpers

(defvar lens-textbox-chars
  '((unicode (top-left     . "\u250C")
             (top-right    . "\u2510")
             (bottom-left  . "\u2514")
             (bottom-right . "\u2518")
             (horizontal   . "\u2500")
             (vertical     . "\u2502"))
    (ascii (top-left     . "+")
           (top-right    . "+")
           (bottom-left  . "+")
           (bottom-right . "+")
           (horizontal   . "-")
           (vertical     . "|"))))

(defun lens-text-to-box (text &rest props)
  (pcase-let ((`(,body ,head ,foot) (apply #'lens--textbox-wrap text props)))
    (concat head
            (cl-loop for (content prefix suffix) in body
                     concat (concat prefix content suffix))
            foot)))

(defun lens--textbox-wrap (text &rest props)
  "Returns (BODY HEAD FOOT CURSOR-LINE CURSOR-COL).

HEAD and FOOT are strings for the header and footer.

BODY is a list of lines which look like (CONTENT PREFIX SUFFIX)

PROPS has the following meaningful properties:
  :cursor - The 0-index of the cursor in the original text
  :charset - unicode or ascii
  :width - The outer width (default 50)
  :shrink - Shrink if the text is smaller than the width
  :pad - The horizontal padding on each side (default 1)
  :title - Text displayed in the top border
  :box-props - Plist of text properties applied only to the border"
  (lens-let-body
   (:let hpad (or (plist-get props :pad) 1))
   (:let width (or (plist-get props :width) 50))
   (when (plist-get props :shrink)
     (let ((longest-line (cl-loop for line in (split-string text "\n")
                                  maximize (1+ (string-width line)))))
       (setq width (min width (+ 2 (* hpad 2) longest-line)))))

   (:let inner-width (- width 2 (* 2 hpad)))
   (unless (> inner-width 0) (error "Text box is too thin"))

   (:let cursor (plist-get props :cursor))
   (when (and cursor (> cursor (length text))) (error "Cursor position exceeds text length"))

   (:let chars (alist-get (or (plist-get props :charset) 'ascii)
                          lens-textbox-chars))

   (:let title (plist-get props :title))
   ;; 3 chars are needed before the title, 3 after the title
   (:let show-title-len (min (- width 6) (length title)))
   (:let show-title (and title (> show-title-len 0)
                         (substring title 0 show-title-len)))

   (:let hor (alist-get 'horizontal chars))
   (:let head-str
         (if show-title
             (concat (alist-get 'top-left chars)
                     hor " " show-title " " hor
                     (make-string (- width 6 show-title-len) (aref hor 0))
                     (alist-get 'top-right chars)
                     "\n")
           (concat (alist-get 'top-left chars)
                   (make-string (- width 2) (aref hor 0))
                   (alist-get 'top-right chars)
                   "\n")))
   (:let foot-str (concat (alist-get 'bottom-left chars)
                          (make-string (- width 2) (aref hor 0))
                          (alist-get 'bottom-right chars)))

   (:let body-lines (list ""))
   ;; The current index we are looking at in TEXT
   (:let idx 0)
   ;; The start of the current word
   (:let cur-word-start nil)

   (:flet add-word (word &optional is-word)
          (cond ((> (string-width word) inner-width)
                 (when (string-empty-p (car body-lines)) (pop body-lines))
                 (while (> (string-width word) inner-width)
                   (let ((idx (cl-loop for i from 1 below (length word)
                                       when (> (string-width word 0 i) inner-width)
                                       do (cl-return (1- i))
                                       finally (cl-return (length word)))))
                     (push (substring word 0 idx) body-lines)
                     (setq word (substring word idx))))
                 (push word body-lines))
                ((or (> (+ (string-width word) (string-width (car body-lines)) (if is-word 1 0))
                        inner-width))
                 (when (string-empty-p (car body-lines)) (pop body-lines))
                 (push word body-lines))
                (t (setcar body-lines (concat (car body-lines) word)))))

   ;; Iterate through the text
   (while (< idx (length text))
     (if (not (string-match-p "[\n\s]" (substring text idx (1+ idx))))
         (setq cur-word-start (or cur-word-start idx))

       ;; Add the previous word to the body
       (when cur-word-start
         (add-word (substring text cur-word-start idx) t)
         (setq cur-word-start nil))
       ;; Add the space to the body
       (if (not (eq (aref text idx) ?\n))
           (add-word (substring text idx (1+ idx)))
         (add-word (apply #'propertize "¶" 'lens-textbox-newline t 'face 'shadow
                          (text-properties-at idx text)))
         (push "" body-lines)))
     (setq idx (1+ idx)))
   (when cur-word-start (add-word (substring text cur-word-start idx) t))

   (setq body-lines (nreverse body-lines))

   ;; Determine the cursor position
   (:let cursor-line nil)
   (:let cursor-col nil)
   (when (eq cursor (length text))
     (setq cursor-line (1- (length body-lines))
           cursor-col (length (car (last body-lines)))))
   (when (and cursor (< cursor (length text)))
     (let ((line-end-pos 0) line-start)
       (setq cursor-line 0)
       (while (progn (setq line-end-pos (+ line-end-pos (length (nth cursor-line body-lines))))
                     (<= line-end-pos cursor))
         (setq cursor-line (1+ cursor-line)))
       (setq line-start (- line-end-pos (length (nth cursor-line body-lines))))
       (setq cursor-col (- cursor line-start))))

   ;; Wrap the lines
   (:let vert (alist-get 'vertical chars))
   (:let box-props (plist-get props :box-props))

   (list (--map (list it
                      (apply #'propertize (concat vert (make-string hpad ?\s))
                             'rear-nonsticky t
                             'front-sticky t
                             box-props)
                      (apply #'propertize
                             (concat (make-string (+ (- inner-width (string-width it)) hpad) ?\s)
                                     vert "\n")
                             box-props))
                body-lines)
         (apply #'propertize head-str box-props)
         (apply #'propertize foot-str box-props)
         cursor-line
         cursor-col)))


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
      (lens-silent-edit
       ;; Refresh the style properties
       (add-text-properties (1+ he) fb (plist-get (get spec :style) :props))
       (lens--append-face (1+ he) fb (plist-get (get spec :style) :face))

       (if norefresh
           ;; Reinsert only the header and footer
           (let ((strs (lens--generate-headers newlens)))
             (save-excursion (goto-char fb) (delete-region fb fe) (insert (cdr strs))
                             (lens--refresh-buffer fb (point)))
             (save-excursion (goto-char hb) (delete-region hb he) (insert (car strs))
                             (lens--refresh-buffer hb (point)))
             (when (functionp norefresh)
               (funcall norefresh oldstate newstate he fb)))

         ;; Reinsert the entire lens
         (let ((insert (lens--generate-insert-text newlens)))
           (lens-save-position
            (delete-region hb fe)
            (insert insert)
            ;; Go back to the start of the lens so that
            ;; lens-save-position-in-ui recognizes the current lens
            (goto-char hb)
            (lens--refresh-buffer hb (point))))))

      ;; Update the data source if this was a direct modification to the lens
      (unless external (funcall (plist-get source :update) newtext)))))


;;;; Removing

(defun lens-remove (region)
  "Remove the lens at REGION."
  (interactive (list (lens-at-point)))
  (lens-unfold (car region))

  (pcase-let* ((`((,spec ,text ,_state) ,hb ,_he ,_fb ,fe) region)
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

Comparison is done using the car of each element, so elements can have
different cdr but still be considered the same.

Returns a list of (:keep/:insert/:delete CAR OLD-CDR NEW-CDR)."
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
     ((= n 0) (mapcar (lambda (e) (list :insert (car e) nil e)) new-list))
     ((= m 0) (mapcar (lambda (e) (list :delete (car e) e nil)) old-list))
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
                (push (let ((old (aref old-vec x))
                            (new (aref new-vec y)))
                        (list :keep (car old) (cdr old) (cdr new)))
                      edits))
              ;; Record the edit
              (if go-down
                  (progn (setq y (1- y))
                         (let ((val (aref new-vec y)))
                           (push (list :insert (car val) nil (cdr val)) edits)))
                (setq x (1- x))
                (push (let ((val (aref old-vec x)))
                        (list :delete (car val) (cdr val) nil))
                      edits)))
            (setq d (1- d))))
        ;; Handle initial diagonal (d=0 matches)
        (while (and (> x 0) (> y 0))
          (setq x (1- x) y (1- y))
          (push (let ((old (aref old-vec x))
                      (new (aref new-vec y)))
                  (list :keep (car old) (cdr old) (cdr new)))
                edits))
        edits)))))


;;;; Rerenderer

(defun lens--ui-string-rows (state key-path)
  "Returns a list of rows.

Each row has the form ((PATH ELEMENT STATE-HOOKS) . TEXT).

This format is designed to be suitable for diffing, as used in
`lens--ui-rerender'."
  (let* ((content (plist-get state :content))
         (element (plist-get state :element)))
    (if (stringp content)
        (list (cons (list key-path element
                          (--filter (eq (car it) :use-state) (plist-get state :hooks)))
                    content))
      (--mapcat (lens--ui-string-rows (cdr it) (append key-path (list (car it))))
                content))))

(defun lens--ui-renderer (old-state new-state he fb)
  (let* ((old-rows (lens--ui-string-rows old-state nil))
         (new-rows (lens--ui-string-rows new-state nil))
         (diff (lens--ui-diff old-rows new-rows))
         ;; Keep a hash map from key paths to (start . end) positions
         (key-path-to-posns (make-hash-table :test #'equal :size (* 2 (length new-rows))))
         (last-key-path nil))

    ;; Add the root position
    (puthash nil (cons (1+ he) fb) key-path-to-posns)

    (lens-save-position-in-ui
     (goto-char (1+ he))
     ;; Apply each diff element
     (pcase-dolist (`(,action (,key-path) ,_old_str ,new-str) (append diff (list nil)))
       (let ((start (point)) match)
         ;; The last pass through is just to record the end positions of the last element
         (when action
           (if (eq action :insert)
               (insert new-str (propertize "\n" 'read-only t 'lens-element-key key-path))
             ;; The match is the newline at the end of the old element
             (setq match (text-property-search-forward 'lens-element-key))
             (cl-assert match)
             (cl-assert (equal (prop-match-value match) key-path))
             (when (eq action :delete) (delete-region start (point)))))

         ;; Record the positions of the element
         (unless (eq action :delete)
           ;; We need to record the start position for all parent key
           ;; paths that are new since the last-key-path
           (dolist (i (number-sequence 1 (length key-path)))
             (let ((sub-path (-slice key-path 0 i)))
               (unless (equal sub-path (-slice last-key-path 0 i))
                 (puthash sub-path (cons start nil) key-path-to-posns))))
           ;; We need to record the end position for all key paths
           ;; that are parents of last-key-path, but not key-path
           (dolist (i (number-sequence 1 (length last-key-path)))
             (let ((sub-path (-slice last-key-path 0 i)))
               (unless (equal sub-path (-slice key-path 0 i))
                 (setcdr (gethash sub-path key-path-to-posns) start))))

           ;; Update last-key-path
           (setq last-key-path key-path)))))

    ;; Run any use effect callbacks
    (lens--ui-run-effects new-state key-path-to-posns)))


;;;; Generating uis

(defvar lens--current-ctx nil)

(defun lens--generate-ui (elem path old-state root-ctx-or-ui-id)
  "Return the new state for the component.

ELEM is either (ELEM KEY ARGS...) or a function.

PATH is a list of keywords, the keys of each of the parent elements. For
the root element, this will be nil.

OLD-STATE is the previous ui state, or nil if this is the first time
generating the ui.

When this is the root context, ROOT-CTX-OR-UI-ID is the ui id, otherwise
it is the pointer to the root context. The root context is the only
context that has the :active, :ui-id, and :ui-func properties, all other
contexts must get this information from the root context.

Create a ui context with the following properties:
  :is-first-call - Indicates whether calling a UI component for the
    first time, or a subsequent time.
  :state - The internal state for the component being called. See
    `lens-ui-display'."
  (let* ((state (if old-state (copy-tree old-state) (list :hooks nil :content nil)))
         (ctx (list :state state
                    :path path
                    :hook-idx 0
                    :is-first-call (null old-state)
                    (if (numberp root-ctx-or-ui-id) :ui-id :root-ctx) root-ctx-or-ui-id))
         ;; Perform the actual function
         (output (let ((lens--current-ctx ctx))
                   (if (functionp elem)
                       (funcall elem ctx)
                     (apply (get (car elem) 'lens-component) ctx (cddr elem)))))
         (content
          (if (stringp output) output
            (--map (cons (if (keywordp (cadr it)) (cadr it)
                           (error "First argument to component must be a keyword"))
                         (lens--generate-ui
                          it (append path (list (cadr it)))
                          (when old-state (alist-get (cadr it) (plist-get old-state :content)))
                          (if (numberp root-ctx-or-ui-id) ctx root-ctx-or-ui-id)))
                   output))))

    ;; Add root-specific properties
    (when (numberp root-ctx-or-ui-id)
      (plist-put ctx :active t)
      (plist-put ctx :ui-func elem))

    (plist-put state :content content)
    (unless (functionp elem) (plist-put state :element elem))

    state))


;;;; Callback

(defvar lens--callback-state nil
  "A copy of the root-level state of the ui, which should be mutated.")

(defvar lens--ui-callbacks nil
  "An alist mapping a ui-id to a list of callbacks.

Each callback takes a single argument, the state that they will mutate.")

(defun lens--follow-key-path (state path)
  (if (null path) state
    (lens--follow-key-path
     (or (alist-get (car path) (plist-get state :content))
         (error "Invalid component key path"))
     (cdr path))))

(defun lens--ui-callback (ctx run)
  (lens-let-body
   (:let root-ctx (or (plist-get ctx :root-ctx) ctx))
   (unless (plist-get root-ctx :active) (error "Context is no longer active"))
   ;; (plist-put root-ctx :active nil)

   (:let region (lens-at-point))
   (:let ui-id (plist-get root-ctx :ui-id))
   (unless (eq ui-id (plist-get (caddr (car region)) :ui-id))
     (error "Incorrect ui at point"))

   (:let old-state (caddr (car region)))
   (:let lens--callback-state (copy-tree old-state))

   ;; Apply the mutations
   (funcall run)

   ;; Generate the ui for the new state
   (:let new-state (lens--generate-ui (plist-get root-ctx :ui-func) nil
                                      lens--callback-state ui-id))

   ;; Apply the new state
   (lens-modify region new-state nil #'lens--ui-renderer)))

(defmacro lens-ui-callback (ctx &rest body)
  "Returns a callback function which performs BODY."
  `(lambda () (interactive)
     (lens--ui-callback ctx (lambda () ,@body))))


;;;; Use state

(defun lens--use-state-callback (path hook-idx value)
  (let* ((root-state (or lens--callback-state
                         (error "Called set-state outside of a ui callback")))
         (nested-state (lens--follow-key-path root-state path))
         (hooks (plist-get nested-state :hooks))
         (hook (nth hook-idx hooks)))
    (setf (cadr hook) value)))

(defun lens--use-state (ctx initial)
  (let* ((state (plist-get ctx :state))
         (hooks (plist-get state :hooks))
         (hook-idx (plist-get ctx :hook-idx))
         (path (plist-get ctx :path))
         hook)

    (if (plist-get ctx :is-first-call)
        ;; Add a hook to the hooks list
        (progn (cl-assert (eq (length hooks) hook-idx))
               (setq hook (list :use-state initial))
               (plist-put state :hooks (nconc hooks (list hook))))
      ;; Check if the indexed hook is a use-state hook
      (setq hook (nth hook-idx hooks))
      (unless hook (error "Called :use-state hook where no hook was previously"))
      (unless (eq (car hook) :use-state) (error "Called :use-state hook where %s hook was previously" (car hook))))

    (plist-put ctx :hook-idx (1+ hook-idx))
    (cons (cadr hook)
          (lambda (value) (lens--use-state-callback path hook-idx value)))))


;;;; Use effect

(defun lens--ui-run-effects (state path-to-posn &optional key-path)
  (let ((posn (or (gethash key-path path-to-posn)
                  (error "No position found for key path %s" key-path))))
    (dolist (hook (plist-get state :hooks))
      (when (and (eq (car hook) :use-effect)
                 (nth 3 hook))
        (funcall (cadr hook) (car posn) (cdr posn))))
    (let ((children (plist-get state :content)))
      (when (listp children)
        (dolist (child children)
          (lens--ui-run-effects (cdr child) path-to-posn
                                (append key-path (list (car child)))))))))

(defun lens--use-effect (ctx dependencies effect)
  "Define a use-effect hook.

The :use-effect hook has 3 parameters: 1. The callback. 2. The list of
values of the dependencies. 3. Whether the callback should be called on
this particular render.

The 3rd parameter is t if this is the first render, or if the
dependencies changed since the last render."
  (let* ((state (plist-get ctx :state))
         (hooks (plist-get state :hooks))
         (hook-idx (plist-get ctx :hook-idx))
         hook)

    (if (plist-get ctx :is-first-call)
        ;; Add a hook to the hooks list
        (progn (cl-assert (eq (length hooks) hook-idx))
               (setq hook (list :use-effect effect dependencies t))
               (plist-put state :hooks (nconc hooks (list hook))))
      ;; Check if the indexed hook is a use-effect hook
      (setq hook (nth hook-idx hooks))
      (unless hook (error "Called use-effect hook where no hook was previously"))
      (unless (eq (car hook) :use-effect) (error "Called use-effect hook where %s hook was previously" (car hook)))
      ;; Set the callback to the new callback
      (setf (cadr hook) effect)
      ;; Check if the dependencies have changed
      (if (equal dependencies (caddr hook))
          (setf (nth 3 hook) nil)
        (setf (nth 2 hook) dependencies)
        (setf (nth 3 hook) t)))

    (plist-put ctx :hook-idx (1+ hook-idx))))


;;;; Ui display

(defun lens-ui-display (ui-func)
  "Display spec for custom ui definitions.

UI is a function which takes a ui context and returns a list of
components.

The state for a particular component in the component tree is a plist
with the following properties:
  :hooks - A list of hooks called in the component, in order. Each call
    must have the same hooks and order. Each hook has the
    format (HOOK-KEYWORD ARGS...).
  :content - An alist from keys to child elements, or a string

The state for the display is the state for the root component, along
with the following properties:
  :text - The underlying text representation of the current state. The
    text can be changed by using the :use-text hook.
  :ui - A list of tuples (ELEM STRING KEY), which is the list of
    elements returned by the root element."

  (let ((ui-id (abs (random))))
    (list :tostate
          (lambda (text)
            (let ((state (lens--generate-ui ui-func nil nil ui-id)))
              (plist-put state :text text)
              (plist-put state :ui-id ui-id)
              state))
          :totext (lambda (state) (plist-get state :text))
          :insert
          (lambda (state)
            (cl-loop for ((key) . string) in (lens--ui-string-rows state nil)
                     for newline = (propertize "\n" 'read-only t 'lens-element-key key)
                     concat (concat string newline) into str
                     finally return (concat (propertize "\n" 'lens-element-key 'START) str))))))


;;;; Defui

(defvar lens-ui-alist nil)

(defmacro lens-ui-body (&rest body)
  `(lens-let-body
    [(:use-state
      lambda (body state-var set-var initial)
      `((let* ((=use-state= (lens--use-state (or lens--current-ctx (error "No containing lens context"))
                                             ,initial))
               (,state-var (car =use-state=))
               (,set-var (cdr =use-state=)))
          (cl-flet ((,set-var (cdr =use-state=)))
            ,@body))))
     (:use-effect
      lambda (body dependencies effect)
      `((lens--use-effect (or lens--current-ctx (error "No containing lens context"))
                          ,dependencies ,effect)
        ,@body))]
    ,@body))

(defmacro lens-defui (name arglist &rest body)
  (declare (indent 2))
  `(setf (alist-get ',name lens-ui-alist)
         (lambda ,arglist (lens-ui-body ,@body))))

(lens-defui test (ctx)
  (:use-state count set-count 1)
  (:use-effect count
               (lambda (ref)
                 (message "Effect!")))

  (:use-state text set-text "hi")

  (list `(wrapped-box :box1 ,text ,set-text)
        `(wrapped-box :box2 ,text ,set-text)
        `(counter :c1 "hi")
        `(counter :c2 "yo")))

(lens-defcomponent counter (ctx label)
  (:use-state count set-count 1)
  (:use-effect count (lambda (_start _end) (message "Counter %s changed to %s" label count)))
  (list `(string :count ,(format "%s: >%s<" label count))
        `(button :increment "++++++" ,(lambda () (set-count (1+ count))))))


;;;; Components

(defmacro lens-defcomponent (name arglist &rest body)
  "Define a UI component."
  (declare (indent 2))

  ;; Parse the beginning of the body as properties
  (let ((props nil))
    (while (keywordp (car body))
      (push (cons (pop body) (pop body)) props))

    (apply #'list #'prog1
           `(put ',name 'lens-component (lambda ,arglist (lens-ui-body ,@body)))
           (--map `(put ',name
                        ',(intern (concat "lens-component-"
                                          (substring (symbol-name (car it)) 1)))
                        ,(cdr it))
                  props))))

(lens-defcomponent box (_ctx text onchange &rest plist)
  (lens--field text
               (lambda (new _cursor)
                 (funcall onchange new))
               "[begin box]\n" "\n[end box]"))

(lens-defcomponent string (_ctx str)
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

(lens-defcomponent button (ctx label onclick &rest plist)
  :can-be-column t
  (propertize (format " %s " label)
              'lens-click
              (when onclick (lambda () (lens--ui-callback ctx onclick)))
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
    (unless (get (car elem) 'lens-component-can-be-column)
      (error "Invalid column component: %s" (car elem))))
  (lens--join-columns (--map (lens--ui-element-to-string it cb) cols)
                      (propertize " " 'read-only t)))


;;;; Border text box

(defcustom lens-box-enter-hook nil
  "Hook run after entering a box."
  :group 'lens
  :type 'hook)

(defcustom lens-box-exit-hook nil
  "Hook run after exiting a box."
  :group 'lens
  :type 'hook)

(defvar-local lens--focused-border-box nil
  "The focused border box in the current buffer, or nil.")

(defun lens--border-box-fix-cursor ()
  "When a box is active, this set as a local `post-command-hook'."
  (when lens--focused-border-box
    (-let (((&plist :last-point prev-pos :unique-id unique-id) lens--focused-border-box)
           (cur-line (line-number-at-pos)))
      (or (eq prev-pos (point))
          (eq (get-text-property (point) 'lens-border-box-content) unique-id)
          ;; Do not run if a modification was made
          (memq #'lens--field-modification-callback (default-value 'post-command-hook))
          ;; If we moved horizontally off of the line, go to the previous/next line
          (if (eq cur-line (line-number-at-pos prev-pos))
              (if (< (point) prev-pos)
                  (text-property-search-backward 'lens-border-box-eol unique-id #'eq)
                (or (text-property-search-forward 'lens-border-box-bol unique-id #'eq)
                    ;; The cursor is allowed to be at the end of the last line
                    (eq (get-text-property (1- (point)) 'lens-border-box-content) unique-id)))
            ;; If we moved to a different line, try to find the end of the line
            (goto-char (pos-eol))
            (and (text-property-search-backward 'lens-border-box-eol unique-id #'eq)
                 ;; If we went off the end of the textbox
                 (eq cur-line (line-number-at-pos))))
          ;; If everything else failed, go back to the last known position
          (progn (goto-char prev-pos)
                 (message "Press ESC to exit the textbox")))
      (plist-put lens--focused-border-box :last-point (point)))))

(defun lens--border-box-callback (body line-idx new-line-text cursor-col)
  (let ((new-content "")
        deleted-bol new-cursor line)

    (dotimes (i (length body))

      ;; If the user deleted the BOL character, delete a character from the previous line
      (setq deleted-bol (and (eq i line-idx)
                             (not (get-text-property 0 'lens-border-box-bol new-line-text))))
      (when (and deleted-bol (not (s-blank? new-content)))
        (setq new-content (substring new-content 0 -1)))
      (setq line (if (eq i line-idx)
                     (substring new-line-text (if deleted-bol 0 1) -1)
                   (car (nth i body))))

      ;; Subtract 1 from the cursor col to account for the ~ prefix
      (when (= i line-idx) (setq new-cursor (+ (length new-content) cursor-col
                                               (if deleted-bol 0 -1))))

      ;; Concat the line to the new content
      (if (and (not (s-blank? line))
               (get-text-property (1- (length line)) 'lens-textbox-newline line))
          (setq new-content (concat new-content (substring line 0 -1) "\n"))
        (setq new-content (concat new-content line))))

    (cons new-content new-cursor)))

(lens-defcomponent wrapped-box (ctx text set-text)
  (:use-state cursor set-cursor nil)
  (:use-state unique-id set-unique-id (abs (random)))

  (:let border-face 'shadow)

  (:pcase-let `(,body ,header ,footer ,cursor-line, cursor-col)
              (lens--textbox-wrap
               text :width 30
               :cursor cursor
               :box-props `(lens-textbox-border t read-only t face ,border-face)
               :title (if cursor "Esc to unfocus" "Enter to focus")
               :charset 'unicode))

  (:flet focus ()
         (set-cursor (length text))
         (setq lens--focused-border-box (list :unique-id unique-id :last-point (point)))
         (add-hook 'post-command-hook #'lens--border-box-fix-cursor nil 'local))
  (:flet unfocus ()
         (set-cursor nil)
         (setq lens--focused-border-box nil)
         (remove-hook 'post-command-hook #'lens--border-box-fix-cursor t))
  (:let map (if cursor
                `(keymap (escape . ,(lens-ui-callback ctx (unfocus))))
              `(keymap (return . ,(lens-ui-callback ctx (focus))))))

  ;; Run the hooks when focus changes
  (:use-effect
   (not cursor)
   (lambda (_ _) (run-hook-with-args (if cursor 'lens-box-enter-hook 'lens-box-exit-hook))))

  (:use-effect
   (list cursor-line cursor-col)
   (lambda (start end)
     (when (and cursor-line cursor-col lens--focused-border-box)
       (goto-char start)
       (forward-line (1+ cursor-line))
       (forward-char (+ 2 cursor-col))
       (plist-put lens--focused-border-box :last-point (point)))))

  (propertize
   (string-join
    (nconc (list (substring header 0 -1))
           (cl-loop for (content prefix suffix) in body and line-idx upfrom 0
                    for bol-char = (propertize "~" 'lens-border-box-bol unique-id 'rear-nonsticky t 'face border-face
                                               'read-only (eq line-idx 0))
                    for eol-char = (propertize "~" 'lens-border-box-eol unique-id 'read-only t 'face border-face)
                    collect
                    (lens--field
                     (concat bol-char (propertize content 'lens-border-box-content unique-id) eol-char)
                     (let ((cur-line-idx line-idx))
                       (lambda (newtext newcursor)
                         (lens--ui-callback
                          ctx
                          (lambda ()
                            (let ((res (lens--border-box-callback body cur-line-idx newtext newcursor)))
                              (funcall set-text (car res))
                              (set-cursor (cdr res)))))))
                     (substring prefix 0 -1)
                     (substring suffix 1 -1)
                     (unless cursor (list 'face 'shadow 'read-only t))))
           (list footer))
    (propertize "\n" 'read-only t))
   'keymap map))


;;; Usable functions
;;;; Ui Functions

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
