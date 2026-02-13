;;; lui.el --- ui library for lens.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'lens)
(require 'text-property-search)


(defgroup lui nil
  "Customization group for lui.el."
  :prefix "lui-"
  :group 'lens)


;;; ============================================================
;;; Utils
;;;; Let macro

(defmacro lui-let-body (&rest body)
  "A macro designed to make ui component bodies simpler.

If any of body is (LET-KEYWORD VAR VAL), then this will be compiled to a
let expression containing the rest of the body. LET-KEYWORD can be any
of `:let', `:flet', `:when-let', or `:pcase-let'.

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
applying `lui-let-body' to the remaining body. The remaining arguments
are the list of arguments passed to the call. The return value is a list
of Lisp expressions to replace the original body.

For example, the following is expression used for use state:
  [(:use-state
    (lambda (body state-var set-var)
      `((let* ((=use-state= (lui--use-state))
               (,state-var (car =use-state=)))
          (cl-flet ((,set-var (cdr =use-state=)))
            ,@body)))))]

This allows for the following syntax:
  (:use-state count set-count)
  (set-count (1+ count))"

  (let ((let-keywords '((:let . let)
                        (:flet . cl-flet)
                        (:when-let . when-let)
                        (:pcase-let . pcase-let)))
        (custom (when (vectorp (car body)) (append (pop body) nil)))
        exprs func lets)
    ;; Go through the expressions in reverse
    (dolist (line (reverse body))
      (cond ((not (listp line)) (push line exprs))
            ((eq (car line) :let)
             (push (cadr line) lets)
             (push `(setq ,@(cdr line)) exprs))
            ((setq func (alist-get (car line) let-keywords))
             (setq exprs `((,func (,(cdr line)) ,@exprs))))
            ((setq func (alist-get (car line) custom))
             (setq exprs (apply func exprs (cdr line))))
            (t (push line exprs))))
    (cond (lets `(let ,lets ,@exprs))
          ((> (length exprs) 1) (cons #'progn exprs))
          (t (car exprs)))))


;;;; Diffing function

(defun lui-diff-lists (old-list new-list)
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


;;;; Box helpers

(defvar lui-textbox-chars
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

(defun lui-text-to-box (text &rest props)
  (pcase-let ((`(,body ,head ,foot) (apply #'lui--textbox-wrap text props)))
    (concat head
            (cl-loop for (content prefix suffix) in body
                     concat (concat prefix content suffix))
            foot)))

(defun lui--textbox-wrap (text &rest props)
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
  (lui-let-body
   (:let hpad (or (plist-get props :pad) 1))
   (:let title (plist-get props :title))
   (:let width (or (plist-get props :width) 50))
   (when (plist-get props :shrink)
     (let ((longest-line (cl-loop for line in (split-string text "\n")
                                  maximize (1+ (lens-string-width line)))))
       (setq width (min width (max (+ 2 (* hpad 2) longest-line)
                                   (if title (+ 6 (length title)) 0))))))

   (:let inner-width (- width 2 (* 2 hpad)))
   (unless (> inner-width 0) (error "Text box is too thin"))

   (:let cursor (plist-get props :cursor))
   (when (and cursor (> cursor (length text))) (error "Cursor position exceeds text length"))

   (:let chars (alist-get (or (plist-get props :charset) 'ascii)
                          lui-textbox-chars))

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
          (cond ((> (lens-string-width word) inner-width)
                 (when (string-empty-p (car body-lines)) (pop body-lines))
                 (while (> (lens-string-width word) inner-width)
                   (let ((idx (cl-loop for i from 1 below (1+ (length word))
                                       when (> (lens-string-width word 0 i) inner-width)
                                       do (cl-return (1- i))
                                       finally (cl-return (length word)))))
                     (push (substring word 0 idx) body-lines)
                     (setq word (substring word idx))))
                 (push word body-lines))
                ((or (> (+ (lens-string-width word) (lens-string-width (car body-lines)) (if is-word 1 0))
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
         (add-word (apply #'propertize "¶" 'lens-textbox-newline t 'face 'lens-field-header
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

   (list (cl-loop
          for line in body-lines
          collect (list line
                        (apply #'propertize (concat vert (make-string hpad ?\s))
                               'rear-nonsticky t
                               'front-sticky t
                               box-props)
                        (apply #'propertize
                               (concat (make-string (+ (- inner-width (lens-string-width line)) hpad) ?\s)
                                       vert "\n")
                               box-props)))
         (apply #'propertize head-str box-props)
         (apply #'propertize foot-str box-props)
         cursor-line
         cursor-col)))


;;;; Find element

(defun lui-find-element (state key-path)
  "Go to and return the element associated with KEY-PATH.

This will first find the lens associated with state STATE, and then find
the corresponding element within that lens.

Returns (START . END)."
  (let* ((region (lui-find-lens state))
         (hb (nth 1 region))
         (fe (nth 4 region)))
    (save-restriction
      (goto-char fe)
      (narrow-to-region hb fe)
      ;; This will get to the newline at the very end of the element
      (or (text-property-search-backward 'lens-element-key key-path #'equal)
          (text-property-search-backward
           'lens-element-key
           key-path
           (lambda (target found)
             ;; Checks if `found' starts with `target'
             (and (listp found)
                  (cl-search target found :end2 (length target)))))
          (error "Element end not found: %s" key-path))
      (forward-char -1)
      (let ((end (point)))
        (or (text-property-search-backward
             'lens-element-key
             key-path
             (lambda (target found)
               ;; Checks if `found' starts with `target'
               (and found
                    (not (and (listp found)
                              (cl-search target found :end2 (length target)))))))
            (error "Element start not found: %s" key-path))
        (forward-char 1)
        (cons (point) end)))))

(defun lui-find-ctx (ctx)
  "Wrapper for `lui-find-element' using a ctx object."
  (lui-find-element (plist-get ctx :root-state) (plist-get ctx :path)))


;;;; Ui element at point

(defun lui-element-at-point ()
  "Return the ui element at point, as ((STATE . KEY-PATH) START END).

START and END are of the element's inner content, not including the
bounding newlines."
  (let (lens me)
    (save-excursion
      (and
       (setq lens (lens-at-point))
       ;; Find the prop match at the end of the component
       (setq me (text-property-search-forward 'lens-element-key))
       ;; If the next prop match is a start, then we weren't inside of an element
       (not (eq (prop-match-value me) 'START))
       ;; Go to the prop match at the beginning of the component
       (progn (forward-char -1)
              (text-property-search-backward 'lens-element-key))
       ;; We are inside of a component
       (list (cons (car (caddr (car lens))) (prop-match-value me))
             (1+ (point))
             (prop-match-beginning me))))))


;;;; Save position in ui

(defmacro lui-save-position-in-ui (&rest body)
  "Save the current position relative to the containing ui element."
  `(let ((line (line-number-at-pos)) (col (current-column))
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
                     (text-property-search-forward 'lens-element-key ui-key #'equal))
              ;; Go to the beginning of the key region
              (progn (forward-char -1) (text-property-search-backward 'lens-element-key)))
         (progn (forward-line ui-line)
                (forward-char (min col (- (pos-eol) (point)))))
       (goto-char (point-min))
       (forward-line (1- line))
       (forward-char (min col (- (pos-eol) (point)))))))


;;; ============================================================
;;; Hooks
;;;; Hook helper macro

(defvar lui--generate-ui-ctx nil)

(defmacro lui--register-hook (ctx type &rest args)
  (declare (indent 2))
  `(let* ((state (plist-get ctx :state))
          (hooks (plist-get state :hooks))
          (hook-idx (plist-get ctx :hook-idx))
          hook)

     (if (plist-get ,ctx :is-first-call)
         ;; Add a hook to the hooks list
         (progn (cl-assert (eq (length hooks) hook-idx))
                (setq hook (list ,type ,@args))
                (plist-put state :hooks (nconc hooks (list hook))))
       ;; Check if the indexed hook has the correct type
       (setq hook (nth hook-idx hooks))
       (unless hook (error "Called %s hook where no hook was previously" ,type))
       (unless (eq (car hook) ,type)
         (error "Called %s hook where %s hook was previously" ,type (car hook))))
     (plist-put ctx :hook-idx (1+ hook-idx))
     hook))


;;;; Use callback

(defvar lui--modification-state nil
  "A copy of the ui state which is to be mutated by a callback.")

(defun lui--use-callback (name cb)
  "Register a :use-callback hook.

\(:use-callback NAME CALLBACK SYMBOL)

Where SYMBOL is a custom symbol whose function value is set to call this
hook. This function returns SYMBOL."

  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         (root-state (plist-get ctx :root-state))
         (ui-id (plist-get root-state :ui-id))
         (path (plist-get ctx :path))
         (hook
          (lui--register-hook ctx :use-callback
            name cb
            (let* ((str (cl-loop for sym in path
                                 concat (symbol-name sym) into str
                                 finally return (format "lens-%s%s:%s" ui-id str name)))
                   (ret-symbol (make-symbol str)))
              (fset ret-symbol
                    `(lambda (&rest args)
                       (interactive)
                       (lui--perform-callback ',root-state ',path ',name args)))
              ret-symbol))))
    (setf (caddr hook) cb)
    (nth 3 hook)))

(defun lui--follow-key-path (state path)
  (if (null path) state
    (lui--follow-key-path
     (or (alist-get (car path) (plist-get state :content))
         (error "Invalid component key path"))
     (cdr path))))

(defun lui--perform-callback (state path name args)
  "Perform a named callback.

STATE is the root ui state. PATH is the path to the component containing
the callback. NAME is the name of the callback."

  ;; Find the correct function to call
  (let* ((nested-state (lui--follow-key-path state path))
         (hook (or (seq-find (lambda (h) (and (eq (car h) :use-callback) (eq (cadr h) name)))
                             (plist-get nested-state :hooks))
                   (error "Callback not found %s %s" path name))))

    ;; If this is a callback called from within another callback
    (lui-modify state (caddr hook) args)))


;;;; Use focusable

(defvar-local lui-focused nil
  "The currently focused element in the current buffer.

This is nil, or a plist representing the current focused element.

The plist has the following internally managed properties:
  :element - A cons cell (STATE . PATH) representing the current element.
  :last-point - The position of the cursor after the previous command.

It also can have any of the following properties specified by the
use-focusable hook:
  :update - A function called every time the cursor moves while the
    element is focused. If it returns nil, then unfocus the element. If
    this property is omited, then the element will be immediately
    unfocused every time it is focused.
  :on-focus - Function called when focusing.
  :on-unfocus - Function called when unfocusing.
  :render - If non-nil, rerender the element when focused/unfocused.

All functions are called with one argument, which is the value of
`lui-focused'.")

(defun lui-find-focused (focused)
  "Helper function to find an element based on its focus spec."
  (let ((element (plist-get focused :element)))
    (lui-find-element (car element) (cdr element))))

(defun lui--focused-post-command ()
  "Handle updates for the currently focused element."
  (if (null lui-focused) (lui-unfocus)

    (unless (eq (point) (plist-get lui-focused :last-point))
      (let* ((fn (or (plist-get lui-focused :update) #'ignore))
             (stay-focused (with-demoted-errors "Error fixing focus: %s"
                             (funcall fn lui-focused))))
        (if stay-focused
            (plist-put lui-focused :last-point (point))
          (lui-unfocus))))))

(cl-defun lui--use-focusable (&rest plist)
  "Make the current element focusable.

See `lui-focused' for a list of valid properties for PLIST."

  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         hook)

    ;; Ensure that there are no other :use-focusable hooks
    (and (plist-get ctx :is-first-call)
         (seq-find (lambda (h) (eq (car h) :use-focusable)) (plist-get (plist-get ctx :state) :hooks))
         (error "You can only have one :use-focusable hook per element"))

    (setq hook (lui--register-hook ctx :use-focusable nil))
    (setf (cddr hook) plist)

    ;; Set the first argument to be whether it is focused
    (setf (cadr hook)
          (when lui-focused
            (let ((elem (plist-get lui-focused :element)))
              (and (eq (car elem) (plist-get ctx :root-state))
                   (equal (cdr elem) (plist-get ctx :path))))))
    (cadr hook)))

(defun lui--perform-focus-change (element focus unfocus)
  "Helper function to perform focus changes."
  (let ((fn #'(lambda (focus unfocus)
                (when-let ((on-focus (plist-get focus :on-focus)))
                  (with-demoted-errors "Error in focus: %s"
                    (funcall on-focus focus)))
                (when-let ((on-unfocus (plist-get unfocus :on-unfocus)))
                  (with-demoted-errors "Error in unfocus: %s"
                    (funcall on-unfocus unfocus))))))

    (if (or (plist-get focus :render) (plist-get unfocus :render))
        (lui-modify (car element) fn (list focus unfocus))
      (funcall fn focus unfocus))))

(defun lui-unfocus ()
  "Unfocus the focused element."
  (interactive)

  (when lui--modification-state
    (error "Cannot call `lui-unfocus' from inside of a callback"))

  (let ((old lui-focused))
    (setq lui-focused nil)
    (remove-hook 'post-command-hook #'lui--focused-post-command t)
    (when old (lui--perform-focus-change (plist-get old :element) nil old))))

(defun lui-focus (&optional ctx &rest relative-path)
  "Focus the element at RELATIVE-PATH relative to CTX.

If CTX is nil, then focus the element at point.

For example, if RELATIVE-PATH is nil, this will focus the current
element. If RELATIVE-PATH is (:a) it will focus the child with key :a.
The element being focused must have a :use-focusable hook."
  (interactive)

  (when lui--modification-state
    (error "Cannot call `lui-focus' from inside of a callback"))

  (if ctx (lui--focus (plist-get ctx :root-state) (append (plist-get ctx :path) relative-path))
    (cl-destructuring-bind ((state . path) _ _) (lui-element-at-point)
      (lui--focus state path))))

(defun lui--focus (state path)
  "Focus the element at PATH in STATE."
  (let* ((elem-state (or (lui--follow-key-path state path)
                         (error "Unable to focus element %s: Not found" path)))
         (hook (or (seq-find (lambda (h) (eq (car h) :use-focusable)) (plist-get elem-state :hooks))
                   (error "Unable to focus element %s: Not focusable" path)))
         (plist (cddr hook))
         (element (cons state path))
         (old-focused lui-focused)
         (old-element (plist-get old-focused :element))
         new-focused)

    (unless (equal element old-element)
      (setq new-focused (apply #'list :element element :last-point (point) plist))

      (setq lui-focused (when (plist-get plist :update) new-focused))
      (if lui-focused
          (add-hook 'post-command-hook #'lui--focused-post-command nil t)
        (remove-hook 'post-command-hook #'lui--focused-post-command t))

      (if (eq (car element) (car old-element))
          ;; different path in same state -- perform a single focus change for both
          (lui--perform-focus-change element new-focused old-focused)
        ;; old state != new state -- perform an unfocus then a focus
        (when old-element (lui--perform-focus-change old-element nil old-focused))
        (lui--perform-focus-change element new-focused nil)))))


;;;; Use state

(defun lui--use-state-callback (path hook-idx value)
  (let* ((root-state (or lui--modification-state
                         (error "Called set-state outside of a ui callback")))
         (nested-state (lui--follow-key-path root-state path))
         (hooks (plist-get nested-state :hooks))
         (hook (nth hook-idx hooks)))
    (setf (cadr hook) value)))

(defun lui--use-state (initial)
  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         (hook-idx (plist-get ctx :hook-idx))
         (path (plist-get ctx :path))
         (hook (lui--register-hook ctx :use-state initial))
         (value (cadr hook)))
    (cons value (lambda (value) (lui--use-state-callback path hook-idx value)))))


;;;; Use buffer state

(defun lui--collect-buffer-states (state path)
  (nconc (cl-loop for hook in (plist-get state :hooks)
                  for hook-idx from 0
                  when (eq (car hook) :use-buffer-state)
                  collect (list path hook-idx (copy-tree (cadr hook))))
         (when (listp (plist-get state :content))
           (cl-loop for (key . child) in (plist-get state :content)
                    nconc (lui--collect-buffer-states child (append path (list key)))))))

(defun lui--propogate-buffer-state (state buffer-states)
  "Update STATE with values from BUFFER-STATES.

Returns non-nil if any state was changed."
  (let ((any-changed nil))
    (pcase-dolist (`(,path ,hook-idx ,value) buffer-states)
      (when-let ((nested-state (lui--follow-key-path state path)))
        (let ((hook (nth hook-idx (plist-get nested-state :hooks))))
          (or (eq :use-buffer-state (car hook))
              (error "Misplaced buffer state hook: %s %s" path hook-idx))
          (unless (equal value (cadr hook))
            (setf (cadr hook) value)
            (setq any-changed t)))))
    any-changed))

(defun lui--use-buffer-state-callback (path hook-idx value)
  "Set the buffer state VALUE for the hook at HOOK-IDX in component at PATH."
  (let* ((root-state (or lui--modification-state
                         (error "Called set-buffer-state outside of a ui callback")))
         (nested-state (lui--follow-key-path root-state path))
         (hooks (plist-get nested-state :hooks))
         (hook (nth hook-idx hooks)))
    (setf (cadr hook) value)))

(defun lui--use-buffer-state (initial)
  "Create a buffer state hook with INITIAL value.

Buffer state is preserved in the undo history.
Returns a cons of (VALUE . SETTER-FUNCTION)."
  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         (hook-idx (plist-get ctx :hook-idx))
         (path (plist-get ctx :path))
         (hook (lui--register-hook ctx :use-buffer-state initial))
         (value (cadr hook)))
    (cons value (lambda (value) (lui--use-buffer-state-callback path hook-idx value)))))


;;;; Use effect

(defun lui--run-effects (state &optional key-path)
  "Run all pending use-effect hooks in STATE and its children.

KEY-PATH is the current path in the component tree."
  ;; Run effects for sub elements
  (let ((children (plist-get state :content)))
    (when (listp children)
      (dolist (child children)
        (lui--run-effects (cdr child) (append key-path (list (car child)))))))
  ;; Run effects for the current element
  (dolist (hook (plist-get state :hooks))
    (when (and (eq (car hook) :use-effect)
               (nth 3 hook))
      (funcall (cadr hook)))))

(defun lui--use-effect (dependencies effect)
  "Define a use-effect hook.

The :use-effect hook has 3 parameters: 1. The callback. 2. The list of
values of the dependencies. 3. Whether the callback should be called on
this particular render.

The 3rd parameter is t if this is the first render, or if the
dependencies changed since the last render."
  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         (hook (lui--register-hook ctx :use-effect
                 effect (not dependencies) t)))

    ;; Set the callback to the new callback
    (setf (cadr hook) effect)
    ;; Check if the dependencies have changed
    (if (equal dependencies (caddr hook))
        (setf (nth 3 hook) nil)
      (setf (caddr hook) dependencies)
      (setf (nth 3 hook) t))))


;;;; Use memo

(defun lui--use-memo (dependencies generator)
  "Store a memoized value.

GENERATOR is a 1-argument function which takes the old value and
generates a new value. DEPENDENCIES is a list of values. The memo will
be rerun when one of DEPENDENCIES changes."
  (let* ((ctx (or lui--generate-ui-ctx (error "No containing lens context")))
         (hook (lui--register-hook ctx :use-memo
                 generator dependencies (funcall generator nil))))

    ;; Set the generator to the new generator
    (setf (cadr hook) generator)
    ;; Check if the dependencies have changed
    (unless (equal dependencies (caddr hook))
      (setf (caddr hook) dependencies)
      (setf (nth 3 hook) (funcall generator (nth 3 hook))))
    (nth 3 hook)))


;;;; Use text properties

(defun lui--apply-text-properties (ctx properties)
  (save-excursion
    (pcase-let ((`(,start . ,end) (lui-find-ctx ctx))
                (ps properties)
                (prop nil) (value nil))
      (lens-silent-edit
       (while ps
         (setq prop (pop ps) value (pop ps))
         (pcase prop
           ('face (add-face-text-property start end value t))
           ('keymap
            (alter-text-property
             start end 'keymap
             (lambda (existing)
               ;; If the keymap is already in the existing, do nothing
               (cond ((or (eq existing value) (memq value (cdr existing))) existing)
                     ;; If the existing is already a list of keymaps, prepend
                     ((and (keymapp existing) (ignore-errors (seq-every-p #'keymapp (cdr existing))))
                      `(keymap ,@(cdr existing) value))
                     ;; If there is already a keymap, create one combining the two
                     (existing (list 'keymap existing value))
                     (value)))))
           (_ (put-text-property start end prop value))))))))

(defun lui--use-text-properties (properties)
  "Set additional text properties for the entire region."
  (let ((ctx (or lui--generate-ui-ctx (error "No containing lens context"))))
    (lui--use-effect (random) (lambda () (lui--apply-text-properties ctx properties)))))


;;; ============================================================
;;; Ui
;;;; Generating

(defun lui--generate-ui (elem path old-state root-state)
  "Generate the UI for a component.

ELEM is either (ELEM KEY ARGS...) or a ui function.

PATH is a list of keywords, the keys of each of the parent elements. For
the root element, this will be nil.

OLD-STATE is the previous state for this component (which will be
mutated), or nil if this is the first time generating the component.

ROOT-STATE is the root state for the ui, or nil if this is the root
component, and it is the first time generating the root component.

Returns the state value used for the new element. This will be either
OLD-STATE (if non nil), ROOT-STATE (if this is the root component), or a
newly created state value.

Creates an intermediate ui-generation context which is a plist containing:
  :state - The internal state for the component being called
  :root-state - The root state for the ui
  :is-first-call - Indicates whether calling a UI component for the
    first time, or a subsequent time.
  :path - The key path to the current level
  :hook-idx - How many hooks have been processed in the current level
  :id - A symbol containing the ui id and path"

  (let* ((state (or old-state (and (null path) root-state)
                    (list :hooks nil :content nil)))
         (ctx (list :state state
                    :root-state (or root-state state)
                    :path path
                    :hook-idx 0
                    :is-first-call (null old-state)
                    :id (intern (format "%s%s" (plist-get (or root-state state) :ui-id)
                                        (string-join (mapcar #'symbol-name path))))))

         ;; Perform the actual function
         (output (let ((lui--generate-ui-ctx ctx))
                   (if (functionp elem)
                       (funcall elem ctx)
                     (apply (get (car elem) 'lens-component) ctx (cddr elem)))))
         (old-children (unless (stringp (plist-get old-state :content)) (plist-get old-state :content))))

    (plist-put state :content
               (if (stringp output) output
                 ;; Recursively generate each of the child elements
                 (cl-loop with used-keys = nil
                          for child in output
                          for (_elem key . rest) = child
                          unless (keywordp key) do (error "First argument to component must be a keyword")
                          when (memq key used-keys) do (error "Duplicate child key: %s" key)
                          do (push key used-keys)
                          collect (cons key (lui--generate-ui
                                             child (append path (list key))
                                             (alist-get key old-children)
                                             (plist-get ctx :root-state))))))

    (plist-put state :element (unless (functionp elem) elem))))


;;;; Extract

(defun lui--state-to-string (state)
  "Extract string content from STATE, making an insertable string."
  (cl-loop for ((keys) . string) in (lui--string-rows state nil)
           for newline = (propertize "\n" 'read-only t 'lens-element-key keys)
           concat (concat string newline)))

(defun lui--nested-rerender-hooks (state)
  "Return a nested list of hooks which are relevant for rerendering.

This is for the purpose of calculating diffs between ui states."
  (let ((content (plist-get state :content)))
    (cons (cl-loop for hook in (plist-get state :hooks)
                   nconc (pcase (car hook)
                           ((or :use-state :use-buffer-state :use-focusable)
                            (list (cadr hook)))))
          (when (listp content)
            (cl-loop for (_key . state) in content
                     collect (lui--nested-rerender-hooks state))))))

(defun lui--string-rows (state key-path)
  "Returns a list of rows.

Each row has the form ((PATH ELEMENT STATE-HOOKS) . TEXT).

This format is designed to be suitable for diffing, as used in
`lui-rerender'."
  (let* ((content (plist-get state :content))
         (element (plist-get state :element))
         (renderer (plist-get state :renderer)))
    (if (or (stringp content) renderer)
        (list (cons (list key-path element (lui--nested-rerender-hooks state))
                    (if (not renderer) content
                      (funcall renderer
                               (cl-loop for (_key . state) in content
                                        collect (lui--state-to-string state))))))
      (cl-loop for (key . state) in content
               nconc (lui--string-rows state (append key-path (list key)))))))


;;;; Rerender

(defcustom lui-debug nil
  "Enable debugging features."
  :group 'lui
  :type 'boolean)

;; Basic gray background
(defface lui-rerender-highlight '((t (:background "#505464")))
  "Briefly highlight the sections of the lens that are rerendered.")


(defun lui-rerender (state old-rows he _fb)
  (let* ((new-rows (lui--string-rows state nil))
         (diff (lui-diff-lists old-rows new-rows))
         new-fb)

    (lui-save-position-in-ui
     (goto-char (1+ he))
     ;; Apply each diff element
     (pcase-dolist (`(,action (,key-path) ,_old_str ,new-str) diff)
       (let ((start (point)) match)
         (if (eq action :insert)
             (progn (insert new-str (propertize "\n" 'read-only t 'lens-element-key key-path))
                    (when lui-debug
                      (let ((ol (make-overlay start (point))))
                        (overlay-put ol 'face 'lui-rerender-highlight))))
           ;; The match is the newline at the end of the old element
           (setq match (text-property-search-forward 'lens-element-key))
           (cl-assert match)
           (cl-assert (equal (prop-match-value match) key-path))
           (when (eq action :delete) (delete-region start (point))))))
     (setq new-fb (point)))

    (when lui-debug
      (run-with-timer
       0.07 nil
       #'(lambda (buf)
           (with-current-buffer buf
             (remove-overlays nil nil 'face 'lui-rerender-highlight)))
       (current-buffer)))

    new-fb))


;;;; Modify

(defun lui-modify (state callback args)
  "Call CALLBACK with ARGS, updating the ui associated with STATE.

CALLBACK is not allowed to modify the buffer contents in any way,
including any action that updates another lens (such as unfocusing the
focused element.)"
  (if lui--modification-state
      (apply callback args)

    ;; This is the root-level callback, so we need to handle updating the lens
    (let (old-rows region)
      (with-current-buffer (plist-get state :buffer)
        (save-excursion
          (goto-char (point-min))

          (or (text-property-search-forward
               'lens-begin state
               #'(lambda (st lens) (eq st (car (caddr lens)))))
              (error "Lens not found"))

          (setq region (lens-at-point)))
        (setq old-rows (lui--string-rows state nil))

        ;; Since we specifically searched for a lens with the correct state,
        ;; this should never be an issue, but check just in case
        (unless (eq (car (caddr (car region))) state)
          (error "Lens at point is for the incorrect state"))

        ;; Update the actual state based on the buffer state
        (when (lui--propogate-buffer-state state (cdr (caddr (car region))))
          (message "Detected buffer state change, pre-regenerating ui!")
          (lui--generate-ui (plist-get state :ui-func) nil
                            state state))

        (let ((lui--modification-state state))
          (apply callback args))

        (lui--generate-ui (plist-get state :ui-func) nil
                          state state)

        (lens-modify region (cons state (lui--collect-buffer-states state nil)) nil
                     (lambda (he fb) (lui-rerender state old-rows he fb)))

        (lui--run-effects state)

        (when lui-focused (plist-put lui-focused :last-point (point)))))))


;;;; Ui display

(defun lui-display (ui-func)
  "Display spec for custom ui definitions.

UI is a function which takes a ui context and returns a list of
components.

Each component has \"component state,\" which is a plist with the
following properties:
  :hooks - A list of hooks called in the component, in order. Each call
    must have the same hooks and order. Each hook has the
    format (HOOK-KEYWORD ARGS...).
  :content - An alist from keys to child component states, or a string
  :renderer? - When :content is an alist, this is a function which takes
    the insert strings of each of the child components, and returns the
    insert string of the current component.

The \"ui state\" is the state for the root component, along with the
following properties:
  :original-text - The original input text that the ui was created with
  :ui-id - Unique id for this ui

The display state is a cons cell, (UI-STATE . LOCAL-STATE). The way that
lenses work, is that the state of each version of a lens must be a
different object, so that we can move through the history. It would be
costly to store an entire copy of the entire ui state for every time
stamp. Thus, the UI-STATE is mutated, and its value shared between all
versions of the lens, whereas LOCAL-STATE store the time-dependenet
aspects of the lens which are copied for every version.

Specifically, LOCAL-STATE is an alist of path symbols to the state
values. If you have a call (:use-local-state count set-count 10) in the
component (:root :column), then the alist would look like

\((:root:column:0 . 10))

where :root:column:count is a single symbol. The path symbol is equal to
the child keys concatenated with the index of the :use-local-state
hook among all hooks in the current component."

  (let ((ui-id (abs (random))))
    (list :tostate
          (lambda (text)
            (let* ((state (list
                           :original-text text
                           :ui-id ui-id
                           :buffer (current-buffer)
                           :ui-func ui-func)))
              (cons (lui--generate-ui ui-func nil nil state)
                    (lui--collect-buffer-states state nil))))
          :totext (lambda (state) (plist-get state :text))
          :insert
          (lambda (state)
            (run-with-timer 0 nil #'lui--run-effects (car state))
            (concat (propertize "\n" 'lens-element-key 'START)
                    (lui--state-to-string (car state)))))))


;;;; Defui

(defvar lui-ui-alist nil)

(defmacro lui-component-body (&rest body)
  `(lui-let-body
    [(:use-callback
      lambda (body name &rest cb)
      (when (and (> (length cb) 1) (not (and (listp (car cb)) (seq-every-p #'symbolp (car cb)))))
        (error "If :use-callback has more than 2 arguments, the second must be an argument list"))
      `((let* ((cb ,(if (eq (length cb) 1) (car cb) (cons #'lambda cb)))
               (,name (lui--use-callback ',name cb)))
          ,@body)))
     (:use-state
      lambda (body state-var set-var initial)
      (unless (symbolp state-var) (error "First argument to :use-state must be a variable symbol"))
      (unless (symbolp set-var) (error "Second argument to :use-state must be a function symbol"))
      `((let* ((=use-state= (lui--use-state ,initial))
               (,state-var (car =use-state=))
               (,set-var (cdr =use-state=)))
          (cl-flet ((,set-var ,set-var))
            ,@body))))
     (:use-buffer-state
      lambda (body state-var set-var initial)
      (unless (symbolp state-var) (error "First argument to :use-buffer-state must be a variable symbol"))
      (unless (symbolp set-var) (error "Second argument to :use-buffer-state must be a function symbol"))
      `((let* ((=use-state= (lui--use-buffer-state ,initial))
               (,state-var (car =use-state=))
               (,set-var (cdr =use-state=)))
          (cl-flet ((,set-var ,set-var))
            ,@body))))
     (:use-memo
      lambda (body var deps &rest cb-body)
      (unless (symbolp var) (error "First argument to :use-memo must be a variable symbol"))
      (unless (vectorp deps) (error "Second argument to :use-memo must be a dependency vector"))
      `((let ((,var (lui--use-memo (list ,@deps ,@nil) (lambda (,var) (ignore ,var) ,@cb-body))))
          ,@body)))
     (:use-effect
      lambda (body deps &rest cb-body)
      (unless (vectorp deps) (error "First argument to :use-effect must be a dependency vector"))
      `((lui--use-effect (list ,@deps ,@nil) (lambda () ,@cb-body))
        ,@body))
     (:use-text-properties
      lambda (body &rest properties)
      `((lui--use-text-properties ,(cons #'list properties))
        ,@body))
     (:use-renderer
      lambda (body ctx renderer)
      (let ((ctx-expr '(or lui--generate-ui-ctx (error "No containing lens context"))))
        (cons `(plist-put (plist-get ,ctx-expr :state) :renderer ,renderer) body)))
     (:use-focusable
      lambda (body var &rest plist)
      `((let ((,var (lui--use-focusable ,@plist)))
          ,@body)))]
    ,@body))

(defmacro lens-defui (name arglist &rest body)
  (declare (indent 2))
  `(prog1 ',name
     (setf (alist-get ',name lui-ui-alist)
           (byte-compile
            (lambda ,@(cl--transform-lambda (list arglist (cons #'lui-component-body body)) name))))))


;;; ============================================================
;;; Components
;;;; Defcomponent

(defmacro lens-defcomponent (name arglist &rest body)
  "Define a UI component."
  (declare (indent 2))

  ;; Parse the beginning of the body as properties
  (let ((props nil))
    (while (keywordp (car body))
      (push (cons (pop body) (pop body)) props))

    (apply #'list #'prog1
           `',name
           `(put ',name 'lens-component
                 (byte-compile
                  (lambda ,@(cl--transform-lambda (list arglist (cons #'lui-component-body body)) name))))
           (cl-loop for (prop . value) in props
                    for sym-name = (concat "lens-component-" (substring (symbol-name prop) 1))
                    collect `(put ',name ',(intern sym-name) ,value)))))

(lens-defcomponent field (_ctx text onchange &rest plist)
  (lens--field text
               (lambda (new _cursor)
                 (funcall onchange new))
               (or (plist-get plist :header) "[begin box]\n")
               (or (plist-get plist :footer) "\n[end box]")))

(lens-defcomponent string (_ctx str)
  :can-be-column t
  (propertize (if (stringp str) str (string-join str "\n"))
              'read-only t))


;;;; Button

(lens-defcomponent button (_ctx label onclick &key face)
  :can-be-column t
  (propertize (format " %s " label)
              'lens-click onclick
              'keymap lens-button-map
              'face (or face 'lens-button)
              'font-lock-face (or face 'lens-button)
              'read-only t))


;;;; Columns

(defun lui--join-columns (cols &optional sep)
  "Vertically align a list of strings as columns.

COLS is the list of strings to align, and SEP is an additional
string to insert between the columns."
  (let* ((splits (cl-loop for col in cols collect (split-string (replace-regexp-in-string "\n\\'" "" col) "\n")))
         (line-ct (apply #'max (mapcar #'length splits)))
         (lines (make-list line-ct ""))
         max-w cell-text)
    (dolist (col-lines splits)
      (setq max-w (apply #'max (mapcar #'lens-string-width col-lines)))

      (dotimes (i line-ct)
        (setq cell-text (or (nth i col-lines) ""))
        (setf (nth i lines)
              (concat (nth i lines)
                      (or (unless (eq col-lines (car splits)) sep) "")
                      cell-text
                      (make-string (- max-w (lens-string-width cell-text)) ?\s)))))
    (string-join lines (propertize "\n" 'read-only t))))

(lens-defcomponent columns (_ctx &rest columns)
  ;; Ensure that all are valid columns
  (dolist (elem columns)
    (unless (get (car elem) 'lens-component-can-be-column)
      (error "Invalid column component: %s" (car elem))))

  (:use-renderer ctx (lambda (cols) (lui--join-columns cols (propertize " " 'read-only t))))
  columns)

(lens-defcomponent rows (_ctx &rest rows)
  :can-be-column t
  rows)


;;;; Text field

(lens-defcomponent text-field (ctx text set-text &key header footer props)
  (:use-callback onchange (newtext _newcursor)
                 (funcall set-text newtext))

  (setq header (propertize (or header "[begin field]\n") 'face 'lens-field-header))
  (setq footer (apply #'propertize (or footer "\n[end field]") 'face 'lens-field-header props))

  (:use-focusable _focused?
                  :on-focus
                  (lambda (_)
                    (goto-char (- (cdr (lui-find-ctx ctx))
                                  (length footer) -1))))

  (lens--field text onchange header footer props))


;;;; Text box

(lens-defcomponent text-box (ctx text set-text &key onenter width)
  :can-be-column t

  (:use-state cursor set-cursor nil)

  (:use-state unique-id _set-unique-id (random))
  (:use-focusable focused?
                  :render t
                  :info (list unique-id)
                  :update #'lui--text-box-update-cursor
                  :on-focus (lambda (_) (run-hook-with-args 'lui-text-box-enter-hook))
                  :on-unfocus (lambda (_) (run-hook-with-args 'lui-text-box-exit-hook)))

  ;; The real value to use for the cursor
  (setq cursor (when focused? (min (or cursor (length text)) (length text))))

  (:let border-face 'lens-field-header)

  (:pcase-let `(,body ,header ,footer ,cursor-line, cursor-col)
              (lui--textbox-wrap
               text :width (or width 30)
               :cursor cursor
               :box-props `(lens-textbox-border t read-only t face ,border-face)
               :title (if focused? "Esc to unfocus" "Enter to focus")
               :charset 'unicode))

  (:use-callback focus () (lui-focus ctx))
  (:use-callback unfocus () (lui-unfocus))

  ;; Update the cursor at the end
  (:use-effect
   [focused? cursor-line cursor-col]
   (when (and focused? cursor-line cursor-col)
     (lui-find-ctx ctx)
     (forward-line (1+ cursor-line))
     (text-property-search-forward 'lens-border-box-bol unique-id #'eq)
     (forward-char cursor-col)))

  (:use-callback
   change-cb (newtext newcursor line-idx)
   (let ((res (lui--text-box-callback body line-idx newtext newcursor)))
     (funcall set-text (car res))
     (set-cursor (cdr res))))

  (:use-memo focused-map [onenter] `(keymap (escape . ,unfocus) (return . ,onenter)))
  (:use-memo unfocused-map [] `(keymap (return . ,focus)))
  (:use-text-properties 'keymap (if focused? focused-map unfocused-map))

  (string-join
   (nconc (list (substring header 0 -1))
          (cl-loop for (content prefix suffix) in body and line-idx upfrom 0
                   for bol-char = (propertize "~" 'lens-border-box-bol unique-id 'rear-nonsticky t 'face border-face
                                              'read-only (eq line-idx 0))
                   for eol-char = (propertize "~" 'lens-border-box-eol unique-id 'read-only t 'face border-face)
                   collect
                   (lens--field
                    (concat bol-char (propertize content 'lens-border-box-content unique-id) eol-char)
                    (let ((scoped-line-idx line-idx))
                      (lambda (newtext newcursor)
                        (funcall change-cb newtext newcursor scoped-line-idx)))
                    (substring prefix 0 -1)
                    (substring suffix 1 -1)
                    (unless focused? (list 'face border-face 'read-only t))))
          (list footer))
   (propertize "\n" 'read-only t)))

(defcustom lui-text-box-enter-hook nil
  "Hook run after entering a box."
  :group 'lui
  :type 'hook)

(defcustom lui-text-box-exit-hook nil
  "Hook run after exiting a box."
  :group 'lui
  :type 'hook)

(cl-defun lui--text-box-update-cursor ((&key info last-point &allow-other-keys))
  (let ((cur-line (line-number-at-pos))
        (unique-id (car info)))

    (or (eq last-point (point))
        (eq (get-text-property (point) 'lens-border-box-content) unique-id)
        ;; Do not run if a modification was made
        (memq #'lens--field-modification-callback (default-value 'post-command-hook))
        ;; If we moved horizontally off of the line, go to the previous/next line
        (if (eq cur-line (line-number-at-pos last-point))
            (if (< (point) last-point)
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
        (when (eq (or (get-text-property last-point 'lens-border-box-content)
                      (get-text-property last-point 'lens-border-box-eol))
                  unique-id)
          (goto-char last-point)
          (message "Press ESC to exit the textbox")
          t))))

(defun lui--text-box-callback (body line-idx new-line-text cursor-col)
  "Process a modification to a border box.

BODY is the list of lines, LINE-IDX is the modified line index,
NEW-LINE-TEXT is the new text, and CURSOR-COL is the cursor column.
Returns (NEW-CONTENT . NEW-CURSOR)."
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


;;;; Select

(defface lui-select-selected '((t :inherit highlight))
  "Face for the selected option in a lens select component.")

(defface lui-select-unselected '((t :inherit default))
  "Face for unselected options in a lens select component.")

(lens-defcomponent select (ctx options current set-current &key horizontal selected-face unselected-face)
  :can-be-column t

  (:let unique-id (plist-get ctx :id))

  (:use-callback next-option () (funcall set-current (mod (1+ current) (length options))))
  (:use-callback prev-option () (funcall set-current (mod (1- current) (length options))))

  (:let fix-cursor (lambda (ps) (plist-put ps :state cursor-type) (setq-local cursor-type 'hbar)))
  (:use-focusable focused?
                  :render t
                  :update (lambda (ps)
                            (unless (eq cursor-type 'hbar) (funcall fix-cursor ps))
                            (cond ((> (point) (plist-get ps :last-point)) (funcall next-option))
                                  ((< (point) (plist-get ps :last-point)) (funcall prev-option))))
                  :on-focus fix-cursor
                  :on-unfocus (lambda (ps) (setq-local cursor-type (plist-get ps :state))))
  ;; Go to the start of the selected option after rendering
  (:use-effect
   [focused? current]
   (when focused?
     (goto-char (point-min))
     (let ((m (text-property-search-forward 'lui:select-selected unique-id #'eq)))
       (when m (goto-char (prop-match-beginning m))))))

  (:use-memo focused-map [] `(keymap (escape . lui-unfocus)))
  (:use-memo unfocused-map [] `(keymap (return . lui-focus)))
  (:use-text-properties 'keymap (if focused? focused-map unfocused-map)
                        'read-only t)

  ;; Build the output string
  (let ((sel-face (or selected-face 'lui-select-selected))
        (unsel-face (or unselected-face 'lui-select-unselected)))
    (propertize
     (string-join
      (cl-loop for opt in options
               for idx from 0
               when (and (= idx 0) focused?) collect (format "Focused: %s" current)
               if (eq idx current)
               collect (propertize (format "%s" opt) 'face sel-face 'lui:select-selected unique-id)
               else
               collect (propertize (format "%s" opt) 'face unsel-face))
      (if horizontal "  " "\n")))))


;;; ============================================================
;;; Inserting

(defun lui-read-ui (&optional prompt)
  "Prompt the user to select a UI from `lui-ui-alist'.

PROMPT is the prompt string to display."
  (let ((name (completing-read (or prompt "Ui: ") (mapcar #'car lui-ui-alist))))
    (alist-get (intern name) lui-ui-alist)))

(defun lui-insert-ui (beg end ui)
  "Create a lens displaying UI, replacing region from BEG to END."
  (interactive (list (point) (if mark-active (mark) (point)) (lui-read-ui)))
  (lens--replace-create beg end #'lens-replace-source (lui-display ui)))

(defun lui-insert-buffer-ui (beg end buffer ui)
  "Create a lens displaying BUFFER with UI, replacing region from BEG to END."
  (interactive (list (point) (if mark-active (mark) (point))
                     (read-buffer "Buffer: ") (lui-read-ui)))
  (lens--replace-create beg end (lambda (str) (lens-buffer-source buffer str)) (lui-display ui)))


;;; ============================================================
;;; End

(provide 'lui)
;;; lui.el ends here
