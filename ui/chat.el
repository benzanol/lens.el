;;; chat.el -*- lexical-binding: t; -*-

(require 'dash)


(setq vec nil)

(lens-defui chat (ctx)
  (:use-state input set-input "hi")
  (:use-state history set-history nil)

  (:flet make-box (title text)
         (lens-text-to-box text
                           :width 60 :shrink t
                           :title title :charset 'unicode
                           :box-props '(face shadow)))

  (:let llm-cb (lambda (output)
                 (lens--ui-callback
                  ctx
                  (lambda ()
                    (let ((first (car history)))
                      (setf (cadr first) (concat (cadr first) output))
                      (setf (nth 3 first) (make-box "AI" (cadr first)))
                      (set-history history))))))

  ;; Define a vector to hold the callback. Since vectors don't get
  ;; cloned, this serves as a cell which can
  (:use-state llm-cb-cell set-llm-cb-cell (vector nil))
  (aset llm-cb-cell 0 llm-cb)

  (:let send-fn
        (lambda ()
          (set-input "")
          (set-history (cons (list input "" (make-box "User" input) (make-box "AI" "...")) history))

          (streaming-llm-request
           "openai/gpt-4.1-mini" input (reverse history)
           (streaming-llm--batch-callback
            (lambda (output)
              (unless (eq output 'done)
                (funcall (aref llm-cb-cell 0) output)))
            0.05))))

  `(,@(--map-indexed
       (list 'string (intern (format ":chat-%s" it-index))
             (concat (nth 2 it) "\n" (nth 3 it)))
       (reverse history))

    (wrapped-box :box ,input ,set-input :onenter ,send-fn :width 50)

    (button :send-button "Send" ,send-fn)))


(defun streaming-llm-request (model prompt history callback)
  "Make a streaming request to OpenRouter API.
  MODEL is the model ID (e.g., \"anthropic/claude-3.5-sonnet\").
  PROMPT is the user's prompt string.
  HISTORY is a list of (USER-STRING AI-STRING) plists.
  CALLBACK is called with each chunk of generated text, or 'done when finished."
  (let* ((api-key (getenv "OPENROUTER_API_KEY"))
         (url "https://openrouter.ai/api/v1/chat/completions")
         (messages (streaming-llm--build-messages history prompt))
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'string)
         (request-body (json-encode
                        `(("model" . ,model)
                          ("messages" . ,messages)
                          ("stream" . t))))
         (buffer (generate-new-buffer " *streaming-llm*"))
         (line-buffer ""))
    (unless api-key
      (error "OPENROUTER_API_KEY environment variable not set"))

    (make-process
     :name "streaming-llm"
     :buffer buffer
     :command (list "curl" "-s" "-N"
                    "-X" "POST"
                    "-H" "Content-Type: application/json"
                    "-H" (concat "Authorization: Bearer " api-key)
                    "-d" request-body
                    url)
     :filter (lambda (proc output)
               (setq line-buffer (concat line-buffer output))
               (let ((lines (split-string line-buffer "\n")))
                 ;; Keep the last incomplete line in the buffer
                 (setq line-buffer (car (last lines)))
                 ;; Process all complete lines
                 (dolist (line (butlast lines))
                   (streaming-llm--process-line line callback))))
     :sentinel (lambda (proc event)
                 ;; Process any remaining data
                 (when (not (string-empty-p line-buffer))
                   (streaming-llm--process-line line-buffer callback))
                 (when (string-match-p "finished\\|exited" event)
                   (funcall callback 'done)
                   (kill-buffer (process-buffer proc)))))))

(defun streaming-llm--process-line (line callback)
  "Process a single LINE from the SSE stream."
  (cond
   ((string-match "^data: \\[DONE\\]" line)
    nil) ; done signal handled in sentinel
   ((string-match "^data: \\({.*}\\)" line)
    (condition-case err
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (json-array-type 'list)
               (json-data (json-read-from-string (match-string 1 line)))
               (choices (alist-get 'choices json-data))
               (delta (alist-get 'delta (car choices)))
               (content (alist-get 'content delta)))
          (when (and content (not (string-empty-p content)))
            (funcall callback content)))
      (error
       (message "Error parsing chunk: %s" err))))))

(defun streaming-llm--build-messages (history prompt)
  "Build messages array from HISTORY and PROMPT."
  (let ((messages '()))
    (dolist (entry history)
      (when (car entry)
        (push `(("role" . "user")
                ("content" . ,(car entry)))
              messages))
      (when (cadr entry)
        (push `(("role" . "assistant")
                ("content" . ,(cadr entry)))
              messages)))
    (push `(("role" . "user")
            ("content" . ,prompt))
          messages)
    (nreverse messages)))

(defun streaming-llm--batch-callback (callback interval)
  "Return a batched version of CALLBACK that groups calls by INTERVAL seconds.
  The returned callback accumulates strings and flushes them every INTERVAL.
  When called with 'done, it flushes any remaining content and calls the original callback."
  (let ((timer nil)
        (accumulated ""))
    (lambda (content)
      (cond ((eq content 'done)
             (when timer
               (cancel-timer timer)
               (funcall callback accumulated)
               (setq timer nil accumulated ""))
             (funcall callback 'done))

            ((not (string-empty-p content))
             ;; Accumulate content
             (setq accumulated (concat accumulated content))
             (unless timer
               (setq timer
                     (run-with-timer
                      interval nil
                      (lambda ()
                        (funcall callback accumulated)
                        (setq timer nil accumulated ""))))))))))
