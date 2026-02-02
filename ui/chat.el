;;; chat.el -*- lexical-binding: t; -*-

(require 'dash)


(setq vec nil)

(lens-defui chat (ctx)
  (:use-state input set-input "hi")
  (:use-state history set-history nil)

  (:let llm-cb (lambda (output)
                 (lens--ui-callback
                  ctx
                  (lambda ()
                    ;; (message "CB %s" history)
                    (pcase-let ((`(,user . ,ai) (car history)))
                      (set-history `((,user . ,(concat ai output)) ,@(cdr history))))))))

  ;; Define a vector to hold the callback. Since vectors don't get
  ;; cloned, this serves as a cell which can 
  (:use-state llm-cb-cell set-llm-cb-cell (vector nil))
  (aset llm-cb-cell 0 llm-cb)

  (:let send-fn
        (lambda ()
          (set-input "")
          (set-history (cons (cons input "") history))

          (streaming-llm-request
           "openai/gpt-4.1-mini" input (reverse history)
           (streaming-llm--batch-callback
            (lambda (output)
              (unless (eq output 'done)
                (funcall (aref llm-cb-cell 0) output)))
            0.05))))

  `(,@(--map-indexed
       (list 'string (intern (format ":chat-%s" it-index))
             (concat (lens-text-to-box (car it)
                                       :width (min 60 (max 10 (+ (length (car it)) 4)))
                                       :title "User" :charset 'unicode
                                       :box-props '(face shadow))
                     "\n"
                     (let ((ai (if (s-blank? (cdr it)) "..." (cdr it))))
                       (lens-text-to-box ai
                                         :width (min 60 (max 8 (+ (length ai) 4)))
                                         :title "AI" :charset 'unicode
                                         :box-props '(face shadow)))))
       (reverse history))

    (wrapped-box :box ,input ,set-input)

    (button :send-button "Send" ,send-fn)))


(defun streaming-llm-request (model prompt history callback)
  "Make a streaming request to OpenRouter API.
  MODEL is the model ID (e.g., \"anthropic/claude-3.5-sonnet\").
  PROMPT is the user's prompt string.
  HISTORY is a list of (:user STRING :ai STRING) plists.
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
      (when (plist-get entry :user)
        (push `(("role" . "user")
                ("content" . ,(plist-get entry :user)))
              messages))
      (when (plist-get entry :ai)
        (push `(("role" . "assistant")
                ("content" . ,(plist-get entry :ai)))
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
