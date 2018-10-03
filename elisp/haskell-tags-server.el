;; haskell-tags-server.el --- -*- lexical-binding: t; -*-

;; Copyright (C) Sergey Vinokurov
;;
;; Author: Sergey Vinokurov <serg.foo@gmail.com>
;; Created: Monday,  3 October 2016
;; Description:

(require 'bert)
(require 'bindat)
(require 'dash)
(require 'haskell-mode)
(require 'queue)

(require 'select-mode)

(defgroup haskell-tags-server nil
  "Navigate Haskell sources using tags server."
  :group 'haskell
  :link '(url-link :tag "Github" "https://github.com/sergv/haskell-tags-server"))

(defcustom haskell-tags-server-executable "haskell-tags-server"
  "Path to the haskell-tags-server executable."
  :group 'haskell-completions
  :type 'stringp)

(defcustom haskell-tags-server-port 10000
  "Port to user for connecting to Haskell tags server."
  :group 'haskell-completions
  :type 'integerp)

(defconst haskell-tags-server--network-buffer-name " *haskell-tags-server-network*")
(defconst haskell-tags-server--subprocess-buffer-name " *haskell-tags-server-subprocess*")

(defvar haskell-tags-server--network-buffer nil)
(defvar haskell-tags-server--network-proc nil)
(defvar haskell-tags-server--subprocess-buffer nil)
(defvar haskell-tags-server--subprocess-proc nil)

(defvar haskell-tags-server--reported-shallow-dirs   (make-hash-table :test #'equal))
(defvar haskell-tags-server--reported-recursive-dirs (make-hash-table :test #'equal))
(defvar haskell-tags-server--reported-ignored-globs  (make-hash-table :test #'equal))

(defun haskell-tags-server-ensure-connected ()
  (unless (haskell-tags-server--is-connected? haskell-tags-server--network-proc)
    (unless (haskell-tags-server--connect)
      (setf haskell-tags-server--subprocess-buffer
            (get-buffer-create haskell-tags-server--subprocess-buffer-name))
      (let ((proc (make-process
                   :name "haskell-tags-server"
                   :buffer haskell-tags-server--subprocess-buffer
                   :command (list haskell-tags-server-executable
                                  "--port"
                                  (number-to-string haskell-tags-server-port)
                                  "--verbosity"
                                  "error"
                                  ;; "--eager-tagging"
                                  )
                   :noquery t
                   :sentinel #'haskell-tags-server--subprocess-sentinel)))
        (let ((tries 10)
              (done nil))
          (while (not done)
            (setf tries (- tries 1)
                  done (or (haskell-tags-server--connect)
                           (= 0 tries)))
            (sit-for 0.1))
          (when (not done)
            (delete-process proc)
            (kill-buffer haskell-tags-server--subprocess-buffer)
            (error "Failed to initiate connection to the freshly created haskell-tags-server subprocess")))))))

(defun haskell-tags-server--subprocess-sentinel (proc event-description)
  (unless (memq (process-status proc) '(run open))
    (message "The haskell-tags-server subprocess terminated: %s" event-description)))

(defun haskell-tags-server--is-connected? (proc)
  "Check whether we are connected to a `haskell-tags-server' process."
  (and proc
       (and (eq 'open (process-status proc)))))

(defun haskell-tags-server-connect ()
  (unless (haskell-tags-server--connect)
    (error "Failed to connect: %s" err)))

(defun haskell-tags-server--connect ()
  (setf haskell-tags-server--network-buffer
        (get-buffer-create haskell-tags-server--network-buffer-name))
  (condition-case err
      (progn
        (setf haskell-tags-server--network-proc
              (make-network-process
               :name "haskell-tags-server"
               :host 'local
               :service haskell-tags-server-port
               :buffer haskell-tags-server--network-buffer
               :coding 'no-conversion
               :filter-multibyte nil
               ;; Don't query to close this process on Emacs exit.
               :noquery t
               :filter #'haskell-tags-server--process-filter
               :sentinel #'haskell-tags-server--network-sentinel)
              haskell-tags-server--response-decoder
              (haskell-tags-server--make-bert-decoder #'haskell-tags-server--register-response-for-processing))
        t

        ;; (set-process-filter-multibyte haskell-tags-server--network-proc nil)
        ;; (set-process-filter-multibyte haskell-tags-server--network-proc t)
        ;; (set-process-coding-system    haskell-tags-server--network-proc 'utf-8 'utf-8)
        ;; (set-process-filter           haskell-tags-server--network-proc #'haskell-tags-server--process-filter)
        )
    (file-error nil)))

(defvar haskell-tags-server--response-timer nil
  "Variable that contains the next scheduled action that will handle the
most recently arrived server response.")

(defvar haskell-tags-server--pending-responses (make-queue)
  "A queue of server responses that we have yet to process.")

(defun haskell-tags-server--register-response-for-processing (response)
  "Register fresh server RESPONSE to be processed at our earliest convenience."
  (cond
    (haskell-tags-server--response-timer
     ;; A response is already being processed.
     (queue-enqueue haskell-tags-server--pending-responses response))
    (t
     ;; Not processing a response at the moment.
     (setf haskell-tags-server--response-timer
           (run-at-time 0.00001 nil #'haskell-tags-server--process-pending-responses response)))
    ;; ((queue-empty haskell-tags-server--pending-responses)
    ;;  ;; Not processing a response at the moment.
    ;;  (haskell-tags-server--process-pending-responses response))
    ;; (t
    ;;  (queue-enqueue haskell-tags-server--pending-responses response)
    ;;  (haskell-tags-server--process-pending-responses
    ;;   (queue-dequeue haskell-tags-server--pending-responses)))
    ))

(defun haskell-tags-server--process-pending-responses (response)
  "Properly process all pending server's response"
  (unwind-protect
      (haskell-tags-server--process-response response)
    (progn
      (setf haskell-tags-server--response-timer
            (if (queue-empty haskell-tags-server--pending-responses)
                nil
              (run-at-time 0.00001 nil #'haskell-tags-server--process-pending-responses
                           (queue-dequeue haskell-tags-server--pending-responses)))))))


(defvar haskell-tags-server--last-call-id nil)

(defvar haskell-tags-server--response-handlers (make-hash-table :test #'equal)
  "Hash table from call ids to functions of one argument (the
reply) that should process the reply when it arrives.")

(defun haskell-tags-server--next-call-id ()
  (setf haskell-tags-server--last-call-id
        (+ (or haskell-tags-server--last-call-id -1) 1))
  haskell-tags-server--last-call-id)

(defun haskell-tags-server--call (proc func-name args callback)
  (cl-assert (symbolp func-name))
  (cl-assert (vectorp args))
  (unless proc
    (error "No server process to send to"))
  (let* ((next-call-id (haskell-tags-server--next-call-id))
         (invocation
          (vector 'call 'haskell-tags-server func-name (list next-call-id args))))
    (puthash next-call-id callback haskell-tags-server--response-handlers)
    (condition-case err
        (process-send-string haskell-tags-server--network-proc
                             (haskell-tasg-server--bert-encode-message
                              (bert-pack invocation)))
      (error
       (remhash next-call-id haskell-tags-server--response-handlers)
       (signal (car err) (cdr err))))))

(defun haskell-tags-server--process-response (response)
  "Properly process server's response"
  (pcase (bert-unpack response)
    ;; -- {reply, Result}
    (`[reply [,call-id [ok ,data]]]
     (let ((handler (gethash call-id haskell-tags-server--response-handlers)))
       (remhash call-id haskell-tags-server--response-handlers)
       (if handler
           (funcall handler data)
         (error "Got response for a call we didn't make (%s): %s"
                call-id
                response))))
    ;; -- {reply, Result}
    (`[reply [,call-id [error ,data]]]
     (remhash call-id haskell-tags-server--response-handlers)
     (error "Error from haskell tags server: %s" data))
    ;; -- {error, {Type, Code, Class, Detail, Backtrace}}
    (`[error [,type ,code ,class ,detail ,_backtrace]]
     (error "BERT-RPC error. Type: %s, code: %s, class: %s. Detail:\n%s" type code class detail))
    (_
     (error "Unexpected BERT-RPC response: %s" response))))

(defun haskell-tags-server--make-bert-decoder (on-request)
  "Make function that will receive new batch of data from server and decide
whether we have received complete message already or must wait for more data to
arrive."
  (letrec
      ((expected-size-length 4) ;; How much bytes are missing until we know the size.
       (size-bytes nil)         ;; Bytes that constitute size.
       (expected-request-size nil) ;; Size of the next request.
       (request-chunks nil) ;; Reversed list of strings that constitute next request.
       (decode
        (lambda (data)
          (cl-assert (stringp data))
          (cl-assert (not (multibyte-string-p data)))
          (let ((len (length data)))
            (if (= 0 expected-size-length)
                (progn
                  (cl-assert (numberp expected-request-size))
                  (cl-assert (not (= 0 expected-request-size)))
                  (if (< len expected-request-size)
                      (setf expected-request-size (- expected-request-size len)
                            request-chunks (cons data request-chunks))
                    (let* ((last-request-chunk (subseq data 0 expected-request-size))
                           (trailing-data (subseq data expected-request-size))
                           (request
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (dolist (chunk (nreverse (cons last-request-chunk request-chunks)))
                                (cl-assert (not (multibyte-string-p chunk)))
                                (insert chunk))
                              (buffer-substring-no-properties (point-min) (point-max)))))
                      (cl-assert (not (multibyte-string-p request)))
                      (setf expected-size-length 4
                            size-bytes nil
                            expected-request-size nil
                            request-chunks nil)
                      (funcall on-request request)
                      (unless (= 0 (length trailing-data))
                        (funcall decode trailing-data)))))
              ;; Start receiving new message
              (if (< len expected-size-length)
                  ;; Store bytes to get the length later
                  (setf expected-size-length (- expected-size-length len)
                        size-bytes (concat size-bytes data))
                ;; If there's enough bytes to get the length
                (let* ((raw-length (concat size-bytes
                                           (subseq data 0 expected-size-length)))
                       (trailing-data (subseq data expected-size-length)))
                  (setf expected-request-size (haskell-tags-server--bert-decode-length raw-length)
                        expected-size-length 0
                        size-bytes nil)
                  (unless (= 0 (length trailing-data))
                    (funcall decode trailing-data)))))))))
    decode))

(defvar haskell-tags-server--response-decoder nil
  "State machine that will receive chunked response and reconstruct it into
separate requests.")

(defun haskell-tags-server--network-sentinel (proc event-description)
  (unless (memq (process-status proc) '(run open))
    (message "Lost connection to haskell-tags-server: %s" event-description)
    (haskell-tags-server--disconnect)))

(defun haskell-tags-server--process-filter (proc data)
  (cl-assert (stringp data))
  (funcall haskell-tags-server--response-decoder data))

(defun haskell-tags-server--disconnect ()
  (setf haskell-tags-server--response-decoder nil)
  (let ((proc haskell-tags-server--network-proc))
    (setf haskell-tags-server--network-proc nil)
    (clrhash haskell-tags-server--reported-shallow-dirs)
    (clrhash haskell-tags-server--reported-recursive-dirs)
    (clrhash haskell-tags-server--reported-ignored-globs)
    (delete-process proc)
    (kill-buffer haskell-tags-server--network-buffer)))

(defun haskell-tasg-server--bert-encode-message (message)
  (cl-assert (stringp message))
  (concat (haskell-tags-server--bert-encode-length (length message))
          message))

(defun haskell-tags-server--bert-encode-length (len)
  (cl-assert (numberp len))
  (bindat-pack '((len u32)) `((len . ,len))))

(defun haskell-tags-server--bert-decode-length (encoded-len)
  (cl-assert (stringp encoded-len))
  (cdr (assq 'len
             (bindat-unpack '((len u32)) encoded-len))))


(defun haskell-tags-server--trim-whitespace (str)
  "Trim leading and tailing whitespace from STR."
  (when str
    (cl-assert (stringp str))
    (save-match-data
      (replace-regexp-in-string "\\(?:\\`[ \t\v\f\r\n]*\\|[ \t\v\f\r\n]*\\'\\)" "" str))))

(defparameter haskell-tags-server--haskell-symbol-re
  (rx (or (group (+ ;; (regexp "[-!#$%&*+./<=>?@^|~:\\]")
                  (any ?\- ?\! ?\# ?\$ ?\% ?\& ?\* ?\+ ?\. ?\/ ?\< ?\= ?\> ?\? ?\@ ?^ ?\| ?\~ ?\: ?\\ )))
          (group
           (seq ;; allow _ as a first char to fit GHC
            (or (regexp "\\<[_a-z]")
                ;; allow ' preceding conids because of DataKinds/PolyKinds
                (regexp "'*[A-Z]")
                (syntax word))
            (group
             (* (regexp "\\(?:['a-zA-Z_0-9#]\\|\\sw\\)")))))))
  "Regexp to recognize haskell symbols as generic entities for search
(with e.g. \"*\" in vim).")

(defparameter haskell-tags-server--forward-haskell-symbol-syntax-table
  (let ((tbl (copy-syntax-table haskell-mode-syntax-table)))
    (modify-syntax-entry ?#  "w" tbl)
    (modify-syntax-entry ?_  "w" tbl)
    (modify-syntax-entry ?\' "w" tbl)
    tbl)
  "Special syntax table for haskell that allows to recognize symbols that contain
both unicode and ascii characters.")


(defun haskell-tags-server--forward-haskell-symbol (arg)
  "Like `forward-symbol' but for generic Haskell symbols (either operators,
uppercase or lowercase names)."
  (interactive "p")
  (let (;; (name-chars "a-zA-Z0-9_#'")

        ;; NB constructs like "''Foobar" we'd like to mach "Foobar"
        ;; via `bounds-of-thing-at-point', not the "''Foobar".
        (beginning-quotes "'")
        (operator-chars "\\-!#$%&*+./<=>?@\\^|~:\\\\"))
    (with-syntax-table forward-haskell-symbol-syntax-table
      (if (natnump arg)
          (re-search-forward haskell-tags-server--haskell-symbol-re nil t arg)
        (while (< arg 0)
          (when (re-search-backward haskell-tags-server--haskell-symbol-re nil t)
            (cond ((not (null? (match-beginning 1)))
                   (skip-chars-backward operator-chars)
                   ;; we may have matched # thas ends a name
                   (skip-syntax-backward "w")
                   (skip-chars-forward beginning-quotes))
                  ((not (null? (match-beginning 2)))
                   ;; (goto-char (match-beginning 2))
                   (when (not (null? (match-beginning 3)))
                     (skip-syntax-backward "w")
                     (skip-chars-forward beginning-quotes)))
                  (t
                   (error "No group of haskell-tags-server--haskell-symbol-re matched, should not happen"))))
          (setf arg (1+ arg)))))))

(put 'haskell-tags-server--haskell-symbol
     'forward-op
     #'haskell-tags-server--forward-haskell-symbol)

(defun haskell-tags-server--identifier-at-point ()
  "Grab a Haskell identifier around current point."
  (if (region-active-p)
      (haskell-tags-server--trim-whitespace
       (buffer-substring-no-properties
        (region-beginning)
        (region-end)))
    (let ((bounds (bounds-of-thing-at-point 'haskell-tags-server--haskell-symbol)))
      (cond (bounds
             (buffer-substring-no-properties (car bounds)
                                             (cdr bounds)))
            (t
             (error "No identifier at point found"))))))


(defvar haskell-tags-server--previous-homes nil
  "Previous locations from which symbol search was invoked.")

(defvar haskell-tags-server--selected-loc nil
  "Home entry corresponding to the most recently visited tag.")

(defvar haskell-tags-server--next-homes nil
  "Next locations that were visited but now obscured by going back.")

(defun make-haskell-tags-server-home-entry (buffer point symbol is-regex)
  (cl-assert (bufferp buffer))
  (list buffer point symbol is-regex))

(defun haskell-tags-server-home-entry--buffer (entry)
  (car entry))

(defsetf haskell-tags-server-home-entry--buffer (entry) (val)
  `(setf (car ,entry) ,val))

(defun haskell-tags-server-home-entry--point (entry)
  (cadr entry))

(defun haskell-tags-server-home-entry--search-query (entry)
  (caddr entry))

(defun haskell-tags-server-home-entry--is-regex? (entry)
  (cadddr entry))

(defun haskell-tags-server--home-entry=? (entry-a entry-b)
  (and (eq (haskell-tags-server-home-entry--buffer entry-a)
           (haskell-tags-server-home-entry--buffer entry-b))
       (= (haskell-tags-server-home-entry--point entry-a)
          (haskell-tags-server-home-entry--point entry-b))
       ;; (eq (haskell-tags-server-home-entry--search-query entry-a)
       ;;     (haskell-tags-server-home-entry--search-query entry-b))
       ))

(defun haskell-tags-server--switch-to-home-entry (home-entry)
  (unless (buffer-live-p (haskell-tags-server-home-entry--buffer home-entry))
    (setf (haskell-tags-server-home-entry--buffer home-entry)
          (find-file-noselect
           (buffer-file-name (haskell-tags-server-home-entry--buffer home-entry)))))
  (switch-to-buffer (haskell-tags-server-home-entry--buffer home-entry))
  (goto-char (haskell-tags-server-home-entry--point home-entry)))



(defun haskell-tags-server--goto-loc (filename line prev-loc search-query use-regexp?)
  "Go to a given position in a given file and maintain navigation information."
  (cl-assert (stringp filename))
  (cl-assert (numberp line))
  (cl-assert (stringp search-query))
  (unless (file-exists-p filename)
    (error "File %s does not exist" filename))
  (find-file filename)
  ;; Remove any narrowing because `line' is an absolute line number in
  ;; a file (counted relative to the beginning of the file, not to the
  ;; beginning of the accessible portion of the buffer).
  (save-restriction
    (widen)
    (goto-line line)
    (save-match-data
      (when (re-search-forward (if use-regexp?
                                   (concat "\\<" (regexp-quote search-query) "\\>")
                                 search-query)
                               (line-end-position)
                               t)
        (goto-char (match-beginning 0))))
    ;; remove annoying "Mark set" message
    (message "")

    (push prev-loc haskell-tags-server--previous-homes)
    (setf haskell-tags-server--selected-loc
          (make-haskell-tags-server-home-entry (current-buffer)
                                               (point-marker)
                                               search-query
                                               use-regexp?)
          haskell-tags-server--next-homes nil)))

(defun haskell-tags-server-add-watched-dirs (shallow-dirs recursive-dirs ignored-globs)
  (let ((filtered-shallow-dirs
         (--filter (not (gethash it haskell-tags-server--reported-shallow-dirs))
                   shallow-dirs))
        (filtered-recursive-dirs
         (--filter (not (gethash it haskell-tags-server--reported-recursive-dirs))
                   recursive-dirs))
        (filtered-ignored-globs
         (--filter (not (gethash it haskell-tags-server--reported-ignored-globs))
                   ignored-globs)))
    (cl-assert (-every-p #'stringp filtered-shallow-dirs))
    (cl-assert (-every-p #'stringp filtered-recursive-dirs))
    (cl-assert (-every-p #'stringp filtered-ignored-globs))
    (cl-assert (-every-p #'file-directory-p filtered-shallow-dirs))
    (cl-assert (-every-p #'file-directory-p filtered-recursive-dirs))

    (when (or filtered-shallow-dirs
              filtered-recursive-dirs
              filtered-ignored-globs)
      (haskell-tags-server--call
       haskell-tags-server--network-proc
       'add-shallow-recursive-ignored-entries
       (vector filtered-shallow-dirs
               filtered-recursive-dirs
               filtered-ignored-globs)
       (lambda (response)
         (unless (eq response 'ok)
           (error "Failed to add watched directories %s: %s"
                  (list filtered-shallow-dirs
                        filtered-recursive-dirs
                        filtered-ignored-globs)
                  response))))
      (dolist (dir filtered-shallow-dirs)
        (puthash dir t haskell-tags-server--reported-shallow-dirs))
      (dolist (dir filtered-recursive-dirs)
        (puthash dir t haskell-tags-server--reported-recursive-dirs))
      (dolist (dir filtered-ignored-globs)
        (puthash dir t haskell-tags-server--reported-ignored-globs)))))

(defun haskell-tags-server-go-back ()
  (interactive)
  (if (null haskell-tags-server--previous-homes)
      (error "No more previous go-to-definition entries")
    (progn
      (when (or (null haskell-tags-server--next-homes)
                (and haskell-tags-server--next-homes
                     (not (haskell-tags-server--home-entry=?
                           haskell-tags-server--selected-loc
                           (car haskell-tags-server--next-homes)))))
        (push haskell-tags-server--selected-loc haskell-tags-server--next-homes))
      (let ((prev-home (pop haskell-tags-server--previous-homes)))
        (setf haskell-tags-server--selected-loc prev-home)
        (haskell-tags-server--switch-to-home-entry prev-home)))))

;;;###autoload
(defun setup-haskell-tags-server ()
  (when (current-local-map)
    (define-key (current-local-map) (kbd "M-.") #'haskell-tags-server-goto-definition)
    (define-key (current-local-map) (kbd "M-,") #'haskell-tags-server-go-back)))

;;;###autoload
(defun haskell-tags-server-goto-definition (&optional use-regexp?)
  "Go to the definition of a name at point."
  (interactive "P")
  (let ((filename (buffer-file-name))
        (identifier (if use-regexp?
                        (read-regexp "Enter regexp to search for")
                      (haskell-tags-server--identifier-at-point)))
        (search-func
         (if use-regexp?
             'find-regexp
           'find))
        (next-home-entry
         (car-safe haskell-tags-server--next-homes)))
    (when filename
      (let ((current-home-entry
             (make-haskell-tags-server-home-entry (current-buffer)
                                                  (point-marker)
                                                  nil
                                                  nil)))
        ;; Try to avoid search if there's entry in the next homes that
        ;; matches current query.
        (if (and next-home-entry
                 (let ((next-symbol
                        (haskell-tags-server-home-entry--search-query next-home-entry))
                       (next-symbol-regex?
                        (haskell-tags-server-home-entry--is-regex? next-home-entry)))
                   (if use-regexp?
                       (if next-symbol-regex?
                           nil
                         (string-match-p identifier next-symbol))
                     (if next-symbol-regex?
                         nil
                       (string= identifier next-symbol)))))
            (progn
              (haskell-tags-server--switch-to-home-entry next-home-entry)
              (push current-home-entry
                    haskell-tags-server--previous-homes)
              (setf haskell-tags-server--selected-loc
                    (pop haskell-tags-server--next-homes)))
          (progn
            ;; Connect to tags-server if not already connected.
            (unless haskell-tags-server--network-proc (haskell-tags-server-connect))
            ;; Proceed only if connection was succesful.
            (when haskell-tags-server--network-proc
              (haskell-tags-server--call
               haskell-tags-server--network-proc
               search-func
               (vector filename identifier)
               (lambda (response)
                 ;; (message "Got response: %s" response)
                 (haskell-tags-server--handle-reply response
                                                    current-home-entry
                                                    identifier
                                                    use-regexp?))))))))))


(defmacro haskell-tags-server--for-buffer-with-file (filename &rest body)
  "Execute BODY in buffer with contents of FILENAME. If FILENAME is already
opened in some buffer, then reuse it, and insert its contents in temporary
buffer if no such buffer exists."
  (declare (indent 1))
  (let ((buf-var (gensym))
        (exec-func (gensym)))
    `(let ((,exec-func (lambda () ,@body)))
       (let ((,buf-var (get-file-buffer ,filename)))
         (if ,buf-var
             (with-current-buffer ,buf-var
               (funcall ,exec-func))
           (with-temp-buffer
             (insert-file-contents ,filename
                                   t ;; make current buffer visit inserted file
                                   )
             (funcall ,exec-func)))))))

(defsubst haskell-tags-server--loc-entry--filename (entry)
  (aref entry 0))

(defsubst haskell-tags-server--loc-entry--line (entry)
  (aref entry 1))

(defsubst haskell-tags-server--loc-entry--type (entry)
  (aref entry 2))

(defun haskell-tags-server--handle-reply (reply prev-loc search-query use-regexp?)
  (pcase reply
    (`[loc_known ,loc-entry]
     (haskell-tags-server--goto-loc
      (haskell-tags-server--loc-entry--filename loc-entry)
      (haskell-tags-server--loc-entry--line loc-entry)
      prev-loc
      search-query
      use-regexp?))
    (`[loc_ambiguous ,entries]
     (cl-assert (listp entries))
     (select-mode-start-selection
      entries
      :buffer-name "Symbols"
      :after-init #'ignore
      :on-selection
      (lambda (idx loc-entry _selection-type)
        (select-mode-exit)
        (haskell-tags-server--goto-loc
         (haskell-tags-server--loc-entry--filename loc-entry)
         (haskell-tags-server--loc-entry--line loc-entry)
         prev-loc
         search-query
         use-regexp?))
      :item-show-function
      (lambda (symbol-entry)
        (haskell-tags-server--tag->string search-query
                                          (haskell-tags-server--loc-entry--filename symbol-entry)
                                          (haskell-tags-server--loc-entry--line symbol-entry)
                                          (haskell-tags-server--loc-entry--type symbol-entry)))
      :preamble-function
      (lambda () "Choose symbol\n\n")))
    (`not_found
     (message "No entries for %s %s"
              (if use-regexp? "regexp" "identifier")
              search-query))
    (`error
     (error "haskell-tags-server error: %s" (aref reply 1)))
    (unexpected
     (error "Unexpected reply from haskell-tags-server: %s" unexpected))))


(defun haskell-tags-server--tag->string (name filename line type)
  (let* ((type-str (symbol-name type))
         (is-module? (string= type-str "Module")))
    (concat name
            " ["
            type-str
            "]\n"
            filename
            ":"
            (number-to-string line)
            "\n"
            (if is-module?
                ""
              (concat
               (haskell-tags-server--extract-haskell-tag-signature filename line)
               "\n")))))

(defun haskell-tags-server--extract-haskell-tag-signature (filename line)
  (haskell-tags-server--for-buffer-with-file
   filename
   (save-excursion
     (goto-char (point-min))
     (forward-line (1- line))
     (haskell-tags-server--extract-haskell-block)
     ;; alternative implementation with regexps
     ;; (save-match-data
     ;;   (goto-line1 line)
     ;;   (if (looking-at "^\\([^ \t\n\r\f\v].* ::\\(?: .*\n\\|\n\\)\\(?:^[ \t]+.+\n\\)*\\)")
     ;;     (match-string-no-properties 1)
     ;;     (current-line)))
     )))

(defun haskell-tags-server--extract-haskell-block ()
  "Extract indented Haskell block that starts on the current line."
  (beginning-of-line)
  (let ((start (point)))
    (cl-symbol-macrolet
        ((advance
          (progn
            (forward-line 1)
            (beginning-of-line)
            (skip-chars-forward " \t"))))
      (skip-chars-forward " \t")
      (let ((col (current-column)))
        ;; actualy this is a loop with postcondition
        advance
        (while (< col (current-column))
          advance)
        (let ((previous-line-end (line-end-position 0)))
          (buffer-substring-no-properties start previous-line-end))))))

;; (haskell-tags-server--bert-decode-length (haskell-tags-server--bert-encode-length 50))

;; (coerce (haskell-tags-server--bert-encode-length 50)
;;         'vector)

;; (insert
;;  (format "%s"
;;          (coerce (bert-pack ["foo", "bar"]) 'vector)))
;; [131 104 2 109 0 0 0 3 102 111 111 108 0 0 0 2 100 0 1 44 109 0 0 0 3 98 97 114 106]
;;
;; (defun pack-verbose (obj)
;;   (insert
;;    (format "%s"
;;            (coerce (bert-pack obj) 'vector))))
;;
;; (pack-verbose [1000])
;; [131 104 1 98 0 0 3 232]
;;
;; (process-sentinel haskell-tags-server--network-proc)
;; internal-default-process-sentinel
;;
;; (process-filter haskell-tags-server--network-proc)
;; internal-default-process-filter
;;
;; ;; (defun haskell-tags-server--call (enctod))
;;
;; (network-interface-list)
;;
;; (network-interface-info "wlan0")

;;
;; (haskell-tags-server-add-watched-dirs
;;  nil
;;  '("/home/sergey/projects/haskell/projects/dev-tools/haskell-tags-server/src")
;;  nil)
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/projects/dev-tools/fast-tags/src/FastTags/Tag.hs" "processFile"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server-connect)
;;
;; (haskell-tags-server--disconnect)
;;
;; (process-status haskell-tags-server--network-proc)
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["foo" "bar"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["baz" "bar"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "Const"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "Applicative"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "WrappedArrow"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "Functor"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "liftA3"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))
;;
;; (haskell-tags-server--call
;;  haskell-tags-server--network-proc
;;  'find
;;  '["/home/sergey/projects/haskell/packages/all-packages/base-4.11.1.0/Control/Applicative.hs" "ZipList"]
;;  (lambda (response)
;;    (message "Got response: %s" response)))

(provide 'haskell-tags-server)

;; Local Variables:
;; End:

;; haskell-tags-server.el ends here
