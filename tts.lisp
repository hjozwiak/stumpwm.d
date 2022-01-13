(in-package :stumpwm)
;;{{{  Settings

(defvar *emacspeak* "/home/sektor/emacspeak/"
  "Root of Emacspeak installation.")

(defvar *tts-process* nil
  "Handle to tts server connection.")

(defvar *tts-dtk*
  (concatenate 'string   *emacspeak* "servers/dtk-exp")
  "DTK tcl server")
(defvar *tts-espeak*
  (concatenate 'string *emacspeak* "/servers/espeak")
  "The Espeak location.")

(defvar *tts-outloud*
  (concatenate 'string   *emacspeak* "servers/outloud")
  "Outloud tcl server")

(defvar *tts-32-outloud*
  (concatenate 'string   *emacspeak* "servers/32-outloud")
  "Outloud tcl server")

(defvar *tts-engine* *tts-dtk*
  "Default TTS  engine. User settable.")

;;}}} 
;;{{{ Internal  Functions

(defun tts-open ()
  "Open a TTS session."
  (setq *tts-process*
        (sb-ext:run-program
         *tts-engine* nil :wait nil  :input :stream))
  (let ((i (sb-ext:process-input *tts-process*)))
    (write-line (format nil "tts_set_punctuations all") i)
    (force-output i)))

(defun tts-close ()
  "Close a TTS session."
  (when(and  (sb-ext:process-p *tts-process*)
             (sb-ext:process-alive-p *tts-process*))
    (sb-ext:process-close *tts-process*))
  (setq *tts-process* nil))

(defun tts-running-p ()
  "Is there a tts process up and running?"
  (and *tts-process*
       (sb-ext:process-p *tts-process*)
       (sb-ext:process-alive-p *tts-process*)))

(defvar *tts-stop-immediately* t
  "Stop speech immediately.")
(defun tts-queue (text)
  "Queue text to speak."
  (unless (and  *tts-process*
                (sb-ext:process-alive-p *tts-process*))
    (tts-open))
  (let ((i (sb-ext:process-input *tts-process*)))
    (write-line (format nil "q {~a}" text) i)
    (force-output i)))

(defun tts-force ()
  "Speak all queued text."
  (let ((i (sb-ext:process-input *tts-process*)))
    (write-line "d" i)
    (force-output i)))

;;}}} 
;;{{{ Exported Functions
(defun tts-stop ()
  "Stop speech."
  (let ((i (sb-ext:process-input *tts-process*)))
      (write-line "s"  i)
      (force-output i)))

(defun tts-speak (text)
  "Say some text."
  (unless (and  *tts-process*
                (sb-ext:process-alive-p *tts-process*))
    (tts-open))
  (let ((i (sb-ext:process-input *tts-process*)))
    (when *tts-stop-immediately*
      (write-line "s"  i)
      (force-output i))
    (write-line (format nil "q {~a}\;d" text) i)
    (force-output i)))

(defun tts-serve-icon (filename)
  "Serve auditory icon  in filename."
  (unless (and  *tts-process*
                (sb-ext:process-alive-p *tts-process*))
    (tts-open))
  (let ((i (sb-ext:process-input *tts-process*)))
    (write-line (format nil "a ~a\;d" filename) i)
    (force-output i)))

(defun tts-say (text)
  "Say some text."
  (unless (and  *tts-process*
                (sb-ext:process-alive-p *tts-process*))
    (tts-open))
  (let ((i (sb-ext:process-input *tts-process*)))
    (when *tts-stop-immediately*
      (write-line "s"  i)
      (force-output i))
    (write-line (format nil "tts_say  ~a" text) i)
    (force-output i)))

(defun tts-speak-list (lines)
  "Speak an arbitrary number of lines."
  (tts-stop)
  (mapc 'tts-queue lines)
  (tts-force))

(defun tts-letter (text)
  "Speak letter."
  (unless (and  *tts-process*
                (sb-ext:process-alive-p *tts-process*))
    (tts-open))
  (let ((i (sb-ext:process-input *tts-process*)))
    (write-line (format nil "l ~a" text) i)
    (force-output i)))

;;}}} 
(provide 'tts)

;;{{{  end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}} 
