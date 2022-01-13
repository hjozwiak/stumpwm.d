(in-package :stumpwm)
;;; {Variables:

;;; Avoid focus getting stolen due to activity.

(setq *deny-raise-request* t)

(defvar *emacspeak-dir*
  "/home/sektor/emacspeak"
  "Root directory of Emacspeak installation.")

(defvar *tts-window-icon*
  (concatenate 'string
               *emacspeak-dir*
               "/"
               "sounds/pan-chimes/window-resize.wav")
  "Window change icon.")

(defvar *tts-off-icon*
  (concatenate 'string
               *emacspeak-dir*
               "/"
               "sounds/pan-chimes/off.wav")
  "Off icon.")

(defvar *tts-on-icon*
  (concatenate 'string
               *emacspeak-dir*
               "/"
               "sounds/pan-chimes/on.wav")
  "On icon.")

;;; }
;;; {Prefix key matches my screen setup:

(set-prefix-key (kbd "C-t"))

;;; }
;;; {TTS
(load "/home/sektor/.stumpwm.d/tts.lisp")
(setq *tts-engine* *tts-espeak*)
(tts-say "TTS: Ready to talk! ")

;;; }
;;; {Speak Actions:


(defun speak-window-change (new old)
  "Speak current window  information."
  (declare (special *tts-window-icon*))
  (when new
    (tts-serve-icon *tts-window-icon*)
    (tts-speak (format nil "~a Window" (window-name new)))))

(defun speak-this-window (window)
  "Speak this window  information."
  (tts-speak (window-name window)))

(defun speak-current-window ()
  "Speak current window  information."
  (tts-speak (window-name (current-window))))

(defvar *tts-speak-messages* nil
  "Switch messages on and off.
Off by default to avoid a stumpwm crash on startup.
Use C-\ t to turn it on.")

(defcommand  tts-toggle-speak-messages ()
  ()
  "Toggle state of speak messages switch."
  (declare (special *tts-speak-messages* *tts-on-icon*
                    *tts-off-icon*))
  (setq *tts-speak-messages* (not *tts-speak-messages*))
  (tts-serve-icon
   (if *tts-speak-messages*
       *tts-on-icon*
       *tts-off-icon*)))

(define-key *root-map* (kbd "T") "tts-toggle-speak-messages")

(defun speak-messages (&rest messages)
  "Speak messages, a list of lines."
  (declare (special *tts-speak-messages*))
  (when *tts-speak-messages*
    (tts-speak-list (mapcar #'stumpwm::uncolorify messages))))

;;; }
;;; {Attach Handlers:

(setq  *focus-window-hook* (list 'speak-window-change))
(setq *new-window-hook* (list 'speak-this-window))
                        (setq *message-hook* (list 'speak-messages))

;;; }
;;; {Chrome:

(defcommand mumble ()
  ()
  "Start  or switch to Chrome."
  (run-or-raise "/usr/bin/mumble" '(:class "mumble" :title "Mumble")))

(define-key *root-map* (kbd "m") "mumble")
(defcommand restart-orca ()
  ()
  "Restarts Orca when it hangs or has an update."
  (run-shell-command "orca --replace"))
(define-key *root-map* (kbd "O") "restart-orca")
(defcommand terminal ()
  ()
  "Opens up a terminal."
  (run-or-raise "/usr/bin/lxterminal" '(:class "lxterminal" :title "Lxterminal")))
(define-key *root-map* (kbd "t") "terminal")
(sb-ext:run-program
        "/usr/bin/play"
        (list
         (concatenate
          'string
          *emacspeak-dir*
          "/sounds/prompts/launch-wm.mp3")))


;; Launching various other applications, namely Orca
(run-commands
"restart-orca")
;;; }
;;; {Emacs local variables

;;; local variables:
;;; mode: folding
;;; folded-file: t
;;; end:

;;; }

