(in-package :stumpwm)
(ql:quickload :slynk)
(slynk:create-server :dont-close t :port 4001)
(load-module "swm-emacs")
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


(set-prefix-key (kbd "C-t"))

(load "/home/sektor/.stumpwm.d/tts.lisp")
(setq *tts-engine* *tts-espeak*)



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

;;; {Attach Handlers:

(setq  *focus-window-hook* (list 'speak-window-change))
(setq *new-window-hook* (list 'speak-this-window))
                        (setq *message-hook* (list 'speak-messages))

;;; {Chrome:

(defcommand mumble ()
  ()
  "Start  or switch to Mumble."
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
(defcommand discord ()
    ()
  "Open Discord."
  (run-or-raise "/usr/bin/discord --force-renderer-accessibility --enable-caret-browsing" '(:class "discord" :title "Discord")))
(define-key *root-map* (kbd "d") "discord")
(defcommand spotify ()
    ()
  "Opens up Spotify, naturally."
  (run-or-raise "/usr/bin/spotify --force-renderer-accessibility --enable-caret-browsing" '(:class "spotify" :title "Spotify")))
(define-key *root-map* (kbd "s") "spotify")

(defcommand brave ()
    ()
  "Open the Brave browser"
  (run-or-raise "/usr/bin/brave --force-renderer-accessibility --enable-caret-browsing" '(:class "brave" :title "Brave")))
(define-key *root-map* (kbd "b") "brave")
(define-key *root-map* (kbd "e") "swm-emacs")
(run-commands
 "restart-orca"
 "discord"
 "brave"
 "spotify"
 "emacs-daemon-start")
