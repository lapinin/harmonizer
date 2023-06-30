;;;; startup.fnl

(local naughty (require "naughty"))
(local startup {})

;; Error handling.
(naughty.connect_signal
 "request::display_error"
 (fn [message startup]
   "Checks when awesome finds an error at startup (fallback config)"
   (naughty.notification
    {: message
     :urgency "critical"
     :title (.. "oops, an error happened"
                (or (and startup
                         " during startup!")
                    "!"))})))

startup
