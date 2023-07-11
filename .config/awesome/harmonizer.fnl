;;;; harmonizer.fnl

(comment
  Here the awesomewm files start.
  Very messy and all but ehh don't mind it much.)

;; Standard awesomewm libs.
(local gears (require "gears"))
(local beautiful (require "beautiful"))

;; Theming.
(local theme-dir (.. (os.getenv "HOME") "./config/awesome/themes"))
(local theme-name "ildaite")
(local theme (require (.. "themes." theme-name ".theme")))
(beautiful.init theme)

;; Signals.
(local startup (require "signals.startup"))
(local tags (require "signals.tags"))
(local notifications (require "signals.notifications"))
;;(local titlebars (require "signals.titlebars"))
(local rules (require "signals.rules"))

;; Mouse/Keys bindings.
(local binds (require "binds"))

;; Widgets
(local bar (require "widgets.bar"))
