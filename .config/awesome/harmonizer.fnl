;;;; harmonizer.fnl

(local gears (require "gears"))
(local beautiful (require "beautiful"))
(local startup (require "fnl.prelude.startup"))
(local theme-dir (.. (os.getenv "HOME")
                     "./config/awesome/themes"))
(local theme-name "oxocarbon")
(local theme (require (.. "themes." theme-name ".theme"))) (beautiful.init theme)
(local binds (require "binds"))
(local rules (require "rules")) 
