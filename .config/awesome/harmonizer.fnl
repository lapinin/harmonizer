;;;; harmonizer.fnl

;; Standard awesome library.
(local gears (require "gears"))
(local awful (require "awful"))
(require "awful.autofocus")

;; Widget and layout library.
(local wibox (require "wibox"))

;; Theme handling library.
(local beautiful (require "beautiful"))

;; Notification library.
(local naughty (require "naughty"))

;; Declarative object management.
(local ruled (require "ruled"))
(local menubar (require "menubar"))
(local hotkeys_popup (require "awful.hotkeys_popup"))

;; Enable hotkeys help widget for vim and other apps
;; when client with a matching name is opened:
(require "awful.hotkeys_popup.keys")

;; Error handling.
;; Check if awesome encountered an error during startup and fell back to
;; another config (this code will only ever execute for the fallback config.)

(fn display_errors [message startup]
  (naughty.notification
    {: message
     :urgency "critical"
     :title (.. "oops, an error happened"
              (or (and startup
                " during startup!")
               "!"))}))

(naughty.connect_signal "request::display_error" display_errors)

;; Theming options.
(local theme-dir (.. (os.getenv "HOME") "./config/awesome/themes"))
(local theme-name "fea")
(local theme (require (.. "themes." theme-name ".theme")))
(local wallpaper (require "fnl.wallpaper"))

;; Set theme.
(beautiful.init theme) 
(screen.connect_signal "request::wallpaper" wallpaper.gradient)	

(local binds (require "binds"))
(local rules (require "rules"))
