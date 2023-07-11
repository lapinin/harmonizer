;;; titlebars.fnl

(import-macros {: btn } :macros)

(local awful (require "awful"))
(local beautiful (require "beautiful"))
(local wibox (require "wibox"))

;; Titlebars
;; Add a titlebar if titlebars_enabled is set to true in the rules.

(fn titlebar [c] ;; Buttons for the titlebar.
  (let [buttons [ (btn 1 (fn [] (c:activate {:action "mouse_move" :context "titlebar"})))
                  (btn 3 (fn [] (c:activate {:action "mouse_resize" :context "titlebar"})))] ]
    (tset (awful.titlebar c)
          :widget {1 {1 {: buttons
                         :layout wibox.layout.flex.horizontal}
                      2 { 1 (awful.titlebar.widget.floatingbutton c)    ;; Right.
                          2 (awful.titlebar.widget.maximizedbutton c)
                          :layout (wibox.layout.fixed.horizontal)}
                      :layout wibox.layout.align.horizontal}})))

(client.connect_signal "request::titlebars" titlebar)
