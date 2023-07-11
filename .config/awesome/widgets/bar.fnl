;;;; bar.fnl

(import-macros {: btn} :macros)

(local awful (require "awful"))
(local wibox (require "wibox"))

(var modkey "Mod4")

;; Wibar.
(screen.connect_signal "request::desktop_decoration"
  (fn [screen]
    (awful.tag [ "1" "2" "3" "4"] screen
      (. awful.layout.layouts 1))
  (set screen.mypromptbox (awful.widget.prompt)) ;; Create a promptbox for each screen.
  ;; Create a taglist widget.
  (set screen.mytaglist
       (awful.widget.taglist
         {:screen screen
          :filter awful.widget.taglist.filter.all
          :buttons [ (btn 1 (fn [t] (t:view_only)))
                     (btn [modkey] 1 (fn [t]
                       (when client.focus (client.focus:move_to_tag t))))
                     (btn 3 awful.tag.viewtoggle)
                     (btn [modkey] 3 (fn [t]
                       (when client.focus (client.focus:toggle_tag t))))
                     (btn 4 (fn [t] (awful.tag.viewprev t.screen)))
                     (btn 5 (fn [t] (awful.tag.viewnext t.screen))) ]}))
   ;; Create the wibox.
   (set screen.mywibox
        (awful.wibar
          {:position "bottom"
           :height 24
           :width 72
           :screen screen
           :widget {:layout wibox.layout.align.horizontal
                    1 {:layout wibox.layout.fixed.horizontal
                       1 screen.mytaglist
                       2 screen.mypromptbox}}}))))

;; Mouse bindings.
(awful.mouse.append_global_mousebindings
  [ (btn 4 awful.tag.viewprev)
    (btn 5 awful.tag.viewnext) ])
