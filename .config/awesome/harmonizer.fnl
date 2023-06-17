;;;; harmonizer.fnl

(import-macros {: btn
                : kb!} :macros)

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

;; Variable definitions.
;; Themes define colours, icons, font and wallpapers.
(beautiful.init (.. (gears.filesystem.get_themes_dir) "default/theme.lua"))

;; This is used later as the default terminal and editor to run.
(var terminal "emacsclient -c -e '(vterm)'")
(var editor (or (os.getenv "EDITOR") "vi"))
(var editor_cmd (.. terminal " -e " editor))

;; Default modkey.
;; Usually, Mod4 is the key with a logo between Control and Alt.
;; If you do not like this or do not have such a key,
;; I suggest you to remap Mod4 to another key using xmodmap or other tools.
;; However, you can use another modifier like Mod1, but it may interact with others.
(var modkey "Mod4")

;; Tag layout.
;; Table of layouts to cover with awful.layout.inc, order matters.
(fn default_layouts
  []
  (awful.layout.append_default_layouts [ awful.layout.suit.floating
                                         awful.layout.suit.tile
                                         awful.layout.suit.tile.left
                                         awful.layout.suit.tile.bottom
                                         awful.layout.suit.tile.top
                                         awful.layout.suit.fair
                                         awful.layout.suit.fair.horizontal
                                         awful.layout.suit.spiral
                                         awful.layout.suit.spiral.dwindle
                                         awful.layout.suit.max
                                         awful.layout.suit.max.fullscreen
                                         awful.layout.suit.magnifier
                                         awful.layout.suit.corner.nw ]))

(tag.connect_signal "request::default_layouts" default_layouts)

;; Menu.
;; Create a launcher widget and a main menu.
(global myawesomemenu [[ "hotkeys" (fn [] (hotkeys_popup.show_help nil (awful.screen.focused))) ]
                       [ "manual" (.. terminal " -e man awesome") ]
                       [ "restart" awesome.restart ]
                       [ "quit" (fn [] (awesome.quit)) ]])

(global mymainmenu (awful.menu {:items [[ "awesome" myawesomemenu beautiful.awesome_icon ]]}))

(global mylauncher (awful.widget.launcher {:image beautiful.awesome_icon
                                           :menu mymainmenu }))

;; Menubar configuration.
(set menubar.utils.terminal terminal) ;; Set the terminal for applications that require it.


;; Wallpaper.
(var tiledwall
  (string.format "%s/.config/awesome/assets/tile.png"
                 (os.getenv "HOME")))

(fn set_wallpaper
  [screen]
  (let [wall_options {:screen screen
                      :widget {:image tiledwall
                               :upscale true
                               :downscale true
                               :widget wibox.widget.imagebox}
                      :valign "center"
                      :halign "center"
                      :tiled true
                      :widget wibox.container.tile}]
    (awful.wallpaper wall_options))) 

(screen.connect_signal "request::wallpaper" set_wallpaper)

;; Wibar.

;; Create a textclock widget.
(global mytextclock (wibox.widget.textclock))

(screen.connect_signal "request::desktop_decoration"
  (fn [screen]
    (awful.tag [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ] screen
      (. awful.layout.layouts 1))
  (set screen.mypromptbox (awful.widget.prompt)) ;; Create a promptbox for each screen.
  ;; Create an imagebox widget which will contain an icon indicating which layout we're using.
  ;; We need one layoutbox per screen.
  (set screen.mylayoutbox
       (awful.widget.layoutbox
         {:screen screen
          :buttons [ (btn 1 (fn [] (awful.layout.inc  1)))
                     (btn 3 (fn [] (awful.layout.inc -1)))
                     (btn 4 (fn [] (awful.layout.inc -1)))
                     (btn 5 (fn [] (awful.layout.inc  1))) ]}))
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
   ;; Create a tasklist widget.
   (set screen.mytasklist
        (awful.widget.tasklist
          {:screen screen
           :filter awful.widget.tasklist.filter.currenttags
           :buttons [ (btn 1 (fn [c] (c:activate {:action "toggle_minimization"
                                                  :context "tasklist"})))
                      (btn 3 (fn [] (awful.menu.client_list {:theme {:width 250}})))
                      (btn 4 (fn [] (awful.client.focus.byidx -1)))
                      (btn 5 (fn [] (awful.client.focus.byidx 1))) ]}))
   ;; Create the wibox.
   (set screen.mywibox
        (awful.wibar
          {:position "bottom"
           :screen screen
           :widget {:layout wibox.layout.align.horizontal
                    1 {:layout wibox.layout.fixed.horizontal
                       1 mylauncher
                       2 screen.mytaglist
                       3 screen.mypromptbox}
                    2 screen.mytasklist
                    3 {:layout wibox.layout.fixed.horizontal
                       1 (wibox.widget.systray)
                       2 mytextclock
                       3 screen.mylayoutbox}}}))))

;; Mouse bindings.
(awful.mouse.append_global_mousebindings
  [ (btn 3 (fn [] (mymainmenu:toggle)))
    (btn 4 awful.tag.viewprev)
    (btn 5 awful.tag.viewnext) ])

;; Key bindings

;; General Awesome keys.
(kb!
 [ {:mods [modkey] :key "s"
    :action hotkeys_popup.show_help
    :description "show help" :group "awesome"}
   {:mods [modkey] :key "w"
    :action (fn [] (mymainmenu:show))
    :description "show main menu" :group "awesome"}
   {:mods [modkey "Control"] :key "r"
    :action awesome.restart
    :description "reload awesome" :group "awesome"}
   {:mods [modkey "Shift"] :key "q"
    :action awesome.quit
    :description "quit awesome" :group "awesome"}
   {:mods [modkey] :key :Return
    :action (fn [] (awful.spawn terminal))
    :description "open a terminal" :group "launcher"}
   {:mods [modkey] :key "r"
    :action (fn [] (: (. (awful.screen.focused) :mypromptbox) :run))
    :description "run prompt" :group "launcher"}
   {:mods [modkey] :key "p"
    :action (fn [] (menubar.show))
    :description "show the menubar" :group "launcher"} ])

;; Tags related keybindings.
(kb!
 [ {:mods [modkey] :key "Left"
    :action awful.tag.viewprev
    :description "view previous" :group "tag"}
    {:mods [modkey] :key "Right"
     :action awful.tag.viewnext
     :description "view next" :group "tag"}
    {:mods [modkey] :key "Escape"
     :action awful.tag.history.restore
     :description "go back" :group "tag"} ])	

;; Focus related keybindings.
(kb!
 [ {:mods [modkey] :key "j"
    :action (fn [] (awful.client.focus.byidx 1))
    :description "focus next by index" :group "client"}
   {:mods [modkey] :key "k"
    :action (fn [] (awful.client.focus.byidx -1))
    :description "focus previous by index" :group "client"}
  {:mods [modkey] :key "Tab"
   :action (fn [] (awful.client.focus.history.previous)
             (when client.focus (client.focus:raise)))
   :description "go back" :group "client"}
  {:mods [modkey "Control"] :key "j"
   :action (fn [] (awful.screen.focus_relative 1))
   :description "focus the next screen" :group "screen"}
  {:mods [modkey "Control"] :key "k"
   :action (fn [] (awful.screen.focus_relative -1))
   :description "focus the previous screen" :group "screen"}
  {:mods [modkey "Control"] :key "n"
   :action (fn [] (let [c (awful.client.restore)] ;; Focus restored client.
             (when c
               (c:activate {:context :key.unminimize
                            :raise true}))))
   :description "restore minimized" :group "client"} ])

;; Layout related keybindings.
(kb!
 [ {:mods [modkey :Shift] :key "j"
    :action (fn [] (awful.client.swap.byidx 1))
    :description "swap with next client by index" :group "client"}
   {:mods [modkey :Shift] :key "k"
    :action (fn [] (awful.client.swap.byidx -1))
    :description "swap with previous client by index" :group "client"}
   {:mods [modkey] :key "u"
    :action awful.client.urgent.jumpto
    :description "jump to urgent client" :group "client"}
   {:mods [modkey] :key "l"
    :action (fn [] (awful.tag.incmwfact 0.05))
    :description "increase master width factor" :group "layout"}
   {:mods [modkey] :key "h"
    :action (fn [] (awful.tag.incmwfact (- 0.05)))
    :description "decrease master width factor" :group "layout"}
   {:mods [modkey :Shift] :key "h"
    :action (fn [] (awful.tag.incnmaster 1 nil true))
    :description "increase the number of master clients" :group "layout"}
   {:mods [modkey :Shift] :key "l"
    :action (fn [] (awful.tag.incnmaster -1 nil true))
    :description "decrease the number of master clients" :group "layout"}
   {:mods [modkey :Control] :key "h"
    :action (fn [] (awful.tag.incncol 1 nil true))
    :description "increase the number of columns" :group "layout"}
   {:mods [modkey :Control] :key "l"
    :action (fn [] (awful.tag.incncol -1 nil true))
    :description "decrease the number of columns" :group "layout"}
   {:mods [modkey] :key "space"
    :action (fn [] (awful.layout.inc 1))
    :description "select next" :group "layout"}
   {:mods [modkey :Shift] :key "space"
    :action (fn [] (awful.layout.inc -1))
    :description "select previous" :group "layout"} ]) 

(kb!
 [ {:mods [modkey] :keygroup :numrow
    :action (fn [idx]
              (let [screen (awful.screen.focused)
                    tag (. screen.tags idx)]
                (when tag
                  (tag:view_only))))
    :description "only view tag" :group "tag"}
    {:mods [modkey :Control] :keygroup :numrow
     :action (fn [idx]
               (let [screen (awful.screen.focused)
                     tag (. screen.tags idx)]
                 (when tag
                   (awful.tag.viewtoggle tag))))
     :description "toggle tag" :group "tag"}
    {:mods [modkey :Shift] :keygroup :numrow
     :action (fn [idx]
               (when client.focus
                 (local tag (. client.focus.screen.tags idx))
                 (when tag
                   (client.focus:move_to_tag tag))))
     :description "move focused client to tag" :group "tag"}
    {:mods [modkey :Control :Shift] :keygroup :numrow
     :action (fn [idx]
               (when client.focus
                 (local tag (. client.focus.screen.tags idx))
                 (when tag
                   (client.focus:toggle_tag tag))))
     :description "toggle focus client on tag" :group "tag"}
    {:mods [modkey] :keygroup :numpad
     :action (fn [idx]
               (local tag (. (awful.screen.focused) :selected_tag))
               (when tag
                 (set tag.layout (or (. tag.layouts idx) tag.layout))))
     :description "select layout directly" :group "layout"} ])

(client.connect_signal "request::default_mousebindings"
  (fn []
    (awful.mouse.append_client_mousebindings
       [ (btn 1 (fn [c] (c:activate {:context :mouse_click})))
         (btn [modkey] 1 (fn [c] (c:activate {:action :mouse_move :context :mouse_click})))
         (btn [modkey] 3 (fn [c] (c:activate {:action :mouse_resize :context :mouse_click}))) ])))

;; TODO: Think about better keybindings.
;; (client.connect_signal "request::default_keybindings"
;;   (fn []
;;     (kb!
;;       [ {:mods [modkey] :key "f"
;;          :action (fn [c] (set c.fullscreen (not c.fullscreen)) (c:raise))
;;          :description "toggle fullscreen" :group "client"}
;;         {:mods [modkey "Shift"] :key "c"
;;          :action (fn [c] (c:kill))
;;          :description "close" :group "client"}
;;         {:mods [modkey "Control"] :key "space"
;;          :action awful.client.floating.toggle
;;          :description "toggle floating" :group "client"}
;;         {:mods [modkey "Control"] :key "Return"
;;          :action (fn [c] (c:swap (awful.client.getmaster)))
;;          :description "move to master" :group "client"}
;;         {:mods [modkey] :key "o"
;;          :action (fn [c] (c:move_to_screen))
;;          :description "move to screen" :group "client"}
;;         {:mods [modkey] :key "t"
;;          :action (fn [c] (set c.ontop (not c.ontop)))
;;          :description "toggle keep on top" :group "client"}
;;         {:mods [modkey] :key "n"
;;          ;; The client currently has the input focus, so it cannot be
;;          ;; minimized, since minimized clients can't have the focus.
;;          :action (fn [c] (set c.minimized true))
;;          :description :minimize :group "client"}
;;         {:mods [modkey] :key "m"
;;          :action (fn [c] (set c.maximized (not c.maximized)) (c:raise))
;;          :description "(un)maximize" :group "client"}
;;         {:mods [modkey "Control"] :key "m"
;;          :action (fn [c] (set c.maximized_vertical (not c.maximized_vertical))
;;                   (c:raise))
;;          :description "(un)maximize vertically" :group "client"}
;;         {:mods [modkey "Shift"] :key "m"
;;          :action (fn [c] (set c.maximized_horizontal (not c.maximized_horizontal))
;;                   (c:raise))
;;          :description "(un)maximize horizontally" :group "client"} ]))) 

;; Rules.

;; Rules to apply to new clients.
(ruled.client.connect_signal "request::rules"
  (fn []
    (ruled.client.append_rule
      {:id "global"
       :rule {}
       :properties {:raise true
                    :focus awful.client.focus.filter
                    :screen awful.screen.preferred
                    :placement (+ awful.placement.no_overlap
                                  awful.placement.no_offscreen)}}) ;; All clients will match this rule.
    (ruled.client.append_rule
      {:id :floating
           :properties {:floating true}
           :rule_any {:class [ :Arandr
                               :Blueman-manager ]
                      :instance [:copyq :pinentry]
                      :name ["Event Tester"]
                      :role [ :AlarmWindow
                              :ConfigManager
                              :pop-up ]}})  ;; Floating clients.
     (ruled.client.append_rule {:id :titlebars
                                      :properties {:titlebars_enabled true}
                                      :rule_any {:type [:normal :dialog]}})))  ;; Add titlebars to normal clients and dialogs. 

;; Titlebars

;; Add a titlebar if titlebars_enabled is set to true in the rules.
(client.connect_signal "request::titlebars"
  (fn [c]
    (let [buttons [ (btn 1 (fn [] (c:activate {:action "mouse_move" :context "titlebar"})))
                    (btn 3 (fn [] (c:activate {:action "mouse_resize" :context "titlebar"})))] ] ;; Buttons for the titlebar.
      (tset (awful.titlebar c) :widget
       {1 {1 (awful.titlebar.widget.iconwidget c)     ;; Left.
              : buttons
              :layout wibox.layout.fixed.horizontal}
           2 {1 {:halign "center" ;; Title.
                 :widget (awful.titlebar.widget.titlewidget c)}
              : buttons
              :layout wibox.layout.flex.horizontal}
           3 { 1 (awful.titlebar.widget.floatingbutton c)    ;; Right.
               2 (awful.titlebar.widget.maximizedbutton c)
              :layout (wibox.layout.fixed.horizontal)}
        :layout wibox.layout.align.horizontal}))))	

;; Notifications.
(ruled.notification.connect_signal "request::rules"
  (fn [] (ruled.notification.append_rule
    {:properties {:rule {}
                  :screen awful.screen.preferred
                  :implicit_timeout 5}})))  ;; All notifications will match this rule.

(naughty.connect_signal "request::display"
  (fn [n] (naughty.layout.box {:notification n})))	

;; Enable sloppy focus, so that focus follows mouse.
(client.connect_signal "mouse::enter"
  (fn [c] (c:activate {:context "mouse_enter" :raise false})))
