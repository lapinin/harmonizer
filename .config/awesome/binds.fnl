;;;; binds.fnl

(import-macros {: btn
                : kb!
                : kbc!} :macros)

(local awful (require "awful"))
(local beautiful (require "beautiful"))
(local menubar (require "menubar"))
(local wibox (require "wibox"))

(var modkey "Mod4")
(var modkeyalt "Mod1")
(var terminal "tym")
(var editor (or (os.getenv "EDITOR") "vi"))
(var editor_cmd (.. terminal " -e " editor))

;; Mouse bindings.
(awful.mouse.append_global_mousebindings
  [ (btn 4 awful.tag.viewprev)
    (btn 5 awful.tag.viewnext) ])

;; General Awesome keys.
(kb!
 [ {:mods [modkey "Control"] :key "r"
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
    :action awful.tag.viewprevG
    :description "view previous" :group "tag"}
    {:mods [modkey] :key "Right"
     :action awful.tag.viewnext
     :description "view next" :group "tag"}
    {:mods [modkey] :key "Escape"
     :action awful.tag.history.restore
     :description "go back" :group "tag"} ])
	
;; Focus related keybindings.
(kb!
 [ {:mods [modkey] :key "Tab"
    :action (fn [] (awful.client.focus.history.previous)
             (when client.focus (client.focus:raise)))
    :description "go back" :group "client"}
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

 (client.connect_signal "request::default_keybindings"
  (fn []
    (kbc!
      [ {:mods [modkey] :key "f"
         :action (fn [c] (set c.fullscreen (not c.fullscreen)) (c:raise))
         :description "toggle fullscreen" :group "client"}
        {:mods [modkey "Shift"] :key "c"
         :action (fn [c] (c:kill))
         :description "close" :group "client"}
        {:mods [modkey "Control"] :key "space"
         :action awful.client.floating.toggle
         :description "toggle floating" :group "client"}
        {:mods [modkey "Control"] :key "Return"
         :action (fn [c] (c:swap (awful.client.getmaster)))
         :description "move to master" :group "client"}
        {:mods [modkey] :key "o"
         :action (fn [c] (c:move_to_screen))
         :description "move to screen" :group "client"}
        {:mods [modkey] :key "t"
         :action (fn [c] (set c.ontop (not c.ontop)))
         :description "toggle keep on top" :group "client"}
        {:mods [modkey] :key "n"
         ;; The client currently has the input focus, so it cannot be
         ;; minimized, since minimized clients can't have the focus.
         :action (fn [c] (set c.minimized true))
         :description :minimize :group "client"}
        {:mods [modkey] :key "m"
         :action (fn [c] (set c.maximized (not c.maximized)) (c:raise))
         :description "(un)maximize" :group "client"}
        {:mods [modkey "Control"] :key "m"
         :action (fn [c] (set c.maximized_vertical (not c.maximized_vertical))
                  (c:raise))
         :description "(un)maximize vertically" :group "client"}
        {:mods [modkey "Shift"] :key "m"
         :action (fn [c] (set c.maximized_horizontal (not c.maximized_horizontal))
                  (c:raise))
         :description "(un)maximize horizontally" :group "client"} ])))

