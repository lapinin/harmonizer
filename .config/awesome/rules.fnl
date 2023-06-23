;;;; rules.fnl

(import-macros {: btn} :fnl.prelude.macros)

(local awful (require "awful"))
(local wibox (require "wibox"))
(local naughty (require "naughty"))
(local ruled (require "ruled"))

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
                                      :rule_any {:type [:normal :dialog]}})))   ;; Add titlebars to normal clients and dialogs. 

;; Titlebars

;; Add a titlebar if titlebars_enabled is set to true in the rules.
(client.connect_signal "request::titlebars"
  (fn [c]
    (let [buttons [ (btn 1 (fn [] (c:activate {:action "mouse_move" :context "titlebar"})))
                    (btn 3 (fn [] (c:activate {:action "mouse_resize" :context "titlebar"})))] ] ;; Buttons for the titlebar.
      (tset (awful.titlebar c) :widget
       {1 {1 {1 {:halign "center" ;; Title.
                 :widget (awful.titlebar.widget.titlewidget c)}
              : buttons
              :layout wibox.layout.flex.horizontal}
           2 { 1 (awful.titlebar.widget.floatingbutton c)    ;; Right.
               2 (awful.titlebar.widget.maximizedbutton c)
              :layout (wibox.layout.fixed.horizontal)}
        :layout wibox.layout.align.horizontal}}))))

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
