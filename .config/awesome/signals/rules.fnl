;;;; rules.fnl

(import-macros {: btn } :macros)

(local awful (require "awful"))
(local wibox (require "wibox"))
(local ruled (require "ruled"))

;; Rules.
;; Rules to apply to new clients.
(ruled.client.connect_signal "request::rules"
  (fn []
    (ruled.client.append_rule
      {:id "global" ;; All clients will match this rule.
       :rule {}
       :properties {:raise true
                    :focus awful.client.focus.filter
                    :screen awful.screen.preferred
                    :placement (+ awful.placement.no_overlap
                                  awful.placement.no_offscreen)}})    
    (ruled.client.append_rule  
      {:id :floating  ;; Floating clients.
           :properties {:floating true}
           :rule_any {:class [ :Arandr
                               :Blueman-manager ]
                      :instance [:copyq :pinentry]
                      :name ["Event Tester"]
                      :role [ :AlarmWindow
                              :ConfigManager
                              :pop-up ]}}) 
     (ruled.client.append_rule {:id :titlebars    ;; Add titlebars to normal clients and dialogs.
                                      :properties {:titlebars_enabled true}
                                      :rule_any {:type [:normal :dialog]}}))) 
