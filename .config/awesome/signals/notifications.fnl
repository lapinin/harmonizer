;;;; notifications.fnl

(local awful (require "awful"))
(local naughty (require "naughty"))
(local ruled (require "ruled"))

;; Notifications.
(ruled.notification.connect_signal "request::rules"
  (fn [] (ruled.notification.append_rule ;; All notifications will match this rule.
    {:properties {:rule {}
                  :screen awful.screen.preferred
                  :implicit_timeout 5}})))  

(naughty.connect_signal "request::display" (fn [n] (naughty.layout.box {:notification n})))
