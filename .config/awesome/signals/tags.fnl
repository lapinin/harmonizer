;;;; tags.fnl

(local awful (require "awful"))

;; Tags layout.
;; Table of layouts to cover with awful.layout.inc.
(tag.connect_signal "request::default_layouts"
	(fn [] (awful.layout.append_default_layouts [ awful.layout.suit.floating ])))
