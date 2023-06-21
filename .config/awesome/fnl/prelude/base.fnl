;;;; base.fnl

;;;; Rewritten (some parts) in fennel from:
;;;; @github
;;;; TorchedSammy/dotfiles

(local beautiful (require "beautiful"))
(local gears (require "gears"))

(local base
       {:width 12
        :gradientColors (or beautiful.gradientColors
                            ["#8795D5"
                             "#BEC6EF"])})

(fn base.createGradient
  [w h start-n end-n]
  "Create a gradient using gears.color.create_pattern"
  (set-forcibly! w (or w base.width))
  (local gradient
         (gears.color.create_pattern
          {:type "linear"
           :from [0 0]
           :to   [w h]
           :stops [[(or start-n 0)
                    (. base.gradientColors 1)]
                   [(or end-n 1)
                    (. base.gradientColors 2)]]}))
  gradient)

base
