;;;; base.fnl

;;;; Rewritten (some parts) in fennel from:
;;;; @github
;;;; TorchedSammy/dotfiles

(local beautiful (require "beautiful"))
(local gears (require "gears"))

(local base
       {:gradientColorsLight (or beautiful.gradientColors
                              ["#8795D5"
                               "#BEC6EF"])
        :gradientColorsDark (or beautiful.gradientColors
                                ["#566081"
                                 "#A2B2E6"])
        :gradientColorsMiddle (or beautiful.gradientColors
                                  ["#CDBC6E"
                                   "#B2B6A7"])
        :gradientColorsNight (or beautiful.gradientColors
                                 ["#503d5f"
                                  "#150e1d"])
        })

(fn base.createDarkGradient
  [w h start-n end-n]
  "Create a gradient using gears.color.create_pattern"
  (local gradient
         (gears.color.create_pattern
          {:type "linear"
           :from [0 0]
           :to   [w h]
           :stops [[(or start-n 0)
                    (. base.gradientColorsDark 1)]
                   [(or end-n 1)
                    (. base.gradientColorsDark 2)]]}))
  gradient)

(fn base.createLightGradient
  [w h start-n end-n]
  "Create a gradient using gears.color.create_pattern"
  (local gradient
         (gears.color.create_pattern
          {:type "linear"
           :from [0 0]
           :to   [w h]
           :stops [[(or start-n 0)
                    (. base.gradientColorsLight 1)]
                   [(or end-n 1)
                    (. base.gradientColorsLight 2)]]}))
  gradient) 

(fn base.createMiddleGradient
  [w h start-n end-n]
  "Create a gradient using gears.color.create_pattern"
  (local gradient
         (gears.color.create_pattern
          {:type "linear"
           :from [0 0]
           :to   [w h]
           :stops [[(or start-n 0)
                    (. base.gradientColorsMiddle 1)]
                   [(or end-n 1)
                    (. base.gradientColorsMiddle 2)]]}))
  gradient) 

(fn base.createDarkerGradient
  [w h start-n end-n]
  "Create a gradient using gears.color.create_pattern"
  (local gradient
         (gears.color.create_pattern
          {:type "linear"
           :from [0 0]
           :to   [w h]
           :stops [[(or start-n 0)
                    (. base.gradientColorsNight 1)]
                   [(or end-n 1)
                    (. base.gradientColorsNight 2)]]}))
  gradient) 

base
