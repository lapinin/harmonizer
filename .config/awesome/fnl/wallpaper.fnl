(local awful (require "awful"))
(local gears (require "gears"))
(local base (require "prelude.base"))
(local wallpaper {})

(fn wallpaper.gradient
  [screen]
  (awful.wallpaper {:screen screen
                    :bg (base.createGradient 1920 1080)}))

wallpaper
