;;;; wallpaper.fnl

(local awful (require "awful"))
(local gears (require "gears"))
(local base (require "prelude.base"))
(local wallpaper {})

(fn wallpaper.dark
  [screen]
  (awful.wallpaper {:screen screen
                    :bg (base.createDarkGradient 1920 1080)})) 

(fn wallpaper.light
  [screen]
  (awful.wallpaper {:screen screen
                    :bg (base.createLightGradient 1920 1080)}))

(fn wallpaper.middle
  [screen]
  (awful.wallpaper {:screen screen
                    :bg (base.createMiddleGradient 1920 1080)})) 

(fn wallpaper.night
  [screen]
  (awful.wallpaper {:screen screen
                    :bg (base.createDarkerGradient 1920 1080)})) 

wallpaper
