;;;; theme.fnl (oxocarbon w/ minor tweaks)

(local awful (require "awful"))
(local gears (require "gears"))
(local theme-name "oxocarbon")
(local theme-assets (require "beautiful.theme_assets"))
(local xresources (require "beautiful.xresources"))
(local xrdb (xresources.get_current_theme))
(local dpi xresources.apply_dpi)
(local gfs (require "gears.filesystem"))
(local themes-path (gfs.get_themes_dir))
(local lume (require "lume"))

(local wibox (require "wibox"))

(local oxocarbon-dark {:base00 "#161616" 
                       :base01 "#262626" 
                       :base02 "#393939" 
                       :base03 "#525252" 
                       :base04 "#dde1e6" 
                       :base05 "#f2f4f8" 
                       :base06 "#ffffff" 
                       :base07 "#08bdba"
                       :base08 "#3ddbd9" 
                       :base09 "#78a9ff" 
                       :base0A "#ee5396" 
                       :base0B "#33b1ff" 
                       :base0C "#ff7eb6" 
                       :base0D "#42be65" 
                       :base0E "#be95ff" 
                       :base0F "#82cfff"})

(local oxocarbon-extra {:wall      "#000000"
                        :bgerror   "#47192D"
                        :bgwarning "#392D4D"
                        :bggood    "#023938" })
(local xorg-colors {})

(local fonts {:font                     "sans bold 8"
              :taglist_font             "sans bold 8"
              :hotkeys_font             "sans 8"
              :hotkeys_description_font "sans 8"})

(local taglist {:taglist_bg_focus           oxocarbon-dark.base00
                :taglist_fg_focus           oxocarbon-dark.base0E
                :taglist_bg_error           oxocarbon-extra.bgerror
                :taglist_fg_error           oxocarbon-dark.base0A
                :taglist_fg_occupied        oxocarbon-dark.base03
                :taglist_bg_occupied        oxocarbon-dark.base00
                :taglist_fg_empty           oxocarbon-dark.base03
                :taglist_bg_empty           oxocarbon-dark.base00
                :taglist_fg_volatile        oxocarbon-dark.base05
                :taglist_bg_volatile        oxocarbon-dark.base00
                :taglist_fg_hover           oxocarbon-dark.base03
                :taglist_bg_hover           oxocarbon-dark.base00
                :taglist_shape_border_width (dpi 8)
                :taglist_shape_border_color oxocarbon-dark.base00})

(local titlebar {:titlebar_fg_focus  oxocarbon-dark.base04
                 :titlebar_bg_focus  oxocarbon-dark.base01
                 :titlebar_fg_normal oxocarbon-dark.base04
                 :titlebar_bg_normal oxocarbon-dark.base00})

(local spacing {:border_width         (dpi 0)
                :screen_margin        (dpi 0) 
                :useless_gap          (dpi 0)
                :menu_height          (dpi 32) 
                :menu_width           (dpi 180)
                :menu_border_width    (dpi 0)
                :hotkeys_group_margin (dpi 20)})

(local wibar {:wibar_ontop              false
              :wibar_height             (dpi 48)
              :wibar_fg                 oxocarbon-dark.base04
              :wibar_bg                 oxocarbon-dark.base00
              :wibar_border_color       oxocarbon-dark.base00
              :wibar_border_width       (dpi 0)
              :wibar_border_radius      (dpi 0)
              :tasklist_disable_icon    true
              :tasklist_plain_task_name true
              :tasklist_bg_focus        oxocarbon-dark.base00
              :tasklist_fg_focus        oxocarbon-dark.base04
              :tasklist_bg_normal       oxocarbon-dark.base00
              :tasklist_fg_normal       oxocarbon-dark.base03
              :tasklist_bg_minimize     oxocarbon-dark.base00
              :tasklist_fg_minimize     oxocarbon-dark.base01
              :tasklist_bg_error        oxocarbon-dark.bgerror
              :tasklist_fg_error        oxocarbon-dark.base0C
              :tasklist_spacing         (dpi 0)
              :tasklist_align           :center})

(fn wall
  [screen]
  (awful.wallpaper {:screen screen
                    :widget {1 {:image (.. (os.getenv "HOME")
                                           "/.config/awesome/themes/oxocarbon/wallpaper/spiral.png")
                                :resize true
                                :widget wibox.widget.imagebox}
                             :valign "center"
                             :halign "center"
                             :tiled true
                             :widget wibox.container.tile}}))

(screen.connect_signal "request::wallpaper" wall) 

(local theme
       (lume.merge
        oxocarbon-dark
        oxocarbon-extra
        taglist
        titlebar
        fonts
        spacing
        wibar
        )) 

theme
