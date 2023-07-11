;;;; theme.fnl

(local awful (require "awful"))
(local gears (require "gears"))
(local theme-name "ildaite")
(local theme-assets (require "beautiful.theme_assets"))
(local xresources (require "beautiful.xresources"))
(local xrdb (xresources.get_current_theme))
(local dpi xresources.apply_dpi)
(local gfs (require "gears.filesystem"))
(local themes-path (gfs.get_themes_dir))
(local lume (require "lume"))

(local ildaite-colors {:wa  "#EFEFEF"
                       :bg  "#FFFFFF"
                       :fg  "#000000"
                       :alt "#F7F7F7"
                       :ti  "#EEEEEE"})


(local titlebar {:titlebar_fg_focus  ildaite-colors.fg
                 :titlebar_bg_focus  ildaite-colors.ti
                 :titlebar_fg_normal ildaite-colors.fg
                 :titlebar_bg_normal ildaite-colors.ti})

(local wibar {:wibar_ontop              false
              :wibar_height             (dpi 48)
              :wibar_fg                 ildaite-colors.fg
              :wibar_bg                 ildaite-colors.bg
              :wibar_border_color       ildaite-colors.bg
              :wibar_border_width       (dpi 0)
              :wibar_border_radius      (dpi 0)
              :tasklist_disable_icon    true
              :tasklist_plain_task_name true
              :tasklist_bg_focus        ildaite-colors.bg
              :tasklist_fg_focus        ildaite-colors.fg
              :tasklist_bg_normal       ildaite-colors.bg
              :tasklist_fg_normal       ildaite-colors.fg
              :tasklist_bg_minimize     ildaite-colors.bg
              :tasklist_fg_minimize     ildaite-colors.fg
              :tasklist_bg_error        ildaite-colors.bg
              :tasklist_fg_error        ildaite-colors.alt
              :tasklist_spacing         (dpi 0)
              :tasklist_align           :center})

(local taglist {:taglist_bg_focus           ildaite-colors.alt
                :taglist_fg_focus           ildaite-colors.fg
                :taglist_bg_error           ildaite-colors.bg
                :taglist_fg_error           ildaite-colors.fg
                :taglist_fg_occupied        ildaite-colors.fg
                :taglist_bg_occupied        ildaite-colors.bg
                :taglist_fg_empty           ildaite-colors.fg
                :taglist_bg_empty           ildaite-colors.bg
                :taglist_fg_volatile        ildaite-colors.fg
                :taglist_bg_volatile        ildaite-colors.bg
                :taglist_fg_hover           ildaite-colors.fg
                :taglist_bg_hover           ildaite-colors.bg
                :taglist_shape_border_width (dpi 2)
                :taglist_shape_border_color ildaite-colors.bg})

(local borders {:border_color_active ildaite-colors.fg
                :border_color_normal ildaite-colors.fg
                :border_color_urgent ildaite-colors.fg
                :border_color_new    ildaite-colors.fg})

(local spacing {:border_width  (dpi 1)
                :screen_margin (dpi 2) 
                :useless_gap   (dpi 2)
                :menu_height   (dpi 32) 
                :menu_width    (dpi 180)
                :menu_border_width    (dpi 0)
                :hotkeys_group_margin (dpi 20)})

(local fonts {:font "Fairfax 9"
              :taglist_font "Fairfax 9"
              :hotkeys_font "Fairfax 9"
              :hotkeys_description_font "Fairfax 9"})

(fn wall
  [screen]
  (awful.wallpaper {:screen screen
                    :bg     ildaite-colors.wa}))

(screen.connect_signal "request::wallpaper" wall)

(local theme
       (lume.merge
        ildaite-colors
        titlebar
        wibar
        taglist
        borders
        spacing
        fonts))

theme
