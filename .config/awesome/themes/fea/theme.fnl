;;;; theme.fnl (fea)

(local gears (require "gears"))
(local theme-name "fea")
(local theme-assets (require "beautiful.theme_assets"))
(local xresources (require "beautiful.xresources"))
(local xrdb (xresources.get_current_theme))
(local dpi xresources.apply_dpi)
(local gfs (require "gears.filesystem"))
(local themes-path (gfs.get_themes_dir))
(local lume (require "lume"))
(local wallpaper (require "fnl.wallpaper"))

;; TODO: Finish all of this...
(local colors {:attention      "#F0E7FA"
               :attention-alt  "#FAF9E7"
               :focus          "#F2F3F4"
               :normal         "#E8ECF5"
               :bg             "#EBEEF6"
               :bg-alt         "#E7E8FA"
               :fg             "#0D0805"
               :fg-alt         "#090B10"
               :error          "#FF3100"
               :warning        "#CC3E00"
               :good           "#5A871C"}) 

(local fonts {:font                     "sans-serif 8"
              :taglist_font             "serif 8"
              :hotkeys_font             "serif 8"
              :hotkeys_description_font "serif 8"})

(local taglist {:taglist_fg_focus           colors.attention
                :taglist_bg_focus           colors.bg-alt
                :taglist_fg_error           colors.error
                :taglist_bg_error           colors.bg-alt
                :taglist_fg_occupied        colors.fg
                :taglist_bg_occupied        colors.bg-alt
                :taglist_fg_empty           colors.fg-alt
                :taglist_bg_empty           colors.bg-alt
                :taglist_fg_volatile        colors.fg
                :taglist_bg_volatile        colors.bg-alt
                :taglist_fg_hover           colors.attention-alt
                :taglist_bg_hover           colors.bg-alt
                :taglist_shape_border_width (dpi 1)
                :taglist_shape_border_color colors.bg})

(local titlebar {:titlebar_fg_focus  colors.fg
                 :titlebar_bg_focus  colors.focus
                 :titlebar_fg_normal colors.fg
                 :titlebar_bg_normal colors.normal})

(local spacing {:border_width         (dpi 1)
                :screen_margin        (dpi 4) 
                :useless_gap          (dpi 8)
                :menu_height          (dpi 32) 
                :menu_width           (dpi 180)
                :menu_border_width    (dpi 0)
                :hotkeys_group_margin (dpi 20)})

(local wibar {;;:wibar_position         :bottom
              :wibar_ontop              false
              :wibar_height             (dpi 1)
              :wibar_fg                 colors.fg
              :wibar_bg                 colors.bg-alt
              :wibar_border_color       colors.bg-alt
              :wibar_border_width       (dpi 0)
              :wibar_border_radius      (dpi 0)
              :tasklist_disable_icon    true
              :tasklist_plain_task_name true
              :tasklist_bg_focus        colors.bg-alt
              :tasklist_fg_focus        colors.attention
              :tasklist_bg_normal       colors.bg-alt
              :tasklist_fg_normal       colors.fg-alt
              :tasklist_bg_minimize     colors.bg-alt
              :tasklist_fg_minimize     colors.fg-alt
              :tasklist_bg_error        colors.bg-alt
              :tasklist_fg_error        colors.error
              :tasklist_spacing         (dpi 0)
              :tasklist_align           :center})

(screen.connect_signal "request::wallpaper" wallpaper.dark) 

(local theme
       (lume.merge
        colors
        taglist
        titlebar
        fonts
        spacing
        wibar)) 

theme
