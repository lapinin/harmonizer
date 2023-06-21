;;;; theme.fnl (fea)

(local gears (require "gears"))
(local theme-name "fea")
(local theme-assets (require "beautiful.theme_assets"))
(local xresources (require "beautiful.xresources"))
(local xrdb (xresources.get_current_theme))
(local dpi xresources.apply_dpi)
(local gfs (require "gears.filesystem"))
(local themes-path (gfs.get_themes_dir))
(local lume (require "modules.lume"))

;;;; FULL OF FLUX

(local colors {:attention      "#FF6D2D"
               :attention-dark "#FF3100"
               :focus          "#F2F3F4"
               :normal         "#E8ECF5"
               :bg             "#EBEEF6"
               :bg-dark        "#E7E8FA"
               :fg             "#090B10"
               :fg-dark        "#0D0805"
               :urgent         "#FF6D2D"})

(local fonts {:font                     "monospace 8"
              :taglist_font             "monospace 8"
              :hotkeys_font             "monospace 8"
              :hotkeys_description_font "monospace 8"})

(local taglist {:taglist_fg_focus           colors.attention
                :taglist_bg_focus           colors.bg-dark
                :taglist_fg_urgent          colors.urgent
                :taglist_bg_urgent          colors.bg-dark
                :taglist_fg_occupied        colors.fg
                :taglist_bg_occupied        colors.bg-dark
                :taglist_fg_empty           colors.fg-dark
                :taglist_bg_empty           colors.bg-dark
                :taglist_fg_volatile        colors.fg
                :taglist_bg_volatile        colors.bg-dark
                :taglist_fg_hover           colors.attention-dark
                :taglist_bg_hover           colors.bg-dark
                :taglist_shape_border_width (dpi 1)
                :taglist_shape_border_color colors.bg})

(local titlebar {:titlebar_fg_focus  colors.fg
                 :titlebar_bg_focus  colors.focus
                 :titlebar_fg_normal colors.fg
                 :titlebar_bg_normal colors.normal})

(local spacing {:border_width         (dpi 1)
                :screen_margin        (dpi 4) 
                :useless_gap          (dpi 2)
                :menu_height          (dpi 32) 
                :menu_width           (dpi 180)
                :menu_border_width    (dpi 0)
                :hotkeys_group_margin (dpi 20)})

(local wibar {;;:wibar_position         :bottom
              :wibar_ontop              false
              :wibar_height             (dpi 36)
              :wibar_fg                 colors.fg
              :wibar_bg                 colors.bg-dark
              :wibar_border_color       colors.bg-dark
              :wibar_border_width       (dpi 0)
              :wibar_border_radius      (dpi 0)
              :tasklist_disable_icon    true
              :tasklist_plain_task_name true
              :tasklist_bg_focus        colors.bg-dark
              :tasklist_fg_focus        colors.attention
              :tasklist_bg_normal       colors.bg-dark
              :tasklist_fg_normal       colors.fg-dark
              :tasklist_bg_minimize     colors.bg-dark
              :tasklist_fg_minimize     colors.fg-dark
              :tasklist_bg_urgent       colors.bg-dark
              :tasklist_fg_urgent       colors.urgent
              :tasklist_spacing         (dpi 0)
              :tasklist_align           :center})

(local theme
       (lume.merge
        colors
        taglist
        titlebar
        fonts
        spacing
        wibar)) 

theme
