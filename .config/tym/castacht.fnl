(local tym (require "tym"))

(tym.set_config
 {
  :shell "/usr/local/bin/hilbish"
  :font "Martian Mono Cn Md 1.4 10"
  :role "term"
  :cursor_shape "ibeam"
  :cursor_blink_mode "off"
  :padding_horizontal 24
  :padding_vertical 24
  :color_window_background "#161616"
  :scrollback_length 1000
  :autohide true
  :bold_is_bright true
  })
