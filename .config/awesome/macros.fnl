;;; macros.fnl

(fn map [func col]
  "Maps a function over a collection."
  (let [out {}]
    (each [idx value (ipairs col)]
      (tset out idx (func value)))
    out))

{:btn (fn [arg1 arg2 arg3]
       "Better awful.button handling."
        (if (= arg3 nil)
          `(awful.button {} ,arg1 ,arg2)
          `(awful.button ,arg1 ,arg2 ,arg3)))

  :kb! (fn [...]
       "Better awful.keyboard.append_global_keybindings handling."
       (let [binds ...]
         `(awful.keyboard.append_global_keybindings
            ,(map (fn [bind]
                  `(awful.key
                     {:modifiers   ,bind.mods
                      :keygroup    ,bind.keygroup
                      :key         ,bind.key
                      :description ,bind.description
                      :group       ,bind.group
                      :on_press    ,bind.action}))
                    binds))))
}
