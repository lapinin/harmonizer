;;;; macros.fnl

(fn map [func col]
  "Maps a function over a collection."
  (let [out {}]
    (each [idx value (ipairs col)]
      (tset out idx (func value)))
    out))

(fn btn [arg1 arg2 arg3]
  "Better awful.button handling."
  (if (= arg3 nil)
      `(awful.button {} ,arg1 ,arg2)
      `(awful.button ,arg1 ,arg2 ,arg3)))

(fn kb! [...]
  "Better awful.keyboard.append_global_keybindings handling."
  (let [binds ...]
    `(awful.keyboard.append_global_keybindings
      ,(map (fn [bind]
              `(awful.key
                {:key         ,bind.key
                 :modifiers   ,bind.mods
                 :keygroup    ,bind.keygroup
                 :description ,bind.description
                 :name        ,bind.name
                 :group       ,bind.group
                 :on_press    ,bind.action
                 :on_release  ,bind.action_release}))
            binds))))

(fn kbc! [...]
  "Better awful.keyboard.append_client_keybindings handling."
  (let [binds ...]
    `(awful.keyboard.append_client_keybindings
      ,(map (fn [bind]
              `(awful.key
                {:key         ,bind.key
                 :modifiers   ,bind.mods
                 :keygroup    ,bind.keygroup
                 :description ,bind.description
                 :name        ,bind.name
                 :group       ,bind.group
                 :on_press    ,bind.action
                 :on_release  ,bind.action_release}))
            binds))))

{: btn
 : kb!
 : kbc!}
