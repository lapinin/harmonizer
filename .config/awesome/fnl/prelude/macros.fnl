;;;; macros.fnl

(fn map [func coll]
  {:fnl/docstring "Maps a function over a collection."
   :fnl/arglist [func coll]}
  (let [out {}]
    (each [idx value (ipairs coll)]
      (tset out idx (func value)))
    out))

;; TODO
;; (fn client-rule! [...]
;;   (let [rules ...]
;;     `(ruled.client.connect_signal "request::rules"
;;       ,(map (fn [ruled]
;;               `(ruled.client.append_rule ...))))))

(fn btn [arg1 arg2 arg3]
  "Macro for awful.button handling."
  (if (= arg3 nil)
      `(awful.button {} ,arg1 ,arg2)
      `(awful.button ,arg1 ,arg2 ,arg3)))

(fn kb! [...]
  "Macro for awful.keyboard.append_global_keybindings handling."
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
  "Macro for awful.keyboard.append_client_keybindings handling."
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
