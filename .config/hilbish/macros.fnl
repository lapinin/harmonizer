;;;; macros.fnl

(fn map [func col]
  "Maps a function over a collection."
  (let [out {}]
    (each [idx value (ipairs col)]
      (tset out idx (func value)))
    out))

{:aliases! (fn [...]
             "Improved hilbish.alias handling"
             (let [aliases ...]
               (map (fn [alias]
                      `(hilbish.alias ,alias.cmd ,alias.orig))
                    aliases)))

 :envars! (fn [...]
            "Improved os.setenv handling"
            (let [envars ...]
              (map (fn [env]
                     `(os.setenv ,env.name ,env.value))
                   envars)))

 :paths! (fn [...]
           "Improved hilbish.appendPath handling"
           (let [paths ...]
             (map (fn [path]
                    `(hilbish.appendPath ,path.dir))
                  paths)))}
