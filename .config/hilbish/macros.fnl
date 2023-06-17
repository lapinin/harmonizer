;; macros.fnl

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
:paths! (fn [...]
        "Improved hilbish.appendPath handling"
          (let [paths ...]
            (map (fn [path]
                    `(hilbish.appendPath ,path.dir))
                  paths)))}
