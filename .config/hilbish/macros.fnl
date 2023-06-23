;;;; macros.fnl

(fn map [func coll]
  {:fnl/docstring "Maps a function over a collection."
   :fnl/arglist [func coll]}
  (let [out {}]
    (each [idx value (ipairs coll)]
      (tset out idx (func value)))
    out))

(lambda aliases! [...]
  "Macro for hilbish.alias handling"
  (let [aliases ...]
    (map (fn [alias]
           `(hilbish.alias ,alias.cmd ,alias.orig))
         aliases)))

(lambda envars! [...]
  "Macro for os.setenv handling"
  (let [envars ...]
    (map (fn [env]
           `(os.setenv ,env.name ,env.value))
         envars)))

(lambda module! [...]
  "Shortcut macro for modules handling"
  (var ret {})
  (for [i 1 (length [...])]
    (if (list? (. [...] i))
        (table.insert ret `(local ,(. (. [...] i) 1) (require ,(. (. [...] i) 2))))
        (table.insert ret `(require ,(. [...] i)))))
  ret)

(lambda paths! [...]
  "Macro for hilbish.appendPath handling"
  (let [paths ...]
    (map (fn [path]
           `(hilbish.appendPath ,path.dir))
         paths)))

(lambda sh! [str]
  "Macro for hilbish.run handling"
  `(hilbish.run ,str))

{: aliases!
 : envars!
 : module!
 : paths!
 : sh!}
