;;;; macros.fnl

(fn map [func col]
  "Maps a function over a collection."
  (let [out {}]
    (each [idx value (ipairs col)]
      (tset out idx (func value)))
    out))

(lambda aliases! [...]
  "Improved hilbish.alias handling"
  (let [aliases ...]
    (map (fn [alias]
           `(hilbish.alias ,alias.cmd ,alias.orig))
         aliases)))

(lambda envars! [...]
  "Improved os.setenv handling"
  (let [envars ...]
    (map (fn [env]
           `(os.setenv ,env.name ,env.value))
         envars)))

(lambda module! [...]
  (var ret {})
  (for [i 1 (length [...])]
    (if (list? (. [...] i))
        (table.insert ret `(local ,(. (. [...] i) 1) (require ,(. (. [...] i) 2))))
        (table.insert ret `(require ,(. [...] i)))))
  ret)

(lambda paths! [...]
  "Improved hilbish.appendPath handling"
  (let [paths ...]
    (map (fn [path]
           `(hilbish.appendPath ,path.dir))
         paths)))

(lambda sh! [str]
  `(hilbish.run ,str))

{: aliases!
 : envars!
 : module!
 : paths!
 : sh!}
