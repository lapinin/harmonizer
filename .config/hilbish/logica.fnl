;;;; logica.fnl

(import-macros {: aliases!
                : envars!
                : module!
                : paths!} :macros)

(module! (b :bait)
         (c :commander)
         (f :fennel)
         (l :lunacolors)
         (p :promptua))

(local unreadCount 0)
(local running false)

(fn promptua-theme
  [theme]
  (p.setTheme theme)
  (p.init))

(promptua-theme "simpli")

;; (hilbish.runnerMode
;;  (fn [input]
;;    (let [ok (pcall f.eval input)]
;;      (when ok (lua "return input, 0, nil"))
;;      (hilbish.runner.sh input))))

(c.register "ver" (fn [] (print hilbish.ver)))

(set hilbish.opts.greeting false)
(set hilbish.opts.motd false)
(set hilbish.opts.autocd true)

(hilbish.inputMode "vim")

;; IDEA: Make notifications with the new hilbish.message interface.

(envars! [{:name "EDITOR" :value "emacs"}])

(paths! [{:dir "~/.local/bin/"}
         {:dir "~/.cargo/bin/"}
         {:dir "~/.cabal/bin/"}
         {:dir "~/.ghcup/bin/"}
         {:dir "~/.elan/bin/"}]) 

(aliases! [
           {:cmd ":cls"  :orig "clear"}
           {:cmd ":dni"  :orig "sudo dnf install -y"}
           {:cmd ":dnr"  :orig "sudo dnf remove -y"}
           {:cmd ":dnu"  :orig "sudo dnf update -y"}
           {:cmd ":fnl"  :orig "fennel --compile"}
           {:cmd ":fnlc" :orig "check.fnl -c ~/.local/bin/check-opts.fnl"} 
           {:cmd ":gc"   :orig "git clone"}
           {:cmd ":l"    :orig "ls -AFhlv --group-directories-first --color=always"}
           {:cmd ":pow"  :orig "poweroff"}
           {:cmd ":q"    :orig "exit"}
           {:cmd ":re"   :orig "reboot"}
           ])
