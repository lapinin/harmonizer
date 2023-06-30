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

(fn promptua-theme
  [theme]
  (p.setTheme theme)
  (p.init))

(promptua-theme "simpli")

(c.register "ver" (fn [] (print hilbish.ver)))
(set hilbish.opts.greeting false)
(hilbish.inputMode "vim")

(envars! [{:name "EDITOR" :value "emacs"}])

(paths! [{:dir "~/.local/bin/"}
         {:dir "~/.cargo/bin/"}
         {:dir "~/.cabal/bin/"}
         {:dir "~/.ghcup/bin/"}
         {:dir "~/.elan/bin/"}]) 

(aliases! [
           {:cmd "cls"  :orig "clear"}
           {:cmd "dni"  :orig "sudo dnf install -y"}
           {:cmd "dnr"  :orig "sudo dnf remove -y"}
           {:cmd "dnu"  :orig "sudo dnf update -y"}
           {:cmd "fnl"  :orig "fennel --compile"}
           {:cmd "fnlc" :orig "check.fnl -c ~/.local/bin/check-opts.fnl"} 
           {:cmd "gc"   :orig "git clone"}
           {:cmd "l"    :orig "ls -AFhlv --group-directories-first --color=always"}
           {:cmd "pow"  :orig "poweroff"}
           {:cmd "q"    :orig "exit"}
           {:cmd "re"   :orig "reboot"}
           ])
