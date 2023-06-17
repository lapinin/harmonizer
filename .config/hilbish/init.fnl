(import-macros {: aliases!
                : paths!} :macros)

(local promptua (require :promptua))

(promptua.setTheme "logic")
(promptua.init)

(os.setenv "EDITOR" "emacs")

(paths! [ {:dir "~/.local/bin/"}
          {:dir "~/.cargo/bin/"}
          {:dir "~/.cabal/bin/"}
          {:dir "~/.ghcup/bin/"} ])

(aliases! [ {:cmd ":cls" :orig "clear"}
            {:cmd ":dni" :orig "sudo dnf install -y"}
            {:cmd ":dnu" :orig "sudo dnf update -y"}
            {:cmd ":gc"  :orig "git clone"}
            {:cmd ":l"   :orig "ls -AFhlv --group-directories-first --color=always"}
            {:cmd ":pow" :orig "poweroff"}
            {:cmd ":re"  :orig "reboot"}
            {:cmd ":fnl" :orig "fennel --compile"} ])
