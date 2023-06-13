(local promptua (require :promptua))

(promptua.setTheme :logic)
(promptua.init)

(os.setenv :XAUTHORITY
           (string.format "%s/Xauthority"
           (os.getenv :XDG_RUNTIME_DIR)))	

(hilbish.run 
  (string.format "source %s/.ghcup/env"
    (os.getenv :HOME)))

(os.setenv "EDITOR" "emacs")

(hilbish.appendPath "~/.local/bin/")
(hilbish.appendPath "~/.cargo/bin/")
(hilbish.appendPath "~/.cabal/bin/")
(hilbish.appendPath "~/.ghcup/bin/")

(hilbish.alias ":cls" "clear")
(hilbish.alias ":ls" "ls -ahX1 --color=always")
(hilbish.alias ":dni" "sudo dnf install -y")
(hilbish.alias ":dnu" "sudo dnf update -y")
(hilbish.alias ":gc" "git clone")
(hilbish.alias ":fnl" "fennel --compile")
