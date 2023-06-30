(local promptua (require :promptua))
(local git (require :promptua.providers.git))

(local conf {:prompt {:icon "$"}
             :dollar {:shlvl 3}})

(promptua.setConfig conf)

(fn git-branch
  [segment]
  (set segment.defaults {:separator (or
                                     (and (git.isDirty) "")
                                     " ")})
  (local branch (git.getBranch))
  (when (not branch)
    (lua "return \"\""))
  branch)

[
 {:provider :dir.path
  :style "white"}
 {:condition git.isRepo
  :provider git-branch
  :style "bold white"}
 {:provider :git.dirty
  :style "bold white"}
 {:provider :command.execTime
  :style "bold cyan"}
 {:separator "\n"} 
 {:provider (fn [] (- (+ (os.getenv :SHLVL) 1) promptua.config.dollar.shlvl))
  :condition (fn [] (> (- (os.getenv :SHLVL) promptua.config.dollar.shlvl) 0))
  :icon "! "
  :style :red}
 {:provider :prompt.failSuccess}
 ]

