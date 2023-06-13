(local promptua (require :promptua))
(local git (require :promptua.providers.git))

(promptua.setConfig {:logic {:shlvl 3} :prompt {:icon ":"}})

[{:provider :dir.path :style :blue}
 {:condition git.isRepo
  :provider (fn [segment]
              (set segment.defaults
                   {:separator (or (and (git.isDirty) "") " ")})
              (local branch (git.getBranch))
              (when (not branch) (lua "return \"\""))
              branch)
  :style :gray}
 {:provider :git.dirty :style :gray}
 {:provider :command.execTime :style "bold cyan"}
 {:condition (fn []
               (> (- (os.getenv :SHLVL) promptua.config.logic.shlvl) 0))
  :icon " "
  :provider (fn []
              (- (+ (os.getenv :SHLVL) 1) promptua.config.logic.shlvl))
  :style :red}
 {:separator "\n"}
 {:provider :prompt.failSuccess}]	
