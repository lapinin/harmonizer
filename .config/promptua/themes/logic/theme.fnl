(local promptua (require :promptua))
(local git (require :promptua.providers.git))

(local conf {:prompt {:icon ":harmonizer"
                      :success "stable"
                      :fail "error!"}})
(promptua.setConfig conf) 

(fn git-branch [segment]
  (set segment.defaults {:separator (or (and (git.isDirty) "") " ")})
                        (local branch (git.getBranch))
                        (when (not branch) (lua "return \"\""))
                      branch)

[{:provider :dir.path
  :style "white"}
 {:condition git.isRepo
  :provider git-branch 
  :style "bold white"}
 {:provider :git.dirty
  :style "bold white"}
 {:provider :command.execTime
  :style "bold white"}
 {:separator "\n"}
 {:provider :prompt.icon
  :style "white"}
 {:provider :prompt.failSuccess
  :style "bold"}]
