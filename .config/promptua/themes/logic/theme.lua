local promptua = require("promptua")
local git = require("promptua.providers.git")
local conf = {prompt = {icon = ":\238\128\162\238\128\135\238\129\138\238\128\131", success = "\238\128\133\238\128\133\238\128\150\238\129\128\238\128\144", fail = "\238\128\148\238\129\145\238\129\134\238\128\148\238\129\138"}}
promptua.setConfig(conf)
local function git_branch(segment)
  segment.defaults = {separator = ((git.isDirty() and "") or " ")}
  local branch = git.getBranch()
  if not branch then
    return ""
  else
  end
  return branch
end
return {{provider = "dir.path", style = "white"}, {condition = git.isRepo, provider = git_branch, style = "bold white"}, {provider = "git.dirty", style = "bold white"}, {provider = "command.execTime", style = "bold white"}, {separator = "\n"}, {provider = "prompt.icon", style = "white"}, {provider = "prompt.failSuccess", style = "bold"}}
