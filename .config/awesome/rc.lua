-- rc.lua

-- Common dirs.
local luaLibs = "/.local/share/lualibs/"
local libsDir = os.getenv("HOME") .. luaLibs
local configDir = os.getenv("HOME") .. "/.config/awesome/"

package.path = package.path .. ";" .. libsDir .. "?.lua"

-- Make awm aware of fennel.
local fennel = require("fennel")

searcher = fennel.makeSearcher({
    correlate = true,
    useMetadata = true,
    allowedGlobals = false
})

fennel.path = fennel.path .. ";" .. configDir .. "?.fnl;"
table.insert(package.loaders or package.searchers, fennel.searcher)
debug.traceback = fennel.traceback
require("harmonizer") -- Load files from $HOME/XDG_CONFIG_HOME/awesome/harmonizer.fnl
