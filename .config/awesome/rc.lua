-- rc.lua

local luaLibs = "/.local/share/lualibs/"
local libsDir = os.getenv("HOME") .. luaLibs
local configDir = os.getenv("HOME") .. "/.config/awesome/"

package.path = package.path .. ";" .. libsDir .. "?.lua"

local fennel = require("fennel")

searcher = fennel.makeSearcher({
    correlate = true,
    useMetadata = true,
    allowedGlobals = false
})

fennel.path = fennel.path .. ";" .. configDir .. "?.fnl;" .. configDir .. "fnl/?.fnl;" .. configDir .. "fnl/?/?.fnl"
table.insert(package.loaders or package.searchers, fennel.searcher)
debug.traceback = fennel.traceback
require("harmonizer")

