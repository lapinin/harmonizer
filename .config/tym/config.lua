-- config.lua

local luaLibs = "/.local/share/lualibs/"
local libsDir = os.getenv("HOME") .. luaLibs

package.path = package.path .. ";" .. libsDir .. "?.lua"

local fennel = require("fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)
fennel.path = fennel.path .. ";.config/tym/?.fnl"
fennel.dofile(string.format("%s/.config/tym/castacht.fnl", os.getenv("HOME")))
require("castacht")
