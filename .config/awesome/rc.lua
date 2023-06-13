local fennel = require("./fennel")
local gears = require("gears")
fennel.path = fennel.path .. ";.config/awesome/?.fnl"
table.insert(package.loaders or package.searchers, fennel.make_searcher({correlate=true}))
require("config") -- Loads $HOME/.config/awesome/config.fnl 
