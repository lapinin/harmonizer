local fennel = require("./modules/fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)
fennel.path = fennel.path .. ";.config/awesome/?.fnl"
require("harmonizer")
