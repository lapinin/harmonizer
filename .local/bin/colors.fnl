#!/usr/bin/env fennel

(print "")
(for [_ 0 2]
  (io.write "    ")
  (each [_ color (pairs [7 3 6 2 5 1 4])]
    (io.write (.. "\027[4" color "m    ")))
  (io.write "\027[0m\n"))

(io.flush) 
(print "") 
