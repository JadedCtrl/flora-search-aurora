(defsystem "flora-search-aurora"
  :depends-on ("alexandria" "assoc-utils" "cl-charms" "str")
  :build-operation "program-op"
  :build-pathname "flora-search-aurora"
  :entry-point "flora-search-aurora:main"
  :components ((:file "packages")
               (:file "util")
               (:file "display")
               (:file "input")
               (:file "menu")
               (:file "inventory")
               (:file "intermission")
               (:file "overworld.util")
               (:file "overworld")
               (:file "dialogue")
               (:file "engine")
               (:file "flora-search-aurora")
               (:file "res/clocks")
               (:file "res/maps/casino.tmx")
               (:file "res/maps/outdoors.tmx")
               (:file "res/maps/flashback-school.tmx")))
