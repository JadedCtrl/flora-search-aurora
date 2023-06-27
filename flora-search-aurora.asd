(defsystem "flora-search-aurora"
  :depends-on ("alexandria" "assoc-utils" "cl-charms" "cl-tiled" "str")
  :build-operation "program-op"
  :build-pathname "flora-search-aurora"
  :entry-point "flora-search-aurora:main"
  :components ((:file "util")
               (:file "display")
               (:file "input")
               (:file "ui")
               (:file "overworld.util")
               (:file "overworld")
               (:file "dialogue")
               (:file "engine")
               (:file "flora-search-aurora")
               (:file "res/maps/casino.tmx")
               (:file "res/maps/outdoors.tmx")))
