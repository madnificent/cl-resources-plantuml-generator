(asdf:defsystem :resources-plantuml-generator
  :name "resources-plantuml-generator"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "PlantUML generator for mu-cl-resources specification."
  :serial t
  :depends-on (mu-cl-resources)
  :components ((:file "packages")
               (:file "plantuml-generator")))
