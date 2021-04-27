(defsystem "palette"
  :version "0.1.0"
  :author "Patrick Bunetic <soup-jingle@protonmail.com"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "conversion")
		 (:file "generator"))))
  :description "")
