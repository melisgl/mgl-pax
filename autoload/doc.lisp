(in-package :autoload)

(pax:defsection @autoload-manual (:title "Autoload Manual" :export nil)
  (@links-and-systems pax:section)
  (@api pax:section))

(pax:defsection @api (:title "API" :export nil)
  (autoload pax:macro)
  (defun/autoloaded pax:macro)
  (defvar/autoloaded pax:macro))

(pax:defsection @links-and-systems (:title "Links and Systems" :export nil)
  "Here is the [official
  repository](https://github.com/melisgl/mgl-pax/) and the [HTML
  documentation](http://melisgl.github.io/mgl-pax-world/autoload.html)
  for the latest version."
  ("autoload" asdf:system)
  ("autoload-doc" asdf:system))


;;;; Register in PAX World

(defun autoload-sections ()
  (list @autoload-manual))

(defun autoload-pages ()
  `((:objects
     (, @autoload-manual)
     :source-uri-fn ,(pax:make-github-source-uri-fn
                      "mgl-pax"
                      "https://github.com/melisgl/mgl-pax"))))

(pax:register-doc-in-pax-world :autoload (autoload-sections) (autoload-pages))


#+nil
(progn
  (asdf:load-system "autoload-doc")
  (pax:update-asdf-system-readmes @autoload-manual "autoload"
                                  :formats '(:plain :markdown)))
