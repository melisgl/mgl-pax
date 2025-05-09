(in-package :mgl-pax)

(defimpl default-connection :sly ()
  (slynk-api:default-connection))

(defimpl call-with-connection :sly (connection thunk)
  (slynk-api:with-connection (connection)
    (funcall thunk)))

(defimpl eval-in-emacs :sly (form &optional nowait)
  (slynk:eval-in-emacs form nowait))
