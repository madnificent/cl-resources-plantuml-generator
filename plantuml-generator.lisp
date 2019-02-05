(in-package :plantuml-generator)

(defun all-resources ()
  (loop for val being
     the hash-values of mu-cl-resources::*resources*
     collect val))

(defun generate ()
  "Construct full plantuml content."
  (let ((plantuml-specification
         (format nil "~A~&~{~&~A~}~A"
                 (make-plantuml-prefix)
                 (loop for resource in (all-resources)
                    collect (emit-resource-description resource))
                 (make-plantuml-postfix))))
    (when (find :docker *features*)
      (with-open-file (output "/config/output/jsonapi-domain.plantuml" :direction :output :if-exists :supersede)
        (format output "~A" plantuml-specification)))
    (format t "~A" plantuml-specification)))

(defun make-plantuml-prefix ()
  "Creates the prefix for a plantuml definition"
  (format nil "~&@startuml~&set namespaceSeparator none~&"))

(defun make-plantuml-postfix ()
  "Creates the postfix for a plantuml definition"
  (format nil "~&@enduml~&"))

(defun emit-resource-description (resource)
  "Emits the specification for an individual resource"
  (format nil "~&class \"~A\"~&~A~&~A~&"
          (resource-name resource)
          (emit-resource-attributes resource)
          (emit-resource-relationships resource)))

(defun emit-resource-attributes (resource)
  "Emits information on the attributes of a resource"
  (format nil "~{~&\"~A\" : ~A~&~}"
          (loop for attribute in (mu-cl-resources::ld-properties resource)
             append (list (resource-name resource)
                          (string-downcase (mu-cl-resources::json-key attribute))))))

(defun emit-resource-relationships (resource)
  "Emits information on the relationships of a resource"
  (format nil "~&~A~&~A~&"
          (emit-has-one-relationships resource)
          (emit-has-many-relationships resource)))

(defun emit-has-one-relationships (resource)
  "Emits relationships information for has-one relationships of a resource"
  (format nil "~{~&\"~A\" --> \"1\" \"~A\" : ~A > ~&~}"
          (loop for relationship in (mu-cl-resources::has-one-links resource)
             append (list (resource-name resource) ; from
                          (resource-name ; to
                           (mu-cl-resources::find-resource-by-name
                            (mu-cl-resources::resource-name relationship)))
                          (mu-cl-resources::request-path relationship)))))

(defun emit-has-many-relationships (resource)
  "Emits relationships information for has-many relationships of a resource"
  (format nil "~{~&\"~A\" --> \"*\" \"~A\" : ~A > ~&~}"
          (loop for relationship in (mu-cl-resources::has-many-links resource)
             append (list (resource-name resource) ; from
                          (resource-name ; to
                           (mu-cl-resources::find-resource-by-name
                            (mu-cl-resources::resource-name relationship)))
                          (mu-cl-resources::request-path relationship)))))


;;;; helpers

(defun resource-name (resource)
  "Returns the name of the resource in this diagram."
  (mu-cl-resources::json-type resource))
