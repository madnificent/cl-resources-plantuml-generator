(in-package :plantuml-generator)

(declaim (optimize (debug 3) (speed 0)))


(define-condition undefined-resource (simple-error)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Could not find name for undefined resource."))))

(define-condition resource-not-found (simple-error)
  ((resource-name :initarg :resource-name :reader error-resource-name)
   (path :initarg :path :reader error-resource-path :initform "none"))
  (:report (lambda (c s) (format s "Could not find resource from name ~A on path ~A"
                            (error-resource-name c)
                            (error-resource-path c)))))


(defun all-resources ()
  (let (values)
    (org.shirakumo.luckless.hashtable:maphash
     (lambda (k v)
       (declare (ignore k))
       (push v values))
     mu-cl-resources::*resources*)
    (reverse values)))

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
  (format nil "~&class \"~A\"~&~A~&~A~&~A~&"
          (resource-name resource)
          (emit-resource-attributes resource)
          (emit-resource-relationships resource)
          (emit-resource-types resource)))

(defun emit-resource-attributes (resource)
  "Emits information on the attributes of a resource"
  (format nil "~{~&\"~A\" : ~A~&~}"
          (loop for attribute in (mu-cl-resources::direct-ld-properties resource)
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
          (loop for relationship in (mu-cl-resources::direct-has-one-links resource)
             append (list (resource-name resource) ; from
                          (handler-case
                              (resource-name ; to
                               (mu-cl-resources::find-resource-by-name
                                (mu-cl-resources::resource-name relationship)))
                            (undefined-resource (u)
                              (declare (ignore u))
                              (error 'resource-not-found
                                     :resource-name (resource-name resource)
                                     :path (mu-cl-resources::request-path relationship))))
                          (mu-cl-resources::request-path relationship)))))

(defun emit-has-many-relationships (resource)
  "Emits relationships information for has-many relationships of a resource"
  (format nil "~{~&\"~A\" --> \"*\" \"~A\" : ~A > ~&~}"
          (loop for relationship in (mu-cl-resources::direct-has-many-links resource)
             append (list (resource-name resource) ; from
                          (handler-case
                              (resource-name ; to
                               (mu-cl-resources::find-resource-by-name
                                (mu-cl-resources::resource-name relationship)))
                            (undefined-resource (u)
                              (declare (ignore u))
                              (error 'resource-not-found
                                     :resource-name (resource-name resource)
                                     :path (mu-cl-resources::request-path relationship))))
                          (mu-cl-resources::request-path relationship)))))

(defun emit-resource-types (resource)
  "Emits the inheritance for resource"
  (format nil "~{~&\"~A\" <|-- \"~A\"~%~}"
          (loop for superclass in (mu-cl-resources::superclass-names resource)
                append (list (resource-name (mu-cl-resources::find-resource-by-name superclass))
                             (resource-name resource)))))


;;;; helpers
(defun resource-name (resource)
  "Returns the name of the resource in this diagram."
  (unless resource
    (error 'undefined-resource))
  (mu-cl-resources::json-type resource))
