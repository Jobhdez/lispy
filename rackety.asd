(asdf:defsystem #:rackety
    :description "A compiler for a racket like language."
    :author "Job Hernandez <hj93@protonmail.com>"
;;    :in-order-to ((asdf:test-op (asdf:test-op #:zetta/tests)))
    :depends-on (#:alexa #:yacc #:alexandria #:trivia #:hunchentoot #:com.inuoe.jzon)
    :serial t
    :pathname "rackety/"
    :components
    ((:file "package")
     ((:module "lexer"
       :components ((:file "lexer"))))
      (:module "parser"
       :components ((:file "parser")))))

