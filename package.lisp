(in-package :cl-user)

;; В package.lisp
(defpackage :regex-library
  (:use :cl)
  (:nicknames :regex
              :re)
  (:export
    #:compile-regex         
    #:match-p               
    #:contains-p            
    #:first-match-span 
    #:all-match-spans 
    #:make-match-span-iterator 
    #:do-match-spans 
    #:split
    #:replace-all
  )
)