(in-package :cl-user)

(defpackage :regex-library
  (:use :cl)
  (:nicknames :regex
              :re)
  (:export
    #:compile-regex         
    #:matches-p               
    #:contains-p            
    #:first-match-span 
    #:all-match-spans 
    #:make-match-span-iterator 
    #:do-match-spans 
    #:split
    #:replace-all
    #:count-disjoint-matches
  )
)
