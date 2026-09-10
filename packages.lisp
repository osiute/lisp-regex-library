(in-package :cl-user)

(defpackage :regex-library
  (:use :cl)
  (:export
    #:regex
    #:compile-regex
    #:regex-match-p
    #:regex-search-p
    #:regex-find-first
    #:regex-find-last
    #:regex-find-all
    #:regex-split
    #:regex-replace-all
    #:run-tests
  )
)