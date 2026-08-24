(in-package :cl-user)

(defpackage :regex-library
  (:use :cl)
  (:export
    #:compile-regex
    #:regex-match-p
    #:regex-search-p
    #:regex-find-all
    #:regex-split
    #:regex-replace-all
  )
)