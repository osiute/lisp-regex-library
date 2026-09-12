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
    #:first-match-span ; (start-match . end-match) или nil для первого совпадения
    #:all-match-spans ; Список всех ((s1 . e1) (s2 . e2) ...)
    #:make-match-span-iterator ; Фабрика лексического замыкания-итератора
    #:do-match-spans ; Макрос обхода по всем диапазонам
  )
)