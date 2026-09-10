(in-package :cl-user)

(defpackage :regex-library
  (:use :cl)
  (:export
    #:regex
    #:compile-regex
    #:regex-match-p
    #:regex-search-p
    #:regex-find-first-match-bounds ; возвращает пару (start . end) для первого совпадения или NIL, если совпадений нет.
    #:make-regex-match-bounds-iterator ; возвращает итератор по всем совпадениям, который при каждом вызове возвращает пару (start . end) для очередного совпадения или NIL, если совпадений больше нет.
    #:regex-match-string ; по переданным (start . end) возвращает подстроку исходного текста, которая соответствует совпадению.
    #:regex-split
    #:regex-replace-all
  )
)