;; Таблица непересекающихся эквавалентных классов,
;; реализованная на отсортированном массиве, где поиск ID происходит за O(log k).
(in-package :regex-library)

;; Структура для таблицы эквивалентных классов. 
(defstruct equivalence-table
  (endpoints #(0 (1+ +max-unicode+)) :type (simple-array fixnum(*)))
  (num-classes 1 :type fixnum)
)

;; Возвращает Class ID для символа по заданной таблице эквивалентности
(defun char-to-class-id (char table)
  (let ((code (char-code char))
        (vec (equivalence-table-endpoints table)))
    (binary-search-class-id code vec)
  )
)