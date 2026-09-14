;; Таблица непересекающихся эквавалентных классов,
;; реализованная на отсортированном массиве, где поиск ID происходит за O(log k).
(in-package :regex-library)

(defun make-equivalence-table-from-ast (ast)
  "Инициализирует и возвращает объект equivalence-table на основе AST (объекта ast-node)"
  (let ((vec (get-sorted-endpoints ast)))
    (make-equivalence-table 
      :endpoints vec
      :num-classes (1- (length vec))
    )
  )
)

(defun char-to-class-id (char eq-table)
  "Возвращает Class ID для символа по объекту equivalence-table"
  (let ((code (char-code char))
        (vec (equivalence-table-endpoints eq-table)))
    (binary-search-class-id code vec)
  )
)