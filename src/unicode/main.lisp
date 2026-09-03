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