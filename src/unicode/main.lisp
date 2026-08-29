;; Таблица непересекающихся эквавалентных классов,
;; реализованная на отсортированном массиве, где поиск ID происходит за O(log k).
(in-package :regex-library)

;; Структура для таблицы эквивалентных классов. 
(defstruct equivalence-table
  (endpoints #(0 (1+ +max-unicode+)) :type (simple-array fixnum(*)))
  (num-classes 1 :type fixnum)
)

(defun make-equivalence-table-from-ast (ast)
  "Инициализирует и возвращает объект equivalence-table на основе ast (объекта ast-node)"
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

(defun binary-search-class-id (code vec)
  (let ((low 0)
        (high (1- (length vec))))
    (loop while (<= low high) do
      (let* ((mid (ash (+ low high) -1))
             (mid-val (aref vec mid)))
        (cond
          ((< code mid-val)
           (setf high (1- mid)))
          ((and (< mid (1- (length vec)))
                (>= code (aref vec (1+ mid))))
           (setf low (1+ mid)))
          (t
           (return mid)))))
  )
)