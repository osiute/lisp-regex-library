;; Логика создания отсортированного массива на основе множества (set) точек символьных кодов
(in-package :regex-library)

(defun fill-endpoints-vector (set vec)
  (let ((i 0))
    (maphash (lambda (key value)
      (declare (ignore value)) ; сообщение компилятору, что я намеренно не использую value
      (setf (aref vec i) key)
      (incf i)
    ) set)
  )
  vec
)

(defun get-sorted-endpoints (ast)
  (assert (not (null ast)) ()
    "get-sorted-endpoints: (null ast). Для пустых узлов нужно использовать ast-empty")
  (let* ((set (extract-endpoints-from-ast ast))
         (n (hash-table-count set))
         (vec (make-array n :element-type 'fixnum)))
    (fill-endpoints-vector set vec)
    (sort vec (function <))
  )
)