;; Логика сбора границ непересекающихся классов эквивалентности по объекту AST
(in-package :regex-library)

(defparameter +max-unicode+ #x10FFFF)

;; Добавляет границы диапазона (start-code, end-code + 1) в set
(defun add-endpoint-range (set start-code end-code)
  (assert (and (>= start-code 0) (<= start-code +max-unicode+)) ()
          "endpoints-collector: add-endpoint-range: недопустимое значение start-code ~A" start-code)
  (assert (and (>= end-code 0) (<= end-code +max-unicode+)) ()
          "endpoints-collector: add-endpoint-range: недопустимое значение end-code ~A" end-code)
  (assert (<= start-code end-code) ()
          "endpoints-collector: add-endpoint-range: start-code (~A) > end-code (~A)"
          start-code end-code)

  (setf (gethash start-code set) t)
  (setf (gethash (1+ end-code) set) t)
)

;; Извлекает точки из всех диапазонов символьного AST-класса, помещая их в set
(defun collect-char-class-endpoints (node set)
  (assert (ast-char-class-p node) ()
          "endpoints-collector: collect-char-class-endpoints: node не является ast-char-class")
  (dolist (pair (ast-char-class-ranges node))
    (let ((start-code (char-code (car pair)))
          (end-code (char-code (cdr pair))))
      (add-endpoint-range set start-code end-code)
    )
  )
)

;; Рекурсивно извлекает точки из узлов графа AST, добавляя их в set
(defun collect-ast-endpoints (node set)
  (typecase node
    (ast-literal
     (let ((code (char-code (ast-literal-char node))))
       (add-endpoint-range set code code)
     ))
    (ast-char-class
     (collect-char-class-endpoints node set))
    (ast-concat
     (dolist (child (ast-concat-elements node))
       (collect-ast-endpoints child set)
     ))
    (ast-alt
     (collect-ast-endpoints (ast-alt-left node) set)
     (collect-ast-endpoints (ast-alt-right node) set))
    (ast-star (collect-ast-endpoints (ast-star-child node) set))
    (ast-plus (collect-ast-endpoints (ast-plus-child node) set))
    (ast-question (collect-ast-endpoints (ast-question-child node) set))
    (ast-range (collect-ast-endpoints (ast-range-child node) set))
  )
)

;; Возвращает хэш-таблицу со всеми уникальными точками разделения
(defun extract-endpoints-from-ast (ast)
  (let ((endpoints-set (make-hash-table :test 'eql)))
    ;; Обязательные базовые границы пространства Unicode
    (setf (gethash 0 endpoints-set) t)
    (setf (gethash (1+ +max-unicode+) endpoints-set) t)
    (when ast
      (collect-ast-endpoints ast endpoints-set)
    )
    endpoints-set
  )
)