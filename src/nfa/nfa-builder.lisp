(in-package :regex-library)

;; Необходимо динамическое расширение состояний на этапе построения НКА, поэтому используется временный builder.
(defstruct nfa-builder
  (states-vector (make-array 10 :adjustable t :fill-pointer 0) :type vector)
)

;; Возвращает список исходящих рёбер для состояния state-id из builder
(defun nfa-builder-edges (builder state-id)
  (aref (nfa-builder-states-vector builder) state-id)
)

;; Проверяет наличие ребра с заданным label и валидирует target.
;; Добавляет ребро в target, если оно ещё не добавлено.
;; При обнаружении ребра у src, которое ведёт не в target, вызывает ошибку.
(defun check-or-add-class-edge! (builder src class-id target)
  (let ((existing-edge 
          (find class-id (nfa-builder-edges builder src) 
                :key #'nfa-edge-label 
                :test #'eql)))

    (if existing-edge
        (assert (= (nfa-edge-target existing-edge) target)
                ()
                "Конфликт: Класс ~A уже ведёт из ~A в ~A, а не в ~A!~%Недопустимо создавать несколько переходов из одного состояния по одному class-id"
                class-id src (nfa-edge-target existing-edge) target
        )
        (builder-add-edge! builder src class-id target)
    )
  )
)

;; Выделяет новое состояние в builder и возвращает его ID (fixnum)
(defun builder-add-state! (builder)
  (let* ((builder-vec (nfa-builder-states-vector builder))
         (id (length builder-vec)))
    (vector-push-extend nil builder-vec) ; Создание пустого списка переходов
    id)
)

;; Добавляет ребро в nfa-builder
(defun builder-add-edge! (builder src label target)
  (let* ((builder-vec (nfa-builder-states-vector builder))
         (new-edge (make-nfa-edge :label label :target target)))
    (push new-edge (aref builder-vec src))
  )
)

;; Финализирует nfa-builder в неизменяемую структуру nfa
(defun finalize-nfa (builder start-id accept-id)
  (let* ((builder-vec (nfa-builder-states-vector builder))
          (n (length builder-vec))
          (final-array (make-array n :element-type 'list)))
    ;; Для каждого состояния устанавливает указатель на список переходов (объектов nfa-edge)
    (loop for i from 0 below n do
      (setf (aref final-array i) (aref builder-vec i))
    )
    (make-nfa
      :states final-array
      :start-state start-id
      :accept-state accept-id
    )
  )
)