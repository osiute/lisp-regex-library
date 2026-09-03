;; Создаёт новый, развёрнутый объект nfa на основе существующего прямого.
(in-package :regex-library)

;; Принимает НКА и возвращает новый НКА с инвертированным направлением всех рёбер
(defun reverse-nfa (nfa-to-reverse)
  (let ((reversed-nfa (make-raw-reversed-nfa nfa-to-reverse))
        (orig-states (nfa-states nfa-to-reverse)))
    (dotimes (src (length orig-states))
      (reverse-state-edges! reversed-nfa src (aref orig-states src))
    )
    reversed-nfa
  )
)

;; Создаёт пустой НКА с поменянными местами start-state и accept-state
(defun make-raw-reversed-nfa (nfa-to-reverse)
  (let* ((size (length (nfa-states nfa-to-reverse)))
         (new-states (make-array size :element-type 'list :initial-element nil)))
    (make-nfa :states new-states
              :start-state (nfa-accept-state nfa-to-reverse)
              :accept-state (nfa-start-state nfa-to-reverse))
  )
)

;; Добавляет развёрнутое ребро: из orig-target в orig-src с сохранением label
(defun add-reversed-edge! (reversed-nfa orig-src label orig-target)
  (let ((edge (make-nfa-edge :label label :target orig-src))
        (states (nfa-states reversed-nfa)))
    (push edge (aref states orig-target))
  )
)

;; Обходит исходящие рёбра состояния orig-src и добавляет их в реверсивный НКА
(defun reverse-state-edges! (reversed-nfa orig-src edges)
  (dolist (edge edges)
    (add-reversed-edge! reversed-nfa
                        orig-src
                        (nfa-edge-label edge)
                        (nfa-edge-target edge))
  )
)