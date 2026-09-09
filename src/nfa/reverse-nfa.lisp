;; Создаёт новый, развёрнутый объект nfa на основе существующего прямого.
(in-package :regex-library)

;; Создаёт пустой НКА с поменянными местами anchored-start-state и accept-state
(defun make-raw-reversed-nfa (nfa-to-reverse)
  (let* ((size (length (nfa-states nfa-to-reverse)))
         (new-states (make-array size :element-type 'list :initial-element nil)))
    (make-nfa :states new-states
              :unanchored-start-state (nfa-unanchored-start-state nfa-to-reverse)
              :anchored-start-state (nfa-accept-state nfa-to-reverse)
              :accept-state (nfa-anchored-start-state nfa-to-reverse))
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

(defun reverse-unanchored-start-state! (reversed-nfa unanchored-start-id)
  (let* ((loop-edge (make-nfa-edge :label :any-class-id :target unanchored-start-id))
         (anchored-start-id (nfa-anchored-start-state reversed-nfa))
         (edge-to-anchored-start (make-nfa-edge :label :epsilon :target anchored-start-id))
         (states (nfa-states reversed-nfa)))
    (push loop-edge (aref states unanchored-start-id))
    (push edge-to-anchored-start (aref states unanchored-start-id))
  )
)