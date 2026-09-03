(in-package :regex-library)

;; -----------------------------------------------------------------------------
;; Вспомогательное форматирование меток рёбер
;; -----------------------------------------------------------------------------

;; Преобразует label рёбра (symbol или fixnum) в строковое представление для Graphviz
(defun format-nfa-edge-label (label)
  (cond
    ((eq label :epsilon) "ε")
    ((eq label :anchor-start) "^")
    ((eq label :anchor-end) "$")
    ((eq label :anchor-word-boundary) "\\\\b")
    ((eq label :anchor-non-word-boundary) "\\\\B")
    ((eq label :anchor-start-of-text) "\\\\A")
    ((eq label :anchor-end-of-text) "\\\\z")
    ((eq label :anchor-end-of-text-or-newline) "\\\\Z")
    ((integerp label) (format nil "Class ~A" label))
    (t (format nil "~A" label))
  )
)

;; -----------------------------------------------------------------------------
;; Генерация Graphviz DOT кода
;; -----------------------------------------------------------------------------

;; Форматирует описание узла (принимающее состояние рисуем двойным кругом)
(defun write-dot-node (stream id accept-id)
  (if (= id accept-id)
      (format stream "  node~A [label=\"~A\", shape=doublecircle];~%" id id)
      (format stream "  node~A [label=\"~A\", shape=circle];~%" id id)
  )
)

;; Записывает все исходящие рёбра одного состояния
(defun write-dot-edges (stream src-id edges)
  (dolist (edge edges)
    (let ((lbl (format-nfa-edge-label (nfa-edge-label edge)))
          (target (nfa-edge-target edge)))
      (format stream "  node~A -> node~A [label=\"~A\"];~%" src-id target lbl)
    )
  )
)

;; -----------------------------------------------------------------------------
;; Основной интерфейс генерации DOT
;; -----------------------------------------------------------------------------

;; Генерирует DOT-представление автомата NFA в поток stream (или строку, если stream nil)
(defun nfa-to-dot (nfa &optional (stream nil))
  "Преобразует объект nfa в формат Graphviz DOT."
  (let ((states (nfa-states nfa))
        (start-id (nfa-start-state nfa))
        (accept-id (nfa-accept-state nfa)))
    (format stream "digraph NFA {~%")
    (format stream "  rankdir=LR;~%")
    ;; Фиктивная начальная стрелка к start-state
    (format stream "  start [shape=none, label=\"\"];~%")
    (format stream "  start -> node~A;~%" start-id)
    ;; Вывод всех вершин и рёбер
    (loop for id from 0 below (length states) do
      (write-dot-node stream id accept-id)
      (write-dot-edges stream id (aref states id))
    )
    (format stream "}~%")
  )
)