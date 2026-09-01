;; Функции поиска эпсилон-замыкания с учётом контекста для множества состояний НКА
(in-package :regex-library)

;; Вычисляет эпсилон-замыкание для initial-states с учётом битового контекста context.
;; Возвращает новый отсортированный статический вектор fixnum состояний.
(defun compute-nfa-closure (nfa initial-states context queue visited)
  (init-closure-queue! queue visited initial-states)
  (let ((head 0))
    ;; Обход в ширину по плоскому буферу queue
    (loop while (< head (length queue)) do
      (let ((curr-state (aref queue head)))
        (incf head)
        (process-closure-state! nfa curr-state context queue visited)
      )
    )
    ;; Создание статического отсортированного результата
    (finalize-closure-array queue)
  )
)

;; Извлекает статический отсортированный массив из очереди
(defun finalize-closure-array (queue)
  (let* ((len (length queue))
         (result (make-array len :element-type 'fixnum)))
    (loop for i from 0 below len do
      (setf (aref result i) (aref queue i))
    )
    (sort result #'<)
  )
)

;; traversable — проходимый.
;; Проверяет, можно ли пройти по ребру edge при заданных битовых флагах context.
(defun nfa-edge-traversable-p (edge context)
  (let ((label (nfa-edge-label edge)))
    (cond
      ((eq label :epsilon) t)
      ((eq label :anchor-start) (logbitp 0 context)) ; -------1
      ((eq label :anchor-abs-start) (logbitp 1 context)) ; ------1-
      ((eq label :anchor-end) (logbitp 2 context)) ; -----1--
      ((eq label :anchor-abs-end-newline) (logbitp 3 context)) ; ----1---
      ((eq label :anchor-abs-end) (logbitp 4 context)) ; ---1----
      ((eq label :anchor-word-boundary) (logbitp 5 context)) ; --1-----
      ((eq label :anchor-non-word-boundary) (not (logbitp 5 context))) ; --0-----
      (t nil)
    )
  )
)

;; Подготавливает буферы: очищает visited, сбрасывает queue и заносит стартовые состояния
(defun init-closure-queue! (queue visited initial-states)
  (fill visited 0)
  (setf (fill-pointer queue) 0)
  (loop for st across initial-states do
    (when (zerop (sbit visited st))
      (setf (sbit visited st) 1)
      (vector-push-extend st queue)
    )
  )
)

;; Обрабатывает исходящие рёбра одного состояния и добавляет новые целевые узлы в очередь
(defun process-closure-state! (nfa curr-state context queue visited)
  (let ((edges (aref (nfa-states nfa) curr-state)))
    (dolist (edge edges)
      (when (nfa-edge-traversable-p edge context)
        (let ((target (nfa-edge-target edge)))
          (when (zerop (sbit visited target))
            (setf (sbit visited target) 1)
            (vector-push-extend target queue)
          )
        )
      )
    )
  )
)