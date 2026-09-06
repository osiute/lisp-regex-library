;; Функции поиска эпсилон-замыкания с учётом контекста для множества состояний НКА, переходов по class-id
(in-package :regex-library)

;; ----------------------------------------------------------------
;; Общие вспомогательные функции
;; ----------------------------------------------------------------

(declaim (inline reset-work-collections!))
(defun reset-work-collections! (queue visited)
  (declare (type (simple-array bit (*)) visited)
           (type (array fixnum (*)) queue))
  (fill visited 0)
  (setf (fill-pointer queue) 0)
)

(declaim (inline check-or-add-target-state!))
(defun check-or-add-target-state! (target queue visited)
  (declare (type fixnum target)
           (type (simple-array bit (*)) visited)
           (type (array fixnum (*)) queue))
  (when (zerop (sbit visited target))
    (setf (sbit visited target) 1)
    (vector-push-extend target queue)
  )
)

;; Извлекает (создаёт новый) статический отсортированный вектор из очереди
(defun normalize-nfa-buffer (queue)
  (declare (type (array fixnum (*)) queue))
  (let* ((len (length queue))
         (result (make-array len :element-type 'fixnum)))
    (loop for i from 0 below len do
      (setf (aref result i) (aref queue i))
    )
    (sort result #'<)
  )
)

;; ----------------------------------------------------------------
;; Вспомогательные функции для эпсилон-замыкания
;; ----------------------------------------------------------------

;; Подготавливает буферы: очищает visited, сбрасывает queue и заносит стартовые состояния
(defun init-closure-queue! (queue visited initial-states)
  (reset-work-collections! queue visited)
  (loop for st across initial-states do
    (when (zerop (sbit visited st))
      (setf (sbit visited st) 1)
      (vector-push-extend st queue)
    )
  )
)

;; traversable — проходимый.
;; Проверяет, можно ли пройти по ребру edge при заданных битовых флагах context.
(defun nfa-edge-traversable-p (edge context)
  (let ((label (nfa-edge-label edge)))
    (cond
      ((eq label :epsilon) t)
      ((eq label :anchor-start) (logbitp 0 context)) ; -------1
      ((eq label :anchor-start-of-text) (logbitp 1 context)) ; ------1-
      ((eq label :anchor-end) (logbitp 2 context)) ; -----1--
      ((eq label :anchor-end-of-text-or-newline) (logbitp 3 context)) ; ----1---
      ((eq label :anchor-end-of-text) (logbitp 4 context)) ; ---1----
      ((eq label :anchor-word-boundary) (logbitp 5 context)) ; --1-----
      ((eq label :anchor-non-word-boundary) (not (logbitp 5 context))) ; --0-----
      (t nil)
    )
  )
)

;; Обрабатывает исходящие рёбра одного состояния и добавляет новые целевые узлы в очередь
(defun process-closure-state! (nfa curr-state context queue visited)
  (let ((edges (aref (nfa-states nfa) curr-state)))
    (dolist (edge edges)
      (when (nfa-edge-traversable-p edge context)
        (let ((target (nfa-edge-target edge)))
          (check-or-add-target-state! target queue visited)
        )
      )
    )
  )
)

;; ----------------------------------------------------------------
;; Основные функции (API файла для main.lisp)
;; ----------------------------------------------------------------

;; Вычисляет эпсилон-замыкание для initial-states с учётом битового контекста context.
;; Возвращает новый отсортированный статический вектор fixnum состояний.
(defun epsilon-closure (nfa initial-states context queue visited)
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
    (normalize-nfa-buffer queue)
  )
)

;; Вычисляет все переходы из множества состояний initial-states по class-id.
;; Возвращает новый отсортированный статический вектор fixnum состояний.
(defun class-id-transitions (nfa initial-states class-id queue visited)
  (reset-work-collections! queue visited)
  (loop for initial-state across initial-states do
    (dolist (edge (aref (nfa-states nfa) initial-state))
      (when (eql (nfa-edge-label edge) class-id)
        (let ((target (nfa-edge-target edge)))
          (check-or-add-target-state! target queue visited)
        )
      )
    )
  )
  ;; Создание статического отсортированного результата
  (normalize-nfa-buffer queue)
)