(in-package :regex-library)

;; -----------------------------------------------------------------------------
;; Вспомогательные функции канонического сравнения НКА
;; -----------------------------------------------------------------------------

;; Преобразует ребро в список для упрощения сравнения
(defun edge-to-list (edge)
  (list (nfa-edge-label edge) (nfa-edge-target edge))
)

;; Сортирует список рёбер состояния в канонический вид
(defun canonical-state-edges (edges)
  (sort (mapcar #'edge-to-list edges)
        (lambda (e1 e2)
          (if (= (second e1) (second e2))
              (string< (princ-to-string (first e1))
                       (princ-to-string (first e2)))
              (< (second e1) (second e2))
          )
        )
  )
)

;; Проверяет два НКА на структурную эквивалентность
(defun nfa-structurally-equal-p (nfa1 nfa2)
  (and (= (nfa-start-state nfa1) (nfa-start-state nfa2))
       (= (nfa-accept-state nfa1) (nfa-accept-state nfa2))
       (= (length (nfa-states nfa1)) (length (nfa-states nfa2)))
       (dotimes (i (length (nfa-states nfa1)) t)
         (unless (equalp (canonical-state-edges (aref (nfa-states nfa1) i))
                         (canonical-state-edges (aref (nfa-states nfa2) i)))
           (return nil)
         )
       )
  )
)

;; -----------------------------------------------------------------------------
;; Набор тестов для reverse-nfa
;; -----------------------------------------------------------------------------

(deftest run-nfa-reverse-tests "nfa/reverse"
  (test-reverse-nfa-minimal #'assert-true)
  (test-reverse-nfa-linear-and-labels #'assert-true)
  (test-reverse-nfa-branching-and-cycles #'assert-true)
  (test-reverse-nfa-involution #'assert-true)
)

;; 1. Тест одиночного состояния
(defun test-reverse-nfa-minimal (assert-true-fn)
  (let* ((orig (make-test-nfa '(())))
         (expected (make-test-nfa '(())))
         (rev (reverse-nfa orig)))
    (funcall assert-true-fn (nfa-structurally-equal-p rev expected)
             "разворот автоматов с 1 состоянием без рёбер сохраняет структуру")
  )
)

;; 2. Линейная цепочка с разными типами меток
(defun test-reverse-nfa-linear-and-labels (assert-true-fn)
  ;; Исходный: 0 -eps-> 1 -[class 10]-> 2 -^-> 3 (Start: 0, Accept: 3)
  (let* ((orig (make-test-nfa '( ((:epsilon 1))
                                 ((10 2))
                                 ((:anchor-start 3))
                                 () )))
         ;; Ожидаемый: 3 -^-> 2 -[class 10]-> 1 -eps-> 0 (Start: 3, Accept: 0)
         (expected (make-nfa :states (vector '()
                                             '((:epsilon 0))
                                             '((10 1))
                                             '((:anchor-start 2)))
                             :start-state 3
                             :accept-state 0))
         (rev (reverse-nfa orig)))
    ;; Приводим элементы expected к nfa-edge структуры для корректности
    (dotimes (i (length (nfa-states expected)))
      (setf (aref (nfa-states expected) i)
            (mapcar (lambda (e) (make-nfa-edge :label (first e) :target (second e)))
                    (aref (nfa-states expected) i))))
    (funcall assert-true-fn (nfa-structurally-equal-p rev expected)
             "разворот линейной цепочки правильно инвертирует направления и метки")
  )
)

;; 3. Разветвления, параллельные рёбра и циклы
(defun test-reverse-nfa-branching-and-cycles (assert-true-fn)
  ;; Исходный: 0 -> 1, 0 -> 2; 1 -> 3, 2 -> 3; 3 -> 3 (self-loop)
  (let* ((orig (make-test-nfa '( ((:epsilon 1) (:epsilon 2))
                                 ((:epsilon 3))
                                 ((:epsilon 3))
                                 ((:epsilon 3)) )))
         (rev (reverse-nfa orig)))
    (funcall assert-true-fn
             (and (= (nfa-start-state rev) 3)
                  (= (nfa-accept-state rev) 0)
                  ;; Из состояния 3 в реверсивном НКА должно быть 3 исходящих ребра: в 1, 2 и 3
                  (= (length (aref (nfa-states rev) 3)) 3)
                  ;; Из состояния 0 в реверсивном НКА не должно быть исходящих рёбер
                  (null (aref (nfa-states rev) 0)))
             "разворот графа с разветвлениями и петлями корректно переносит рёбра")
  )
)

;; 4. Инволюция (двойной разворот)
(defun test-reverse-nfa-involution (assert-true-fn)
  (let* ((orig (make-test-nfa '( ((:anchor-start 1) (:epsilon 2))
                                 ((10 3))
                                 ((:anchor-word-boundary 3))
                                 ((:epsilon 3) (:anchor-end 4))
                                 () )))
         (double-rev (reverse-nfa (reverse-nfa orig))))
    (funcall assert-true-fn (nfa-structurally-equal-p double-rev orig)
             "двойной разворот (reverse-nfa (reverse-nfa nfa)) дает исходный автомат")
  )
)