(in-package :regex-library)

;;; ============================================================================
;;; Вспомогательные функции
;;; ============================================================================

;; Создаёт НКА из строкового представления регулярного выражения
(defun create-nfa (pattern)
  (let* ((ast-root (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast-root))
         (nfa (build-nfa-from-ast ast-root eq-table)))
    nfa
  )
)

;; Проверяет, сброшены ли все 64 элемента вектора start-states в -1
(defun start-states-all-reset-p (start-states-vec)
  (loop for id across start-states-vec
        always (= id -1)
  )
)

;;; ============================================================================
;;; Модульные тесты
;;; ============================================================================

(deftest run-start-states-tests "dfa/start-states"
  (test-reset-start-states #'assert-true #'assert-equal)
  (test-compute-closure-independent #'assert-equal)
  (test-compute-closure-dependent #'assert-true #'assert-equal)
  (test-get-dfa-start-state-caching #'assert-equal)
  (test-get-dfa-start-state-dedup #'assert-equal)
  (test-reset-and-reget-integration #'assert-equal)
)

;; 1. Проверка работы сброса массива стартовых состояний reset-start-states!
(defun test-reset-start-states (assert-true-fn assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa)))
    (get-dfa-start-state dfa 5)
    (funcall assert-equal-fn (aref (dfa-start-states dfa) 5) 0
             "reset-start-states: значение 0 записано по индексу контекста 5")
    
    ;; Выполняем сброс и проверяем вектор
    (reset-start-states! dfa)
    (funcall assert-true-fn (start-states-all-reset-p (dfa-start-states dfa))
             "reset-start-states: все 64 элемента start-states сброшены в -1")
  )
)

;; 2a. Вычисление замыкания для НКА без контекстно-зависимых якорей
(defun test-compute-closure-independent (assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa))
         (closure-ctx0 (compute-start-nfa-closure dfa 0))
         (closure-ctx63 (compute-start-nfa-closure dfa 63)))
    ;; Без якорей замыкания для масок 0 и 63 абсолютно идентичны
    (funcall assert-equal-fn closure-ctx0 closure-ctx63
             "closure-independent (\"(a|b){2,5}c*\"): контекстно-независимый НКА дает одинаковое замыкание")
  )
)

;; 2b. Вычисление замыкания для контекстно-зависимого НКА с якорями ^ и \b
(defun test-compute-closure-dependent (assert-true-fn assert-equal-fn)
  (let* ((nfa (create-nfa "^\\b(abc)+$\\z"))
         (dfa (make-lazy-dfa nfa))
         ;; ctx=0: якоря непроходимы, замыкание содержит только начальную вершину 0
         (closure-no-ctx (compute-start-nfa-closure dfa 0))
         ;; ctx=63: маска флагов активна, замыкание проходит через рёбра якорей
         (closure-full-ctx (compute-start-nfa-closure dfa 63)))
    (funcall assert-equal-fn closure-no-ctx #(0)
             "closure-dependent (\"^\\b(abc)+$\\z\"): при ctx=0 замыкание содержит только стартовое состояние 0")
    (funcall assert-true-fn (= (length closure-full-ctx) 6)
             "closure-dependent (\"^\\b(abc)+$\\z\"): при ctx=63 замыкание продвигается за якоря ^ и \\b ровно на 6 состояний")
  )
)

;; 3a. Проверка получения и кэширования стартового состояния get-dfa-start-state
(defun test-get-dfa-start-state-caching (assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa)))
    (funcall assert-equal-fn (aref (dfa-start-states dfa) 0) -1
             "get-dfa-start-state (\"(a|b){2,5}c*\"): до расчета ячейка start-states равна -1")
    (let ((id1 (get-dfa-start-state dfa 0)))
      (funcall assert-equal-fn id1 0 "get-dfa-start-state (\"(a|b){2,5}c*\"): первичный расчет заносит id 0")
      (funcall assert-equal-fn (aref (dfa-start-states dfa) 0) 0
               "get-dfa-start-state (\"(a|b){2,5}c*\"): значение 0 закэшировано в массиве по индексу 0")
      (funcall assert-equal-fn (get-dfa-start-state dfa 0) id1
               "get-dfa-start-state (\"(a|b){2,5}c*\"): повторный вызов возвращает закэшированный id")
    )
  )
)

;; 3b. Проверка дедупликации состояний при разном контексте для независимого НКА
(defun test-get-dfa-start-state-dedup (assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa))
         (id-ctx0 (get-dfa-start-state dfa 0))
         (id-ctx5 (get-dfa-start-state dfa 5)))
    (funcall assert-equal-fn id-ctx0 id-ctx5
             "get-dfa-start-state (дедупликация): разные контексты возвращают один id 0 благодаря state-map")
    (funcall assert-equal-fn (length (dfa-states dfa)) 1
             "get-dfa-start-state (дедупликация): создан только 1 экземпляр dfa-state")
    (funcall assert-equal-fn (aref (dfa-start-states dfa) 5) 0
             "get-dfa-start-state (дедупликация): ячейка массива по индексу 5 обновлена значением 0")
  )
)

;; 4. Интеграционный тест совместной работы reset-start-states! и get-dfa-start-state
(defun test-reset-and-reget-integration (assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa))
         (id-before (get-dfa-start-state dfa 0)))
    (reset-start-states! dfa)
    (funcall assert-equal-fn (aref (dfa-start-states dfa) 0) -1
             "reset + reget: после сброса ячейка start-states снова равна -1")
    
    (let ((id-after (get-dfa-start-state dfa 0)))
      (funcall assert-equal-fn id-before id-after
               "reset + reget: повторный get-dfa-start-state возвращает тот же id")
      (funcall assert-equal-fn (length (dfa-states dfa)) 1
               "reset + reget: новое состояние dfa-state не создавалось в dfa-states")
    )
  )
)