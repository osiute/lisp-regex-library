(in-package :regex-library)

;;; ============================================================================
;;; Вспомогательные функции
;;; ============================================================================

;; Создаёт тестовый ДКА с искусственно ограниченным размером кэша
(defun make-test-dfa-with-limit (pattern max-states)
  (let* ((nfa (create-nfa pattern))
         (dfa (make-lazy-dfa nfa)))
    (setf (dfa-max-states dfa) max-states)
    dfa
  )
)

;; Заполняет ДКА указанным количеством искусственных состояний
(defun populate-dfa-states (dfa count)
  (dotimes (i count)
    (let ((nfa-set (make-array i :element-type 'fixnum)))
      (dotimes (j i) (setf (aref nfa-set j) j))
      (get-or-register-dfa-state! dfa nfa-set)
    )
  )
)

;;; ============================================================================
;;; Модульные тесты
;;; ============================================================================

(deftest run-cache-tests "dfa/cache"
  (test-dfa-cache-full-p #'assert-true #'assert-equal)
  (test-flush-dfa-cache #'assert-true #'assert-equal)
  (test-ensure-cache-space #'assert-true #'assert-equal)
)

;; 1. Проверка правильности срабатывания граничных условий заполнения кэша
(defun test-dfa-cache-full-p (assert-true-fn assert-equal-fn)
  (let ((dfa (make-test-dfa-with-limit "(a|b){2,5}c*" 2)))
    ;; 0 состояний из 2
    (funcall assert-equal-fn (dfa-cache-full-p dfa) nil "кэш не полон (0/2)")
    
    ;; 1 состояние из 2
    (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 1))
    (funcall assert-equal-fn (dfa-cache-full-p dfa) nil "кэш не полон (1/2) при регистрации существующего состояния")

    ;; Стараемся зарегистрировать уже существующее состояние.
    (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 1))
    (funcall assert-equal-fn (dfa-cache-full-p dfa) nil "кэш не полон (1/2)")

    ;; 2 состояния из 2 (достижение лимита)
    (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 2))
    (funcall assert-true-fn (dfa-cache-full-p dfa) "кэш полон (2/2)")
  )
)

;; 2. Проверка полной синхронной очистки всех 3 структур ДКА
(defun test-flush-dfa-cache (assert-true-fn assert-equal-fn)
  (let ((dfa (make-test-dfa-with-limit "(a|b){2,5}c*" 10)))
    (populate-dfa-states dfa 3)
    (get-dfa-start-state dfa 0)
    
    (flush-dfa-cache! dfa)
    
    (funcall assert-equal-fn (length (dfa-states dfa)) 0 "вектор dfa-states обнулен")
    (funcall assert-equal-fn (hash-table-count (dfa-state-map dfa)) 0 "state-map очищена")
    (funcall assert-true-fn (start-states-all-reset-p (dfa-start-states dfa))
             "все стартовые состояния сброшены в -1")
    
    ;; Генерация ID началась заново с 0
    (let ((new-id (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 100))))
      (funcall assert-equal-fn new-id 0 "после сброса первое состояние снова получает id 0")
    )
  )
)

;; 3. Проверка гарантированного освобождения места при достижении лимита
(defun test-ensure-cache-space (assert-true-fn assert-equal-fn)
  (let ((dfa (make-test-dfa-with-limit "(a|b){2,5}c*" 2)))
    (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 1))
    (ensure-cache-space! dfa)
    (funcall assert-equal-fn (length (dfa-states dfa)) 1
             "ensure-cache-space! не сбрасывает неполный кэш")
    
    ;; Превышение лимита 2/2
    (get-or-register-dfa-state! dfa (make-array 1 :element-type 'fixnum :initial-element 2))
    (ensure-cache-space! dfa)
    (funcall assert-equal-fn (length (dfa-states dfa)) 0
             "ensure-cache-space! автоматически сбрасывает заполненный кэш")
  )
)