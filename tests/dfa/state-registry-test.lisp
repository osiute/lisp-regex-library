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

;;; ============================================================================
;;; Модульные тесты
;;; ============================================================================

(deftest run-state-registry-tests "dfa/state-registry"
  (test-nfa-set-accept-p #'assert-true #'assert-equal)
  (test-create-dfa-state #'assert-true #'assert-equal)
  (test-register-dfa-state #'assert-true #'assert-equal)
  (test-get-or-register-dfa-state #'assert-true #'assert-equal)
)

;; Проверяет корректность поиска принимающего состояния в подмножестве
(defun test-nfa-set-accept-p (assert-true-fn assert-equal-fn)
  (let ((accept-id 39)
        (empty-set (make-array 0 :element-type 'fixnum))
        (non-accept-set (make-array 3 :element-type 'fixnum :initial-contents '(0 1 5)))
        (accept-set (make-array 3 :element-type 'fixnum :initial-contents '(10 20 39))))
    
    ;; Проверка пустых и неподходящих подмножеств
    (funcall assert-equal-fn (nfa-set-accept-p empty-set accept-id) nil
             "пустой nfa-set не содержит accept-state")
    (funcall assert-equal-fn (nfa-set-accept-p non-accept-set accept-id) nil
             "nfa-set без accept-state возвращает nil")
    
    ;; Проверка наличия терминального состояния
    (funcall assert-true-fn (nfa-set-accept-p accept-set accept-id)
             "nfa-set с accept-state возвращает t")
  )
)

;; Проверяет создание объектов dfa-state и установку флага accept-p
(defun test-create-dfa-state (assert-true-fn assert-equal-fn)
  (let* ((accept-id 39)
         (vec-normal (make-array 2 :element-type 'fixnum :initial-contents '(4 5)))
         (vec-accept (make-array 2 :element-type 'fixnum :initial-contents '(38 39)))
         (state-normal (create-dfa-state vec-normal accept-id))
         (state-accept (create-dfa-state vec-accept accept-id)))
    
    ;; Проверка сохранения вектора и корректности флага accept-p
    (funcall assert-equal-fn (dfa-state-nfa-set state-normal) vec-normal
             "вектор nfa-set сохранен в dfa-state")
    (funcall assert-equal-fn (dfa-state-accept-p state-normal) nil
             "обычное состояние: accept-p = nil")
    (funcall assert-true-fn (dfa-state-accept-p state-accept)
             "принимающее состояние: accept-p = t")
  )
)

;; Проверяет принудительную регистрацию состояния в векторе states и state-map
(defun test-register-dfa-state (assert-true-fn assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa))
         (nfa-set (make-array 2 :element-type 'fixnum :initial-contents '(0 1)))
         (state (create-dfa-state nfa-set 39))
         (id0 (register-dfa-state! dfa nfa-set state)))
    
    ;; Проверка присвоения id, изменения размера массива и реестра
    (funcall assert-equal-fn id0 0 "первое состояние получает id 0")
    (funcall assert-equal-fn (length (dfa-states dfa)) 1 "длина dfa-states = 1")
    (funcall assert-true-fn (eq (aref (dfa-states dfa) 0) state) "объект сохранен в массиве")
    (funcall assert-equal-fn (gethash nfa-set (dfa-state-map dfa)) 0 "маппинг в state-map создан")
  )
)

;; Вспомогательная функция для проверки попаданий в кэш (Cache Hit)
(defun test-get-or-register-cache-hit (dfa set-a assert-equal-fn)
  (let ((set-a-copy (copy-seq set-a)))
    ;; Запросы по той же ссылке и по эквивалентному вектору
    (funcall assert-equal-fn (get-or-register-dfa-state! dfa set-a) 0
             "cache hit по той же ссылке на объект")
    (funcall assert-equal-fn (get-or-register-dfa-state! dfa set-a-copy) 0
             "cache hit по эквивалентной копии вектора (equalp)")
  )
)

;; Проверяет логику «поиск или регистрация» (Cache Miss / Cache Hit)
(defun test-get-or-register-dfa-state (assert-true-fn assert-equal-fn)
  (let* ((nfa (create-nfa "(a|b){2,5}c*"))
         (dfa (make-lazy-dfa nfa))
         (set-a (make-array 2 :element-type 'fixnum :initial-contents '(1 2)))
         (set-b (make-array 2 :element-type 'fixnum :initial-contents '(38 39))))
    
    ;; Первичный промах кэша
    (funcall assert-equal-fn (get-or-register-dfa-state! dfa set-a) 0 "cache miss: id 0")
    
    ;; Повторные попадания в кэш
    (test-get-or-register-cache-hit dfa set-a assert-equal-fn)
    
    ;; Регистрация второго независимого состояния
    (let ((id1 (get-or-register-dfa-state! dfa set-b)))
      (funcall assert-equal-fn id1 1 "cache miss для нового множества: id 1")
      (funcall assert-true-fn (dfa-state-accept-p (aref (dfa-states dfa) id1))
               "зарегистрированное состояние с accept-id имеет accept-p = t")
    )
  )
)