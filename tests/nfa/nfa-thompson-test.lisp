(in-package :regex-library)

;; -----------------------------------------------------------------------------
;; Вспомогательные функции для тестирования алгоритма Томпсона
;; -----------------------------------------------------------------------------

(defun build-nfa-from-pattern (pattern)
  (let* ((ast (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast))
         (builder (make-nfa-builder))
         (frag (compile-ast-node builder ast eq-table)))
    (finalize-nfa builder (nfa-fragment-start frag) (nfa-fragment-accept frag))
  )
)

;; Возвращает количество состояний в НКА
(defun nfa-states-count (nfa)
  (length (nfa-states nfa))
)

;; -----------------------------------------------------------------------------
;; Тестовые подфункции
;; -----------------------------------------------------------------------------

(defun test-thompson-atoms (assert-equal-fn assert-true-fn)
  (let* ((nfa-empty (build-nfa-from-pattern ""))
         (nfa-lit (build-nfa-from-pattern "a")))
    (funcall assert-equal-fn (nfa-states-count nfa-empty) 2 "паттерн '': ровно 2 состояния")
    (funcall assert-equal-fn (nfa-states-count nfa-lit) 2 "паттерн 'a': ровно 2 состояния")
    (funcall assert-true-fn (= (nfa-start-state nfa-lit) 0) "паттерн 'a': start-state = 0")
    (funcall assert-true-fn (= (nfa-accept-state nfa-lit) 1) "паттерн 'a': accept-state = 1")
  )
)

(defun test-thompson-concat (assert-equal-fn)
  (let ((nfa (build-nfa-from-pattern "ab")))
    ;; 'a' (2 состояния) + 'b' (2 состояния) = 4 состояния
    (funcall assert-equal-fn (nfa-states-count nfa) 4 "паттерн 'ab': 4 состояния (цепочка)")
    (funcall assert-equal-fn (nfa-start-state nfa) 0 "паттерн 'ab': начальное состояние 0")
    (funcall assert-equal-fn (nfa-accept-state nfa) 3 "паттерн 'ab': принимающее состояние 3")
  )
)

(defun test-thompson-alt (assert-equal-fn)
  (let ((nfa (build-nfa-from-pattern "a|b")))
    ;; 'a' (2) + 'b' (2) + 2 внешних состояния Томпсона (start/accept) = 6 состояний
    (funcall assert-equal-fn (nfa-states-count nfa) 6 "паттерн 'a|b': 6 состояний по Томпсону")
  )
)

(defun test-thompson-star (assert-equal-fn)
  (let ((nfa (build-nfa-from-pattern "a*")))
    ;; 'a' (2) + 2 внешних состояния Томпсона = 4 состояния
    (funcall assert-equal-fn (nfa-states-count nfa) 4 "паттерн 'a*': 4 состояния")
  )
)

(defun test-thompson-quantifiers (assert-equal-fn)
  (let ((nfa-q (build-nfa-from-pattern "a?"))
        (nfa-p (build-nfa-from-pattern "a+")))
    (funcall assert-equal-fn (nfa-states-count nfa-q) 4 "паттерн 'a?': 4 состояния")
    (funcall assert-equal-fn (nfa-states-count nfa-p) 4 "паттерн 'a+': 4 состояния")
  )
)

(defun test-thompson-char-classes (assert-equal-fn assert-true-fn)
  (let* ((ast (parse-regex "[a-z]"))
         (eq-table (make-equivalence-table-from-ast ast))
         (nfa (build-nfa-from-pattern "[a-z]")))
    (funcall assert-equal-fn (nfa-states-count nfa) 2 "паттерн '[a-z]': 2 состояния")
    ;; Проверяем, что Class ID для 'k' находится корректно через char-to-class-id
    (let ((cid (char-to-class-id #\k eq-table)))
      (funcall assert-true-fn (integerp cid) "char-to-class-id возвращает корректный fixnum ID")
    )
  )
)

(defun test-thompson-complex-expression (assert-equal-fn)
  (let ((nfa (build-nfa-from-pattern "(a|b)*c")))
    ;; (a|b) -> 6 состояний; (a|b)* -> 6 + 2 = 8 состояний; 'c' -> 2 состояния. Итого: 10 состояний.
    (funcall assert-equal-fn (nfa-states-count nfa) 10 "паттерн '(a|b)*c': 10 состояний")
  )
)

;; -----------------------------------------------------------------------------
;; Точка входа для запуска тестов
;; -----------------------------------------------------------------------------
(deftest run-nfa-thompson-tests "nfa/ast-to-nfa"
  (test-thompson-atoms #'assert-equal #'assert-true)
  (test-thompson-concat #'assert-equal)
  (test-thompson-alt #'assert-equal)
  (test-thompson-star #'assert-equal)
  (test-thompson-quantifiers #'assert-equal)
  (test-thompson-char-classes #'assert-equal #'assert-true)
  (test-thompson-complex-expression #'assert-equal)
)