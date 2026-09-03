(in-package :regex-library)

;; -----------------------------------------------------------------------------
;; Вспомогательные функции для тестирования
;; -----------------------------------------------------------------------------

;; Парсит выражение и возвращает корневой ast-node (для одиночного класса)
(defun get-first-ast-char-class (pattern)
  (let ((ast (parse-regex pattern)))
    (if (typep ast 'ast-concat)
        (first (ast-concat-elements ast))
        ast
    )
  )
)

;; Cобирает class-ids по паттерну
(defun get-class-ids-for-pattern (pattern)
  (let* ((ast (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast))
         (node (get-first-ast-char-class pattern)))
    (char-class-to-class-ids node eq-table)
  )
)

;; -----------------------------------------------------------------------------
;; Тестовые подфункции
;; -----------------------------------------------------------------------------

;; Простой диапазон [a-z]
(defun test-char-class-simple-range (assert-equal-fn assert-true-fn)
  (let* ((ids (get-class-ids-for-pattern "[a-z]")))
    (funcall assert-true-fn (listp ids) "[a-z]: возвращает список")
    ;; Так как кроме [a-z] других диапазонов в AST нет, [a-z] занимает 1 класс эквивалентности
    (funcall assert-equal-fn (length ids) 1 "[a-z]: ровно 1 класс эквивалентности в своем контексте")
  )
)

;; Несколько диапазонов [a-z0-9]
(defun test-char-class-multi-range (assert-equal-fn)
  (let* ((pattern "[a-z0-9]")
         (ast (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast))
         (node (get-first-ast-char-class pattern))
         (ids (char-class-to-class-ids node eq-table)))
    ;; 'a'-'z' и '0'-'9' разбивают Юникод на непересекающиеся классы, всего их 2
    (funcall assert-equal-fn (length ids) 2 "[a-z0-9]: покрывает 2 непересекающихся класса")
  )
)

;; Отрицание [^a]
(defun test-char-class-negated (assert-equal-fn assert-true-fn)
  (let* ((pattern "[^a]")
         (ast (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast))
         (node (get-first-ast-char-class pattern))
         (pos-node (make-ast-char-class :ranges (list (cons #\a #\a)) :negated-p nil))
         (pos-ids (char-class-to-class-ids pos-node eq-table))
         (neg-ids (char-class-to-class-ids node eq-table))
         (total-classes (equivalence-table-num-classes eq-table)))
    (funcall assert-equal-fn (+ (length pos-ids) (length neg-ids)) total-classes
             "[^a]: сумма прямого и инвертированного списков равна общему числу классов")
    (funcall assert-true-fn (not (intersection pos-ids neg-ids))
             "[^a]: прямой и инвертированный списки не пересекаются")
  )
)

;; Перекрывающиеся диапазоны (a-e), (c-z)
(defun test-char-class-overlapping-ranges (assert-equal-fn)
  (let* ((pattern "[a-ec-z]")
         (ast (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast))
         (node (get-first-ast-char-class pattern))
         (ids (char-class-to-class-ids node eq-table))
         ;; Проверяем класс напрямую для символов 'b' (попадает в a-e) и 'k' (попадает в c-z)
         (id-b (char-to-class-id #\b eq-table))
         (id-k (char-to-class-id #\k eq-table)))
    ;; Проверяем, что и ID для 'b', и ID для 'k' присутствуют в результате
    (funcall assert-equal-fn (length ids) 3 "[a-ec-z]: покрывает все 3 сформированных класса")
    (funcall assert-equal-fn (null (member id-b ids)) nil "[a-ec-z]: содержит класс для 'b'")
    (funcall assert-equal-fn (null (member id-k ids)) nil "[a-ec-z]: содержит класс для 'k'")
  )
)

;; -----------------------------------------------------------------------------
;; Точка входа для запуска тестов символьного класса
;; -----------------------------------------------------------------------------
(deftest run-char-class-to-class-ids-tests "unicode/char-class-to-class-ids"
  (test-char-class-simple-range #'assert-equal #'assert-true)
  (test-char-class-multi-range #'assert-equal)
  (test-char-class-negated #'assert-equal #'assert-true)
  (test-char-class-overlapping-ranges #'assert-equal)
)