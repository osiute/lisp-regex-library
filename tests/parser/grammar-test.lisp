(in-package :regex-library)

(deftest run-grammar-atoms-tests "parser/grammar (parse-atoms)"
  ;; --- Позитивные тесты атомов ---
  (let ((ast (parse-regex "a")))
    (assert-equal (ast-literal-p ast) t "литерал 'a'")
    (assert-equal (ast-literal-char ast) #\a "код символа 'a'"))

  (let ((ast (parse-regex "^")))
    (assert-equal (ast-anchor-p ast) t "якорь ^")
    (assert-equal (ast-anchor-type ast) :start-of-line "тип start-of-line"))

  (let ((ast (parse-regex "$")))
    (assert-equal (ast-anchor-p ast) t "якорь $")
    (assert-equal (ast-anchor-type ast) :end-of-line "тип end-of-line"))

  (let ((ast (parse-regex ".")))
    (assert-equal (ast-char-class-p ast) t "точка .")
    (assert-equal (ast-char-class-ranges ast) +dot-all-chars+ "диапазон точки")
    (format t "    Диапазоны точки: ~S~%" (ast-char-class-ranges ast))
  )

  (let ((ast (parse-regex "[a-z]")))
    (assert-equal (ast-char-class-p ast) t "символьный класс [a-z]"))

  (let ((ast (parse-regex "\\d")))
    (assert-equal (ast-char-class-p ast) t "экранированный класс \\d"))

  (let ((ast (parse-regex "(a)")))
    (assert-equal (ast-literal-p ast) t "скобки (a) возвращают внутренний литерал"))

  ;; --- Негативные тесты ---
  (assert-error (lambda () (parse-regex "*")) "ошибка: квантификатор * без атома")
  (assert-error (lambda () (parse-regex "(a")) "ошибка: незакрытая скобка (a"))


(deftest run-grammar-quantifier-tests "parser/grammar (parse-quantifier)"
  ;; Квантификатор *
  (let ((ast (parse-regex "a*")))
    (assert-equal (ast-star-p ast) t "узел ast-star")
    (assert-equal (ast-literal-p (ast-star-child ast)) t "дочерний узел - литерал"))

  ;; Квантификатор +
  (let ((ast (parse-regex "b+")))
    (assert-equal (ast-plus-p ast) t "узел ast-plus")
    (assert-equal (ast-literal-p (ast-plus-child ast)) t "дочерний узел - литерал"))

  ;; Квантификатор ?
  (let ((ast (parse-regex "c?")))
    (assert-equal (ast-question-p ast) t "узел ast-question")
    (assert-equal (ast-literal-p (ast-question-child ast)) t "дочерний узел - литерал"))

  ;; Диапазонный квантификатор {2,4}
  (let ((ast (parse-regex "[0-9]{2,4}")))
    (assert-equal (ast-range-p ast) t "узел ast-range")
    (assert-equal (ast-range-min ast) 2 "минимум диапазона = 2")
    (assert-equal (ast-range-max ast) 4 "максимум диапазона = 4")
    (assert-equal (ast-char-class-p (ast-range-child ast)) t "дочерний узел - символьный класс"))

  ;; Квантификатор над группой (a)*
  (let ((ast (parse-regex "(a)*")))
    (assert-equal (ast-star-p ast) t "звезда над группой")
    (assert-equal (ast-literal-p (ast-star-child ast)) t "внутри группы литерал"))

  ;; --- 3. Негативные тесты ---
  
  ;; Ошибки отсутствия атома
  (assert-error (lambda () (parse-regex "*")) "ошибка: квантификатор * без атома")
  (assert-error (lambda () (parse-regex "+")) "ошибка: квантификатор + без атома")
  (assert-error (lambda () (parse-regex "?")) "ошибка: квантификатор ? без атома")
  (assert-error (lambda () (parse-regex "{1,2}")) "ошибка: квантификатор {1,2} без атома")
  (assert-error (lambda () (parse-regex "(a")) "ошибка: незакрытая скобка (a")

  ;; Ошибки квантификации якорей
  (assert-error (lambda () (parse-regex "^*")) "ошибка: квантификация ^* запрещена")
  (assert-error (lambda () (parse-regex "$+")) "ошибка: квантификация $+ запрещена")
  (assert-error (lambda () (parse-regex "^{2}")) "ошибка: квантификация ^{2} запрещена"))

(deftest run-grammar-concatenation-tests "parser/grammar (parse-concatenation)"
  ;; Последовательность литералов "ab"
  (let ((ast (parse-regex "ab")))
    (assert-equal (ast-concat-p ast) t "узел ast-concat для 'ab'")
    (assert-equal (length (ast-concat-elements ast)) 2 "длина списка elements = 2"))

  ;; Выражение с якорями и квантификаторами "^a*b$"
  (let ((ast (parse-regex "^a*b$")))
    (assert-equal (ast-concat-p ast) t "узел ast-concat для '^a*b$'")
    (assert-equal (length (ast-concat-elements ast)) 4 "4 элемента в конкатенации")
    (assert-equal (ast-anchor-p (first (ast-concat-elements ast))) t "первый элемент - якорь ^")
    (assert-equal (ast-star-p (second (ast-concat-elements ast))) t "второй элемент - звезда a*")
    (assert-equal (ast-anchor-p (fourth (ast-concat-elements ast))) t "четвертый элемент - якорь $"))

  ;; Глубокая вложенность скобок без создания concat
  (let ((ast (parse-regex "((((a))))")))
    (assert-equal (ast-literal-p ast) t "((((a)))) схлопывается в единичный литерал"))
)
(deftest run-grammar-expression-tests "parser/grammar (parse-expression)"
  ;; Простая альтернация "a|b"
  (let ((ast (parse-regex "a|b")))
    (assert-equal (ast-alt-p ast) t "узел ast-alt для 'a|b'")
    (assert-equal (ast-literal-char (ast-alt-left ast)) #\a "левая ветвь 'a'")
    (assert-equal (ast-literal-char (ast-alt-right ast)) #\b "правая ветвь 'b'"))

  ;; Множественная альтернация "a|b|c"
  (let ((ast (parse-regex "a|b|c")))
    (assert-equal (ast-alt-p ast) t "внешний ast-alt")
    (assert-equal (ast-alt-p (ast-alt-right ast)) t "вложенный ast-alt справа"))

  ;; Альтернация внутри групп с конкатенацией "^(a|b)*$"
  (let ((ast (parse-regex "^(a|b)*$")))
    (assert-equal (ast-concat-p ast) t "корень - ast-concat")
    (let ((star-node (second (ast-concat-elements ast))))
      (assert-equal (ast-star-p star-node) t "второй элемент - звезда")
      (assert-equal (ast-alt-p (ast-star-child star-node)) t "внутри звезды - альтернация (a|b)")))
    ;; --- Тест: Пустые скобки () ---
  (let ((s (make-parser-state :str "()" :len 2)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-empty "парсинг () дает ast-empty")
    )
  )

  ;; --- Тест: Одиночная альтернация | ---
  (let ((s (make-parser-state :str "|" :len 1)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-empty "парсинг одиночного | дает ast-empty")
    )
  )

  ;; --- Тест: Альтернация с пустой правой ветвью a| ---
  (let ((s (make-parser-state :str "a|" :len 2)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-alt "парсинг a| дает ast-alt")
      (assert-equal (ast-literal-char (ast-alt-left node)) #\a "левая ветвь literal 'a'")
      (assert-equal (type-of (ast-alt-right node)) 'ast-empty "правая ветвь ast-empty")
    )
  )

  ;; --- Тест: Альтернация с пустой левой ветвью |b ---
  (let ((s (make-parser-state :str "|b" :len 2)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-alt "парсинг |b дает ast-alt")
      (assert-equal (type-of (ast-alt-left node)) 'ast-empty "левая ветвь ast-empty")
      (assert-equal (ast-literal-char (ast-alt-right node)) #\b "правая ветвь literal 'b'")
    )
  )

  ;; --- Тест: Альтернация в скобках (|) ---
  (let ((s (make-parser-state :str "(|)" :len 3)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-empty "парсинг (|) дает ast-empty")
    )
  )

  ;; --- Тест: Глубокая вложенность пустых скобок ((())) ---
  (let ((s (make-parser-state :str "((()))" :len 6)))
    (let ((node (parse-expression s)))
      (assert-equal (type-of node) 'ast-empty "парсинг ((())) схлопывается в ast-empty")
    )
  )
)