(in-package :regex-library)

(deftest run-grammar-atoms-tests "parser/grammar (parse-atoms)"
  ;; Атомы включают литералы, якоря, точку, классы и группы.
  (test-grammar-basic-atoms #'assert-true #'assert-equal)
  ;; Квантификатор и незакрытая группа без корректного атома дают ошибку.
  (test-grammar-atom-errors #'assert-error))

;; Проверяет типы AST и ключевые значения, создаваемые для атомов.
(defun test-grammar-basic-atoms (assert-true-fn assert-equal-fn)
  (let ((ast (parse-regex "a")))
    (funcall assert-true-fn (ast-literal-p ast) "литерал 'a'")
    (funcall assert-equal-fn (ast-literal-char ast) #\a "код символа 'a'"))
  (let ((ast (parse-regex "^")))
    (funcall assert-true-fn (ast-anchor-p ast) "якорь ^")
    (funcall assert-equal-fn (ast-anchor-type ast) :start-of-line
             "тип start-of-line"))
  (let ((ast (parse-regex "$")))
    (funcall assert-true-fn (ast-anchor-p ast) "якорь $")
    (funcall assert-equal-fn (ast-anchor-type ast) :end-of-line
             "тип end-of-line"))
  (let ((ast (parse-regex ".")))
    (funcall assert-true-fn (ast-char-class-p ast) "точка .")
    (funcall assert-equal-fn (ast-char-class-ranges ast) +dot-all-chars+
             "диапазон точки")
    (format t "    Диапазоны точки: ~S~%" (ast-char-class-ranges ast)))
  (let ((ast (parse-regex "[a-z]")))
    (funcall assert-true-fn (ast-char-class-p ast) "символьный класс [a-z]"))
  (let ((ast (parse-regex "\\d")))
    (funcall assert-true-fn (ast-char-class-p ast)
             "экранированный класс \\d"))
  (let ((ast (parse-regex "(a)")))
    (funcall assert-true-fn (ast-literal-p ast)
             "скобки (a) возвращают внутренний литерал")))

(defun test-grammar-atom-errors (assert-error-fn)
  (funcall assert-error-fn
           (lambda () (parse-regex "*"))
           "ошибка: квантификатор * без атома")
  (funcall assert-error-fn
           (lambda () (parse-regex "(a"))
           "ошибка: незакрытая скобка (a"))

(deftest run-grammar-quantifier-tests "parser/grammar (parse-quantifier)"
  (test-grammar-quantifiers #'assert-true #'assert-equal)
  (test-grammar-quantifier-errors #'assert-error))

;; Проверяет все поддерживаемые квантификаторы и вложенные группы.
(defun test-grammar-quantifiers (assert-true-fn assert-equal-fn)
  (let ((ast (parse-regex "a*")))
    (funcall assert-true-fn (ast-star-p ast) "узел ast-star")
    (funcall assert-true-fn (ast-literal-p (ast-star-child ast))
             "дочерний узел - литерал"))
  (let ((ast (parse-regex "b+")))
    (funcall assert-true-fn (ast-plus-p ast) "узел ast-plus")
    (funcall assert-true-fn (ast-literal-p (ast-plus-child ast))
             "дочерний узел - литерал"))
  (let ((ast (parse-regex "c?")))
    (funcall assert-true-fn (ast-question-p ast) "узел ast-question")
    (funcall assert-true-fn (ast-literal-p (ast-question-child ast))
             "дочерний узел - литерал"))
  (let ((ast (parse-regex "[0-9]{2,4}")))
    (funcall assert-true-fn (ast-range-p ast) "узел ast-range")
    (funcall assert-equal-fn (ast-range-min ast) 2 "минимум диапазона = 2")
    (funcall assert-equal-fn (ast-range-max ast) 4 "максимум диапазона = 4")
    (funcall assert-true-fn (ast-char-class-p (ast-range-child ast))
             "дочерний узел - символьный класс"))
  (let ((ast (parse-regex "(a)*")))
    (funcall assert-true-fn (ast-star-p ast) "звезда над группой")
    (funcall assert-true-fn (ast-literal-p (ast-star-child ast))
             "внутри звезды литерал")))

(defun test-grammar-quantifier-errors (assert-error-fn)
  ;; Ошибки отсутствия атома.
  (dolist (regex '("*" "+" "?" "{1,2}" "(a"))
    (funcall assert-error-fn
             (lambda () (parse-regex regex))
             (format nil "ошибка: недопустимый квантификатор ~A" regex)))
  ;; Якоря нельзя квантифицировать.
  (funcall assert-error-fn (lambda () (parse-regex "^*"))
           "ошибка: квантификация ^* запрещена")
  (funcall assert-error-fn (lambda () (parse-regex "$+"))
           "ошибка: квантификация $+ запрещена")
  (funcall assert-error-fn (lambda () (parse-regex "^{2}"))
           "ошибка: квантификация ^{2} запрещена"))

(deftest run-grammar-concatenation-tests "parser/grammar (parse-concatenation)"
  ;; Конкатенация сохраняет порядок элементов выражения.
  (test-grammar-concatenation #'assert-true #'assert-equal))

(defun test-grammar-concatenation (assert-true-fn assert-equal-fn)
  (let ((ast (parse-regex "ab")))
    (funcall assert-true-fn (ast-concat-p ast) "узел ast-concat для 'ab'")
    (funcall assert-equal-fn (length (ast-concat-elements ast)) 2
             "длина списка elements = 2"))
  (let ((ast (parse-regex "^a*b$")))
    (funcall assert-true-fn (ast-concat-p ast)
             "узел ast-concat для '^a*b$'")
    (funcall assert-equal-fn (length (ast-concat-elements ast)) 4
             "4 элемента в конкатенации")
    (funcall assert-true-fn (ast-anchor-p (first (ast-concat-elements ast)))
             "первый элемент - якорь ^")
    (funcall assert-true-fn (ast-star-p (second (ast-concat-elements ast)))
             "второй элемент - звезда a*")
    (funcall assert-true-fn (ast-anchor-p (fourth (ast-concat-elements ast)))
             "четвертый элемент - якорь $"))
  (let ((ast (parse-regex "((((a))))")))
    (funcall assert-true-fn (ast-literal-p ast)
             "((((a)))) схлопывается в единичный литерал")))

(deftest run-grammar-expression-tests "parser/grammar (parse-expression)"
  (test-grammar-alternations #'assert-true #'assert-equal)
  (test-grammar-empty-expressions #'assert-equal))

(defun test-grammar-alternations (assert-true-fn assert-equal-fn)
  (let ((ast (parse-regex "a|b")))
    (funcall assert-true-fn (ast-alt-p ast) "узел ast-alt для 'a|b'")
    (funcall assert-equal-fn (ast-literal-char (ast-alt-left ast)) #\a
             "левая ветвь 'a'")
    (funcall assert-equal-fn (ast-literal-char (ast-alt-right ast)) #\b
             "правая ветвь 'b'"))
  (let ((ast (parse-regex "a|b|c")))
    (funcall assert-true-fn (ast-alt-p ast) "внешний ast-alt")
    (funcall assert-true-fn (ast-alt-p (ast-alt-right ast))
             "вложенный ast-alt справа"))
  (let ((ast (parse-regex "^(a|b)*$")))
    (funcall assert-true-fn (ast-concat-p ast) "корень - ast-concat")
    (let ((star-node (second (ast-concat-elements ast))))
      (funcall assert-true-fn (ast-star-p star-node) "второй элемент - звезда")
      (funcall assert-true-fn (ast-alt-p (ast-star-child star-node))
               "внутри звезды - альтернация (a|b)"))))

;; Пустые ветви и пустые группы превращаются в ast-empty.
(defun test-grammar-empty-expressions (assert-equal-fn)
  (dolist (test-case '(("()" . "парсинг () дает ast-empty")
                       ("|" . "парсинг одиночного | дает ast-empty")
                       ("(|)" . "парсинг (|) дает ast-empty")
                       ("((()))" . "парсинг ((())) схлопывается в ast-empty")))
    (let ((node (parse-expression
                 (make-parser-state :str (car test-case)
                                    :len (length (car test-case))))))
      (funcall assert-equal-fn (type-of node) 'ast-empty (cdr test-case))))
  (let ((state (make-parser-state :str "a|" :len 2)))
    (let ((node (parse-expression state)))
      (funcall assert-equal-fn (type-of node) 'ast-alt
               "парсинг a| дает ast-alt")
      (funcall assert-equal-fn (ast-literal-char (ast-alt-left node)) #\a
               "левая ветвь literal 'a'")
      (funcall assert-equal-fn (type-of (ast-alt-right node)) 'ast-empty
               "правая ветвь ast-empty")))
  (let ((state (make-parser-state :str "|b" :len 2)))
    (let ((node (parse-expression state)))
      (funcall assert-equal-fn (type-of node) 'ast-alt
               "парсинг |b дает ast-alt")
      (funcall assert-equal-fn (type-of (ast-alt-left node)) 'ast-empty
               "левая ветвь ast-empty")
      (funcall assert-equal-fn (ast-literal-char (ast-alt-right node)) #\b
               "правая ветвь literal 'b'"))))
