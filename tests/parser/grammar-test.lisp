(in-package :regex-library)

(deftest run-grammar-atoms-tests "parser/grammar"
  ;; --- Позитивные тесты атомов ---
  (let ((ast (parse-regex "a")))
    (assert-equal (ast-literal-p ast) t "литерал 'a'")
    (assert-equal (ast-literal-code ast) (char-code #\a) "код символа 'a'"))

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