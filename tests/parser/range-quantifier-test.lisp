(in-package :regex-library)

(deftest run-range-quantifier-tests "parser/range-quantifier"
  ;; --- Позитивные тесты ---
  (let ((s (make-parser-state :str "{3}" :len 3)))
    (assert-equal (parse-range-quantifier s) '(3 . 3) "точное число {3}")
  )

  (let ((s (make-parser-state :str "{2,5}" :len 5)))
    (assert-equal (parse-range-quantifier s) '(2 . 5) "диапазон от и до {2,5}")
  )

  (let ((s (make-parser-state :str "{2,}" :len 4)))
    (assert-equal (parse-range-quantifier s) '(2 . nil) "без верхней границы {2,}")
  )

  (let ((s (make-parser-state :str "{,5}" :len 4)))
    (assert-equal (parse-range-quantifier s) '(0 . 5) "без нижней границы {,5}")
  )

  ;; --- Негативные тесты (Проверка обработки ошибок) ---
  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{5,2}" :len 5)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: min больше max {5,2}")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{2,5" :len 4)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: незакрытая фигурная скобка {2,5")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{}" :len 2)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: пустой диапазон {}")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{a,10}" :len 6)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: буква вместо первого числа {a,10}")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{10,a}" :len 6)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: буква вместо второго числа {10,a}")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "{ ,10}" :len 6)))
                    (parse-range-quantifier s)
                  )
                )
                "ошибка: пробел перед запятой { ,10}")
)