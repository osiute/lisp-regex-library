(in-package :regex-library)

(deftest run-range-quantifier-tests "parser/range-quantifier"
  (test-valid-range-quantifiers #'assert-equal)
  (test-invalid-range-quantifiers #'assert-error))

;; Проверяет точный диапазон, полный диапазон и диапазон без верхней границы.
(defun test-valid-range-quantifiers (assert-equal-fn)
  (funcall assert-equal-fn
    (parse-range-quantifier
    (make-parser-state :str "{3}" :len 3))
    '(3 . 3)
    "точное число {3}")
  (funcall assert-equal-fn
    (parse-range-quantifier
    (make-parser-state :str "{2,5}" :len 5))
    '(2 . 5)
    "диапазон от и до {2,5}")
  (funcall assert-equal-fn
    (parse-range-quantifier
    (make-parser-state :str "{2,}" :len 4))
    '(2 . nil)
    "без верхней границы {2,}")
  (funcall assert-equal-fn
    (parse-range-quantifier
    (make-parser-state :str "{0,0}" :len 5))
    '(0 . 0)
    "нулевой квантификатор {0,0}")
  (funcall assert-equal-fn
    (parse-range-quantifier
    (make-parser-state :str "{0,}" :len 4))
    '(0 . nil)
    "от нуля до бесконечности {0,}")
)

(defun test-invalid-range-quantifiers (assert-error-fn)
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{5,2}" :len 5)))
    "ошибка: min больше max {5,2}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{2,5" :len 4)))
    "ошибка: незакрытая фигурная скобка {2,5")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{,5}" :len 4)))
    "ошибка: не указана нижняя граница {,5}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{}" :len 2)))
    "ошибка: пустой квантификатор {}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{,}" :len 3)))
    "ошибка: пустой квантификатор с запятой {,}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{a,10}" :len 6)))
    "ошибка: буква вместо первого числа {a,10}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{10,a}" :len 6)))
    "ошибка: буква вместо второго числа {10,a}")
  (funcall assert-error-fn
    (lambda ()
      (parse-range-quantifier
      (make-parser-state :str "{ ,10}" :len 6)))
    "ошибка: пробел перед запятой { ,10}")
)
