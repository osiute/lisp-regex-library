(in-package :regex-library)

(deftest run-range-quantifier-tests "parser/range-quantifier"
  ;; Положительные сценарии покрывают все поддерживаемые формы квантификатора.
  (test-valid-range-quantifiers #'assert-equal)
  ;; Некорректные границы, числа и закрывающая скобка должны давать ошибку.
  (test-invalid-range-quantifiers #'assert-error))

;; Проверяет точный диапазон, открытый диапазон и каждую из неполных форм.
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
            (make-parser-state :str "{,5}" :len 4))
           '(0 . 5)
           "без нижней границы {,5}"))

;; Ошибочные формы проверяются через общий assert-error, переданный из deftest.
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
              (make-parser-state :str "{}" :len 2)))
           "ошибка: пустой диапазон {}")
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
           "ошибка: пробел перед запятой { ,10}"))
