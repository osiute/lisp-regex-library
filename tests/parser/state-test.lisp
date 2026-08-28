(in-package :regex-library)

(deftest run-state-tests "parser/state"
  ;; Навигация корректно читает символы и возвращает nil за концом строки.
  (test-parser-state-navigation #'assert-equal)
  ;; Число считывается до первого нецифрового символа.
  (test-parser-state-number-parsing #'assert-equal)
  ;; Вызов parser-parse-number без цифр является синтаксической ошибкой.
  (test-parser-state-number-error #'assert-error))

;; Проверяет peek, next и match-p в начале, середине и конце строки.
(defun test-parser-state-navigation (assert-equal-fn)
  (let ((state (make-parser-state :str "abc" :len 3)))
    (funcall assert-equal-fn (parser-peek state) #\a "peek на первом символе")
    (funcall assert-equal-fn (parser-match-p state #\a) t
             "match-p совпадающего символа")
    (funcall assert-equal-fn (parser-peek state) #\b "peek после сдвига")
    (funcall assert-equal-fn (parser-next state) #\b
             "next считывает первый символ после сдвига")
    (funcall assert-equal-fn (parser-next state) #\c
             "next считывает последний символ")
    (funcall assert-equal-fn (parser-peek state) nil
             "peek за пределами границы строки")
    (funcall assert-equal-fn (parser-next state) nil
             "next за пределами границы строки")
    (funcall assert-equal-fn (parser-match-p state #\x) nil
             "match-p за пределами границы строки")))

;; Число 1234 не захватывает следующий символ 'a'.
(defun test-parser-state-number-parsing (assert-equal-fn)
  (let ((state (make-parser-state :str "1234abc" :len 7)))
    (funcall assert-equal-fn (parser-parse-number state) 1234
             "корректное считывание числа")
    (funcall assert-equal-fn (parser-peek state) #\a
             "указатель останавливается сразу за числом")))

(defun test-parser-state-number-error (assert-error-fn)
  (funcall assert-error-fn
           (lambda ()
             (parser-parse-number
              (make-parser-state :str "xyz" :len 3)))
           "вызов ошибки при отсутствии цифры"))
