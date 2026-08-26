(in-package :regex-library)

(deftest run-state-tests "parser/state"
  ;; --- Тест 1: Базовый проход и навигация курсора ---
  (let ((s (make-parser-state :str "abc" :len 3)))
    (assert-equal (parser-peek s) #\a "peek на первом символе")
    (assert-equal (parser-match-p s #\a) t "match-p совпадающего символа")
    (assert-equal (parser-peek s) #\b "peek после сдвига")
    (assert-equal (parser-next s) #\b "next считывает символ и двигает индекс")
    (assert-equal (parser-next s) #\c "next считывает последний символ")
    (assert-equal (parser-peek s) nil "peek за пределами границы строки")
    (assert-equal (parser-next s) nil "next за пределами границы строки")
    (assert-equal (parser-match-p s #\x) nil "match-p за пределами границы строки")
  )

  ;; --- Тест 2: Считывание положительных чисел ---
  (let ((s (make-parser-state :str "1234abc" :len 7)))
    (assert-equal (parser-parse-number s) 1234 "корректное считывание числа")
    (assert-equal (parser-peek s) #\a "указатель останавливается сразу за числом")
  )

  ;; --- Тест 3: Обработка синтаксической ошибки при парсинге числа ---
  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "xyz" :len 3)))
                    (parser-parse-number s)
                  )
                )
                "вызов ошибки при отсутствии цифры")
)