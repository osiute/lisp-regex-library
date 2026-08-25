(in-package :regex-library)

(defun run-state-tests ()
  (format t "~%=== Запуск тестов модуля parser-state ===~%")
  (let ((passed 0)
        (failed 0))
    
    (flet ((assert-equal (expected actual test-name)
             (if (equal expected actual)
                 (progn
                   (incf passed)
                   (format t "  [OK] ~A~%" test-name))
                 (progn
                   (incf failed)
                   (format t "  [FAIL] ~A: ожидалось ~S, получено ~S~%" test-name expected actual)))))

      ;; --- Тест 1: Базовый проход и навигация курсора ---
      (let ((s (make-parser-state :str "abc" :len 3)))
        (assert-equal #\a (parser-peek s) "peek на первом символе")
        (assert-equal t (parser-match-p s #\a) "match-p совпадающего символа")
        (assert-equal #\b (parser-peek s) "peek после сдвига")
        (assert-equal #\b (parser-next s) "next считывает символ и двигает индекс")
        (assert-equal #\c (parser-next s) "next считывает последний символ")
        (assert-equal nil (parser-peek s) "peek за пределами границы строки")
        (assert-equal nil (parser-next s) "next за пределами границы строки")
        (assert-equal nil (parser-match-p s #\x) "match-p за пределами границы строки"))

      ;; --- Тест 2: Считывание положительных чисел ---
      (let ((s (make-parser-state :str "1234abc" :len 7)))
        (assert-equal 1234 (parser-parse-number s) "корректное считывание числа")
        (assert-equal #\a (parser-peek s) "указатель останавливается сразу за числом"))

      ;; --- Тест 3: Обработка синтаксической ошибки при парсинге числа ---
      (let ((s (make-parser-state :str "xyz" :len 3))
            (error-caught nil))
        (handler-case (parser-parse-number s)
          (error () (setf error-caught t)))
        (assert-equal t error-caught "вызов ошибки при отсутствии цифры")))

    (format t "=== Итог: Успешно: ~A | Ошибок: ~A ===~%" passed failed)
    (= failed 0)))