;; Тут назначены макросы бойлерплейтного тест кода
(in-package :regex-library)

; =====================================================================
; Вспомогательные функции для deftest
; =====================================================================

;; Вспомогательная функция вывода результата одиночной проверки
(defun report-test-result (passed-p test-name fail-msg)
  "Печатает [OK] при успехе или [FAIL] с сообщением FAIL-MSG при ошибке."
  (if passed-p
      (format t "  [OK] ~A~%" test-name)
      (format t "  [FAIL] ~A: ~A~%" test-name fail-msg)
  )
  passed-p
)

;; Логика проверки на равенство объектов (equalp)
(defun test-assert-equal (actual expected test-name)
  "Проверяет равенство фактического и ожидаемого значений."
  (let ((passed-p (equalp actual expected))
        (msg (format nil "ожидалось ~S, получено ~S" expected actual)))
    (report-test-result passed-p test-name msg)
  )
)

;; Логика проверки на истинность значения (non-nil)
(defun test-assert-true (actual test-name)
  "Проверяет, что фактическое значение не является NIL."
  (let ((passed-p (not (null actual)))
        (msg (format nil "ожидалось истинное значение, получено ~S" actual)))
    (report-test-result passed-p test-name msg)
  )
)

;; Логика проверки перехвата исключения/ошибки
(defun test-assert-error (fn test-name)
  "Проверяет, что выполнение функции FN приводит к ошибке."
  (let ((error-caught nil))
    (handler-case (funcall fn)
      (error () (setf error-caught t)))
    (report-test-result error-caught
                        test-name
                        "ожидалась ошибка, но код выполнился")
  )
)

;; Печать финального отчёта по тестовому модулю
(defun print-test-summary (module-name passed failed)
  "Выводит итоговую статистику и возвращает T, если все тесты пройдены."
  (format t "~%=== Модуль ~A: тестирование завершено ===~%" module-name)
  (format t "Успешные: ~A~%Неудачные: ~A~%" passed failed)
  (= failed 0)
)

; =====================================================================
; Макросы для тестов
; =====================================================================

(defmacro deftest (name module-name &body body)
  "Определяет функцию тестирования с именем NAME для модуля MODULE-NAME.

  Особенности созданной функции:
  
  1) Имеет локальную функцию (assert-equal (actual expected test-name)), 
  которая проверяет на соответствие actual и expected для теста.
  Выводит результат проверки в терминал.

  2) Имеет локальную функцию (assert-true (actual test-name)),
  которая проверяет истинность actual.
  Выводит результат проверки в терминал.
  
  3) Имеет локальную функцию (assert-error (fn test-name)),
  которая проверяет на ошибку при вызове функци.
  Выводит результат проверки в терминал.

  4) В результате тестирования выводит в терминал количество 
  успешных и неудачных проверок. 
  "
  (let ((passed-sym (gensym "PASSED-"))
          (failed-sym (gensym "FAILED-")))
    `(defun ,name ()
      (let ((,passed-sym 0)
            (,failed-sym 0))
        (format t "~%=== Модуль ~A: начало тестирования ===~%" ,module-name)
        ;; Локальные функции проверок, проброшенные через flet
        (flet ((assert-equal (actual expected test-name)
                (if (test-assert-equal actual expected test-name)
                    (incf ,passed-sym)
                    (incf ,failed-sym)))
              (assert-true (actual test-name)
                (if (test-assert-true actual test-name)
                    (incf ,passed-sym)
                    (incf ,failed-sym)))
              (assert-error (fn test-name)
                (if (test-assert-error fn test-name)
                    (incf ,passed-sym)
                    (incf ,failed-sym))))
          ,@body
        )
        
        (print-test-summary ,module-name ,passed-sym ,failed-sym)
      )
    )
  )
)

; =====================================================================
; Вызов групп тестовых функций
; =====================================================================
(defun run-all-tests ()
  (format t "~%=== Запуск всех тестов regex-library ===~%~%")
  (let ((all-succeed-p 
        (and
          (run-state-tests)
          (run-char-class-tests)
          (run-range-quantifier-tests)
          (run-grammar-atoms-tests)
          (run-grammar-quantifier-tests)
          (run-grammar-concatenation-tests)
          (run-grammar-expression-tests)
          (run-endpoints-collector-tests)
          (run-endpoints-converter-tests)
          (run-unicode-tests)
          (run-char-class-to-class-ids-tests)
          (run-nfa-thompson-tests)
          (run-nfa-closure-tests)
          (run-nfa-reverse-tests)
          (run-state-registry-tests)
          (run-start-states-tests)
          (run-cache-tests)
          (run-dfa-tests)
          (run-contains-p-tests)
          (run-matches-p-tests)
          (run-first-match-span-tests)
          (run-all-match-spans-tests)
          (run-split-tests)
          (run-replace-all-tests)
          (run-count-disjoint-matches-tests)
          (run-ppcre-war-and-peace-suite)
          (run-ppcre-pathological-suite)
        )))
    (if all-succeed-p
      (progn 
        (format t "~%=== Все тесты были успешно исполнены ===~%~%")
        t)
      (progn
        (format t "~%=== Не все тесты отработали корректно ===~%~%")
        nil)
    )
  )
)