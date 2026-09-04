;; Тут назначены макросы бойлерплейтного тест кода
(in-package :regex-library)

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
  (let ((passed-sym (gensym)) 
        (failed-sym (gensym)))
  `(defun ,name ()
     (let ((,passed-sym 0)
           (,failed-sym 0))
       (format t "~%=== Модуль ~A: начало тестирования ===~%" ,module-name)
       (flet ((assert-equal (actual expected test-name)
                (if (equalp actual expected)
                    (progn
                      (incf ,passed-sym)
                      (format t "  [OK] ~A~%" test-name))
                    (progn
                      (incf ,failed-sym)
                      (format t "  [FAIL] ~A: ожидалось ~S, получено ~S~%" test-name expected actual))))
              (assert-true (actual test-name)
                (if actual
                    (progn
                      (incf ,passed-sym)
                      (format t "  [OK] ~A~%" test-name))
                    (progn
                      (incf ,failed-sym)
                      (format t "  [FAIL] ~A: ожидалось истинное значение, получено ~S~%"
                             test-name actual))))
              (assert-error (fn test-name)
                (let ((error-caught nil))
                  (handler-case (funcall fn)
                    (error () (setf error-caught t)))
                  (if error-caught
                      (progn
                        (incf ,passed-sym)
                        (format t "  [OK] ~A~%" test-name))
                      (progn
                        (incf ,failed-sym)
                        (format t "  [FAIL] ~A: ожидалась ошибка, но код выполнился~%" test-name))))))
          
         ,@body)
       (format t "~%=== Модуль ~A: тестирование завершено ===~%" ,module-name)
       (format t "Успешные: ~A~%Неудачные: ~A~%" ,passed-sym ,failed-sym)
       (= ,failed-sym 0)))
  )
)

(defun run-all-tests ()
  (format t "~%=== Запуск всех тестов regex-library ===~%~%")
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
  (format t "~%=== Все тесты были исполнены ===~%~%")
)

