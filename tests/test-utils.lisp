;; Тут назначены макросы бойлерплейтного тест кода
(in-package :regex-library)

(defmacro deftest (name module-name &body body)
  "Определяет функцию тестирования с именем NAME для модуля MODULE-NAME.

  Особенности созданной функции:

  1) Имеет локальные счётчики failed и passed,
  которые выводятся в терминал после завершения тестирования.
  
  2) Имеет локальную функцию (assert-equal (actual expected test-name)), 
  которая проверяет на соответствие actual и expected для теста;
  инкрементирует passed, если actual и expected равны — иначе инкрементирует failed.
  Выводит результат проверки в терминал.
  
  3) Имеет локальную функцию (assert-error (fn test-name)),
  которая проверяет на ошибку при вызове функци;
  инкрементирует passed, если поймали ошибку — иначе инкрементирует failed.
  Выводит результат проверки в терминал.
  "
  `(defun ,name ()
     (let ((passed 0)
           (failed 0))
       (format t "~%=== Модуль ~A: начало тестирования ===~%" ,module-name)
       (flet ((assert-equal (actual expected test-name)
                (if (equal actual expected)
                    (progn
                      (incf passed)
                      (format t "  [OK] ~A~%" test-name))
                    (progn
                      (incf failed)
                      (format t "  [FAIL] ~A: ожидалось ~S, получено ~S~%" test-name expected actual))))
              (assert-error (fn test-name)
                (let ((error-caught nil))
                  (handler-case (funcall fn)
                    (error () (setf error-caught t)))
                  (if error-caught
                      (progn
                        (incf passed)
                        (format t "  [OK] ~A~%" test-name))
                      (progn
                        (incf failed)
                        (format t "  [FAIL] ~A: ожидалась ошибка, но код выполнился~%" test-name))))))
         ,@body)
       (format t "~%=== Модуль ~A: тестирование завершено ===~%" ,module-name)
       (format t "Успешные: ~A~%Неудачные: ~A~%" passed failed)
       (= failed 0))))