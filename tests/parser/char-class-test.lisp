(in-package :regex-library)

(defun run-char-class-tests ()
  (let ((passed 0) (failed 0))
    (format t "~%=== Модуль parser/char-class: начало тестирования ===~%")
    (flet (
      (assert-equal (actual expected test-name)
        (when (equal actual expected)
          (incf passed)
          (format t "  [OK] ~A~%" test-name)
          (return-from assert-equal)
        )
        (incf failed)
        (format t "  [FAIL] ~A: ожидалось ~S, получено ~S~%" test-name expected actual)
      )
      (assert-error (fn test-name)
        (let ((error-caught nil))
          (handler-case (funcall fn)
            (error () (setf error-caught t))
          )
          (if error-caught
            (progn
              (incf passed)
              (format t "  [OK] ~A~%" test-name)
            )
            (progn
              (incf failed)
              (format t "  [FAIL] ~A: ожидалась ошибка, но код выполнился~%" test-name)
            )
          )
        )
      )
      )

      ;; --- Тест 1: Экранированные спецклассы \d, \w ---
      (let ((s (make-parser-state :str "\\d" :len 2)))
        (let ((node (parse-escape-char-class s)))
          (assert-equal '((#\0 . #\9)) (ast-char-class-ranges node) "\\d дает диапазон 0-9")
          (assert-equal nil (ast-char-class-negated-p node) "\\d не отрицательный")))

      (let ((s (make-parser-state :str "\\w" :len 2)))
        (let ((node (parse-escape-char-class s)))
          (assert-equal '((#\a . #\z) (#\A . #\Z) (#\0 . #\9) (#\_ . #\_)) (ast-char-class-ranges node) 
          "\\w дает диапазоны (a-z, A-Z, 0-9, _)")
          (assert-equal nil (ast-char-class-negated-p node) "\\w не отрицательный")))

      ;; --- Тест 2: Скобочные группы с диапазонами [a-z0-9] ---
      (let ((s (make-parser-state :str "[a-z0-9]" :len 8)))
        (let ((node (parse-bracket-char-class s)))
          (assert-equal '((#\a . #\z) (#\0 . #\9)) (ast-char-class-ranges node) "парсинг диапазонов a-z и 0-9")
          (assert-equal nil (ast-char-class-negated-p node) "обычный класс без отрицания")))

      ;; --- Тест 3: Отрицание в скобках [^abc] ---
      (let ((s (make-parser-state :str "[^abc]" :len 6)))
        (let ((node (parse-bracket-char-class s)))
          (assert-equal '((#\a . #\a) (#\b . #\b) (#\c . #\c)) (ast-char-class-ranges node) "парсинг отдельных символов a, b, c")
          (assert-equal t (ast-char-class-negated-p node) "установлен флаг negated-p")))

      ;; --- Неправильные написания класса (Проверка обработки ошибок) ---
      (assert-error (lambda ()
                      (let ((s (make-parser-state :str "[z-a]" :len 5)))
                        (parse-bracket-char-class s)))
                    "ошибка: перевернутый диапазон [z-a]")

      (assert-error (lambda ()
                      (let ((s (make-parser-state :str "[a-z" :len 4)))
                        (parse-bracket-char-class s)))
                    "ошибка: незакрытая квадратная скобка [a-z")

      (assert-error (lambda ()
                      (let ((s (make-parser-state :str "\\" :len 1)))
                        (parse-escape-char-class s)))
                    "ошибка: обрывающаяся escape-последовательность \\")

      (assert-error (lambda ()
                      (let ((s (make-parser-state :str "[a-]" :len 4)))
                        (parse-bracket-char-class s)))
                    "ошибка: висячий дефис в конце диапазона [a-]"))
    
    (format t "~%=== Модуль parser/char-class: тестирование завершено ===~%")
    (format t "Успешные: ~A~%Неудачные: ~A~%" passed failed)
    (= failed 0)
  )  
)