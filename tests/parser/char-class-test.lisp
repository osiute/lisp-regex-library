(in-package :regex-library)

(deftest run-char-class-tests "parser/char-class"
  ;; --- Тест 1: Экранированные спецклассы \d, \w ---
  (let ((s (make-parser-state :str "\\d" :len 2)))
    (let ((node (parse-escape-char-class s)))
      (assert-equal (ast-char-class-ranges node) '((#\0 . #\9)) "\\d дает диапазон 0-9")
      (assert-equal (ast-char-class-negated-p node) nil "\\d не отрицательный")
    )
  )

  (let ((s (make-parser-state :str "\\w" :len 2)))
    (let ((node (parse-escape-char-class s)))
      (assert-equal (ast-char-class-ranges node)
                    '((#\a . #\z) (#\A . #\Z) (#\0 . #\9) (#\_ . #\_))
                    "\\w дает диапазоны (a-z, A-Z, 0-9, _)")
      (assert-equal (ast-char-class-negated-p node) nil "\\w не отрицательный")
    )
  )

  ;; --- Тест 2: Скобочные группы с диапазонами [a-z0-9] ---
  (let ((s (make-parser-state :str "[a-z0-9]" :len 8)))
    (let ((node (parse-bracket-char-class s)))
      (assert-equal (ast-char-class-ranges node) '((#\a . #\z) (#\0 . #\9)) "парсинг диапазонов a-z и 0-9")
      (assert-equal (ast-char-class-negated-p node) nil "обычный класс без отрицания")
    )
  )

  ;; --- Тест 3: Отрицание в скобках [^abc] ---
  (let ((s (make-parser-state :str "[^abc]" :len 6)))
    (let ((node (parse-bracket-char-class s)))
      (assert-equal (ast-char-class-ranges node)
                    '((#\a . #\a) (#\b . #\b) (#\c . #\c))
                    "парсинг отдельных символов a, b, c")
      (assert-equal (ast-char-class-negated-p node) t "установлен флаг negated-p")
    )
  )

  ;; --- Тест 4: Дефис в конце [^a-] ---
  (let ((s (make-parser-state :str "[^a-]" :len 5)))
    (let ((node (parse-bracket-char-class s)))
      (assert-equal (ast-char-class-ranges node)
                    '((#\a . #\a) (#\- . #\-))
                    "[^a-]: дефис в конце диапазона воспринимается как символ")
      (assert-equal (ast-char-class-negated-p node) t "[^a-]: установлен флаг negated-p")
    )
  )

  ;; --- Проверка обработки ошибок (неправильного задания классов символов) ---
  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "[z-a]" :len 5)))
                    (parse-bracket-char-class s)
                  )
                )
                "ошибка: перевернутый диапазон [z-a]")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "[a-z" :len 4)))
                    (parse-bracket-char-class s)
                  )
                )
                "ошибка: незакрытая квадратная скобка [a-z")

  (assert-error (lambda ()
                  (let ((s (make-parser-state :str "\\" :len 1)))
                    (parse-escape-char-class s)
                  )
                )
                "ошибка: обрывающаяся escape-последовательность \\")
)