(in-package :regex-library)

(deftest run-char-class-tests "parser/char-class"
  (test-escape-char-classes #'assert-equal)
  (test-bracket-char-class-ranges #'assert-equal)
  (test-negated-bracket-char-class #'assert-equal)
  (test-literal-hyphens-in-bracket-class #'assert-equal)
  (test-char-class-errors #'assert-error))

;; Встроенные классы возвращают реальные диапазоны; дополнительный класс хранится
;; как обычный char-class с диапазоном дополнения, а не как флаг negated-p.
(defun test-escape-char-classes (assert-equal-fn)
  (let ((node (parse-escape-char-class (make-parser-state :str "\\d" :len 2) :ascii)))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\0 . #\9)) "\\d дает диапазон 0-9")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "\\d не отрицательный"))
  (let ((node (parse-escape-char-class (make-parser-state :str "\\w" :len 2) :ascii)))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\a . #\z) (#\A . #\Z) (#\0 . #\9) (#\_ . #\_))
             "\\w дает диапазоны (a-z, A-Z, 0-9, _)" )
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "\\w не отрицательный"))
  (let ((node (parse-escape-char-class (make-parser-state :str "\\W" :len 2) :ascii)))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             (get-builtin-char-class-ranges-complement #\W :ascii)
             "\\W дает реальные диапазоны дополнения к \\w")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "\\W не отрицательный")))

;; Диапазоны внутри класса сохраняются в исходном порядке.
(defun test-bracket-char-class-ranges (assert-equal-fn)
  (let ((node (parse-bracket-char-class
               (make-parser-state :str "[a-z0-9]" :len 8))))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\0 . #\9) (#\a . #\z))
             "парсинг диапазонов a-z и 0-9")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "обычный класс без отрицания")))

;; Символ '^' после '[' помечает класс как отрицательный.
(defun test-negated-bracket-char-class (assert-equal-fn)
  (let ((node (parse-bracket-char-class
               (make-parser-state :str "[^abc]" :len 6))))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\c . #\c) (#\b . #\b) (#\a . #\a))
             "парсинг отдельных символов a, b, c")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             t "установлен флаг negated-p")))

;; Дефис является литералом в начале/конце класса и после escape-последовательности.
(defun test-literal-hyphens-in-bracket-class (assert-equal-fn)
  (let ((node (parse-bracket-char-class
               (make-parser-state :str "[a\\-z]" :len 6))))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\z . #\z) (#\- . #\-) (#\a . #\a))
             "экранированный дефис воспринимается как три отдельных символа")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "обычный класс без отрицания"))
  (let ((node (parse-bracket-char-class
               (make-parser-state :str "[-az]" :len 5))))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\z . #\z) (#\a . #\a) (#\- . #\-))
             "дефис в начале класса разбирается как литерал '-'")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "обычный класс без отрицания"))
  (let ((node (parse-bracket-char-class
               (make-parser-state :str "[az-]" :len 5))))
    (funcall assert-equal-fn (ast-char-class-ranges node)
             '((#\- . #\-) (#\z . #\z) (#\a . #\a))
             "дефис в конце класса разбирается как литерал '-'")
    (funcall assert-equal-fn (ast-char-class-negated-p node)
             nil "обычный класс без отрицания")))

;; Некорректные диапазоны и незавершённые escape-последовательности.
(defun test-char-class-errors (assert-error-fn)
  (funcall assert-error-fn
           (lambda () (parse-bracket-char-class
                       (make-parser-state :str "[z-a]" :len 5)))
           "ошибка: перевернутый диапазон [z-a]")
  (funcall assert-error-fn
           (lambda () (parse-bracket-char-class
                       (make-parser-state :str "[a-z" :len 4)))
           "ошибка: незакрытая квадратная скобка [a-z")
  (funcall assert-error-fn
           (lambda () (parse-escape-char-class
                       (make-parser-state :str "\\" :len 1) :ascii))
           "ошибка: обрывающаяся escape-последовательность")
  )
