;; Основная логика рекурсивного спуска
(in-package :regex-library)

;; Опережающее объявление функций для взаимной рекурсии.
;; Без него файл не компилируется.
(declaim (ftype (function (t) t) 
                parse-atom 
                parse-quantifier 
                parse-concatenation 
                parse-expression))

;; Константа спецсимволов, которые не могут начинать атом.
;; Из-за особенностей символа #\) редактор кода путается в скобках.
(defparameter +invalid-atom-start-chars+ '(#\) #\] #\} #\| #\* #\+ #\?))

;; 1. Разбор базовых атомов
(defun parse-atom (state)
"Считывает базовый элемент грамматики (литерал, класс символов, якорь или подвыражение в скобках).
Возвращает один из AST-узлов: AST-LITERAL, AST-CHAR-CLASS, AST-ANCHOR или результат внутреннего parse-expression.
"
  (let ((cur (parser-peek state)))
    (unless cur
      (error "Синтаксическая ошибка: неожиданный конец строки")
     )
    
    (cond
      ;; Якорь начала строки ^
      ((eql cur #\^)
       (parser-next state)
       (make-ast-anchor :type :start-of-line))

      ;; Якорь конца строки $
      ((eql cur #\$)
       (parser-next state)
       (make-ast-anchor :type :end-of-line))

      ;; Точка . (любой символ)
      ((eql cur #\.)
       (parser-next state)
       (make-ast-dot))

      ;; Символьные классы [...]
      ((eql cur #\[)
       (parse-bracket-char-class state))

      ;; Экранирование \d, \w, \s или экранированный литерал
      ((eql cur #\\)
       (parse-escape-char-class state))

      ;; Группирующие скобки (...)
      ((eql cur #\()
       (parser-next state)
       (let ((expr (parse-expression state)))
         (unless (eql (parser-peek state) #\))
           (error "Синтаксическая ошибка: ожидалась ')' в позиции ~A"
                  (parser-state-index state))
          )
         (parser-next state)
         expr
        ))

      ;; Проверка на запрещённые спецсимволы в начале атома
      ((find cur +invalid-atom-start-chars+ :test #'char=)
       (error "Синтаксическая ошибка: неожиданный служебный символ '~A' в позиции ~A"
              cur (parser-state-index state)))

      ;; Обычный литерал
      (t
       (let ((ch (parser-next state)))
         (make-ast-literal :code (char-code ch))
        ))
     )
   )
)

;; --- Заглушки для интеграции (будем наполнять на следующих шагах) ---
;; 2. Разбор квантификаторов
(defun parse-quantifier (state)
"Считывает атом и применимый к нему квантификатор (*, +, ?, {n,m}).
Возвращает обёрнутый узел (AST-STAR, AST-PLUS, AST-QUESTION, AST-RANGE) или исходный атом, если квантификатор отсутствует.
"
  (let ((node (parse-atom state)))
    (let ((next-char (parser-peek state)))
      (cond
        ;; 1. Квантификатор *
        ((eql next-char #\*)
         (when (ast-anchor-p node)
           (error "Синтаксическая ошибка: квантификатор '*' не может применяться к якорю в позиции ~A"
                  (parser-state-index state))
          )
         (parser-next state)
         (make-ast-star :child node))

        ;; 2. Квантификатор +
        ((eql next-char #\+)
         (when (ast-anchor-p node)
           (error "Синтаксическая ошибка: квантификатор '+' не может применяться к якорю в позиции ~A"
                  (parser-state-index state))
          )
         (parser-next state)
         (make-ast-plus :child node))

        ;; 3. Квантификатор ?
        ((eql next-char #\?)
         (when (ast-anchor-p node)
           (error "Синтаксическая ошибка: квантификатор '?' не может применяться к якорю в позиции ~A"
                  (parser-state-index state))
          )
         (parser-next state)
         (make-ast-question :child node))

        ;; 4. Диапазонный квантификатор {n,m}
        ((eql next-char #\{)
         (when (ast-anchor-p node)
           (error "Синтаксическая ошибка: диапазонный квантификатор не может применяться к якорю в позиции ~A"
                  (parser-state-index state))
          )
         (let ((range (parse-range-quantifier state)))
           (make-ast-range :min (car range)
                           :max (cdr range)
                           :child node
            )
          ))

        ;; 5. Квантификатор отсутствует
        (t node)
       )
     )
   )
)
;; 3. Разбор конкатенаций
(defun parse-concatenation (state)
"Считывает последовательность квантифицированных элементов до |, ) или конца строки.
Возвращает список элементов, обёрнутый в AST-CONCAT, либо единичный узел (без обёртки), если элемент ровно один.
"
  (parse-quantifier state)
)
;; 4. Разбор всего выражения
(defun parse-expression (state)
"Точка входа грамматики. Считывает альтернации выражений, разделённые символом '|'.
Возвращает дерево AST-ALT при наличии альтернаций, либо прокинутый узел нижней операции (результат parse-concatenation).
"
  (parse-concatenation state)
)