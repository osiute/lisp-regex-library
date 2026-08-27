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
  (parse-atom state)
)
;; 3. Разбор конкатенаций
(defun parse-concatenation (state)
  (parse-quantifier state)
)
;; 4. Разбор всего выражения
(defun parse-expression (state)
  (parse-concatenation state)
)