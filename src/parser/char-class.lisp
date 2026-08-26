;; Реализует парсинг символьных классов ([a-z], [^0-9], \d, \w, \s)

(in-package :regex-library)

;; Возвращает список диапазонов для встроенных классов (\d, \w, \s).
;; Результат имеет вид ((start.end)...).
;; Возвращает nil, если char-code не является встроенным классом.
(defun get-builtin-char-class-ranges (char-code)
  (case char-code
    (#\d (list (cons #\0 #\9)))
    (#\w (list (cons #\a #\z)
               (cons #\A #\Z)
               (cons #\0 #\9)
               (cons #\_ #\_)))
    (#\s (list (cons #\Space #\Space)
               (cons #\Tab #\Tab)
               (cons #\Page #\Page)
               (cons (code-char 10) (code-char 10))   ; \n
               (cons (code-char 13) (code-char 13)))) ; \r
    (t nil)
   )
)

;; Парсит встроенный класс (\d, \w, \s) или символ экранирования (\., \\ и т.д.)
;; Вызывается, когда текущий символ — '\'
(defun parse-escape-char-class (state)
  (parser-next state) ; Пропускаем '\'
  (let ((escaped (parser-next state)))
    (unless escaped
      (error "Синтаксическая ошибка: незавершённая escape-последовательность в позиции ~A" 
             (parser-state-index state))
     )
    (let ((builtin-ranges (get-builtin-char-class-ranges escaped)))
      (if builtin-ranges
          (make-ast-char-class :ranges builtin-ranges :negated-p nil)
          ;; Если это не \d, \w, \s — воспринимаем как обычный экранированный символ
          (make-ast-char-class :ranges (list (cons escaped escaped)) :negated-p nil)
       )
     )
   )
)

;; Парсит класс символов в квадратных скобках: [a-z0-9] или [^abc]
;; Вызывается, когда текущий символ — '['
(defun parse-bracket-char-class (state)
  (parser-next state) ; Пропускаем '['
  (let ((negated nil)
        (ranges nil))
    
    ;; Проверяем отрицание '^'
    (when (parser-match-p state #\^)
      (setf negated t)
     )
    
    (loop
      (let ((cur (parser-peek state)))
        ;; Ошибка: строка закончилась, скобка не закрыта
        (unless cur
          (error "Синтаксическая ошибка: незакрытый символьный класс '[' в позиции ~A" 
                 (parser-state-index state))
         )
        
        ;; Условие выхода: встретили закрывающую скобку ']'
        (when (eql cur #\])
          (parser-next state)
          (return)
         )
        
        ;; Читаем первый символ диапазона или одиночный символ
        (let ((start-char (parser-next state)))
          ;; Проверяем, задан ли диапазон через дефис (например, 'a-z')
          (if (and (eql (parser-peek state) #\-)
                    ;; после дефиса не стоит сразу же закрывающая скобка (класс символов не имеет вид [a-])
                   (not (eql (char (parser-state-str state) (1+ (parser-state-index state))) #\]))) 
              (progn
                (parser-next state) ; Пропускаем '-'
                (let ((end-char (parser-next state)))
                  (unless end-char
                    (error "Синтаксическая ошибка: некорректный диапазон в позиции ~A" 
                           (parser-state-index state))
                   )
                  (when (> (char-code start-char) (char-code end-char))
                    (error "Синтаксическая ошибка: неверный порядок диапазона (~A-~A) в позиции ~A"
                           start-char end-char (parser-state-index state))
                   )
                  (push (cons start-char end-char) ranges)
                 )
               )
              ;; Одиночный символ
              (push (cons start-char start-char) ranges)
           )
         )
       )
     )
    
    (make-ast-char-class :ranges (nreverse ranges) :negated-p negated)
   )
)