(in-package :regex-library)

;; Парсит unicode-символ, записанный в виде XXXX или {X+}, где X+ — 1-6 подряд идущих hex-цифр.
(defun parse-unicode-character (state)
  (let* ((cur (parser-peek state))
         (code (cond 
           ((not cur) (error "Синтаксическая ошибка в позиции ~A: незаконченное экранирование. \\u должен иметь вид \\uXXXX или \\u{X+} (1-6 hex-цифр в фигурных скобках)"
                               (parser-state-index state)))
           ((digit-char-p cur 16) (parse-hex-number 4 4 state)) ; должен иметь вид \uXXXX
           ((eql cur #\{) (get-unicode-in-curly state)) ; должен иметь вид \u{X+}
           (t (error "Синтаксическая ошибка: в позиции ~A неизвестный символ '~A'. \\u должен иметь вид \\uXXXX или \\u{X+} (1-6 hex-цифр в фигурных скобках)"
                               (parser-state-index state) cur)))))
    
    (code-char code)
  )
)

;; парсит и возвращает код символа, записанного в виде {X+}
(defun get-unicode-in-curly (state)
  (let ((cur (parser-next state))) ; считывает '{' и переводит курсор
    (assert (and cur (eql cur #\{)) () 
            "parser/unicode/get-unicode-in-curly: парсинг не начинается с '{', Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ! parser-peek — ~S" cur)
    (let ((code (parse-hex-number 1 6 state)))
      (setf cur (parser-next state)) ; считывает '}' и переводит курсор
      (assert (and cur (eql cur #\})) () 
               "Синтаксическая ошибка: в позиции ~A неизвестный символ '~A'. \\u должен иметь вид \\uXXXX или \\u{X+} (1-6 hex-цифр в фигурных скобках)"
               (1- (parser-state-index state)) cur)
      code
    )
  )
)

; парсит hex-число размером от min-digit-count до max-digit-count с текущей позиции.
(defun parse-hex-number (min-digit-count max-digit-count state)
  (let ((acc 0)) 
    (dotimes (digit-count max-digit-count)
      (let ((cur (parser-peek state)))
        (when (null cur)
          (when (>= digit-count min-digit-count) (return)) ;; выйти из цикла
          (error "Синтаксическая ошибка в позиции ~A: незаконченное экранирование. \\u должен иметь вид \\uXXXX или \\u{X+} (1-6 hex-цифр в фигурных скобках)"
               (parser-state-index state))
        )
        (when (not (digit-char-p cur 16))
          (when (>= digit-count min-digit-count) (return)) ;; выйти из цикла
          (error "Синтаксическая ошибка: в позиции ~A неизвестный символ '~A'. \\u должен иметь вид \\uXXXX или \\u{X+} (1-6 hex-цифр в фигурных скобках)"
               (parser-state-index state) cur)
        )
        (setf acc (+ (* acc 16) (digit-char-p cur 16))) ; дописать hex-цифру справа
        (parser-next state)
      )
    )
    (assert (and (>= acc 0) (<= acc #x10FFFF)) ()
            "parser/unicode/parse-hex-number: acc не входит в диапазон unicode, Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ! acc — ~A" acc)
    acc
  )
)