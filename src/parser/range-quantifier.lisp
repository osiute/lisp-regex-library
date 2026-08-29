;; Парсинг диапазонов квантификаторов вида {n}, {n,}, {n,m}
(in-package :regex-library)

;; Парсит первое число в диапазоне (минимальное значение)
(defun parse-first-number (state)
  (if (digit-char-p (or (parser-peek state) #\Nul))
      (parser-parse-number state)
      (error "Синтаксическая ошибка: не указана нижняя граница квантификатора в позиции ~A"
              (parser-state-index state))
  )
) 

;; Проверяет, что после первого числа есть символ
(defun check-next-char-or-error (state)
  (let ((cur (parser-peek state)))
    (unless cur
      (error "Синтаксическая ошибка: незакрытый диапазон '{' в позиции ~A"
             (parser-state-index state)))
    cur))

;; Обрабатывает вариант точного количества {n}
(defun handle-exact-quantifier (state min-val)
  (unless min-val
    (error "Синтаксическая ошибка: пустой диапазон {} в позиции ~A"
           (parser-state-index state)))
  (parser-next state) ; Пропуск '}'
  (cons min-val min-val))

;; Парсит второе число после запятой (максимальное значение)
(defun parse-second-number (state)
  (let ((next-cur (parser-peek state)))
    (unless next-cur
      (error "Синтаксическая ошибка: незакрытый диапазон '{' в позиции ~A"
             (parser-state-index state)))
    
    (if (digit-char-p next-cur)
        (parser-parse-number state)
        nil)))

;; Обрабатывает вариант {n,} — без верхней границы
(defun handle-unbounded-range (state min-val)
  (parser-next state) ; Пропуск '}'
  (cons min-val nil))

;; Обрабатывает вариант {n,m} — с верхней границей
(defun handle-bounded-range (state min-val)
  (unless (digit-char-p (parser-peek state))
    (error "Синтаксическая ошибка: ожидалась цифра в позиции ~A"
           (parser-state-index state)))
  (let ((max-val (parser-parse-number state)))
    (unless (eql (parser-peek state) #\})
      (error "Синтаксическая ошибка: ожидалась '}' в позиции ~A"
             (parser-state-index state)))
    (parser-next state) ; Пропуск '}'
    (cons min-val max-val)))

;; Обрабатывает вариант диапазона {n,m}, {n,}
(defun handle-range-quantifier (state min-val)
  (parser-next state) ; Пропуск ','
  (let ((next-cur (parser-peek state)))
    (unless next-cur
      (error "Синтаксическая ошибка: незакрытый диапазон '{' в позиции ~A"
             (parser-state-index state)))
    
    (if (eql next-cur #\})
        ;; Вариант {n,} — без верхней границы
        (handle-unbounded-range state min-val)
        ;; Вариант {n,m} — с верхней границей
        (handle-bounded-range state min-val))))

;; Выбирает вариант обработки в зависимости от символа
(defun dispatch-quantifier (state min-val cur)
  (cond
    ;; Вариант {n}
    ((eql cur #\})
     (handle-exact-quantifier state min-val))
    
    ;; Вариант с запятой: {n,m}, {n,}
    ((eql cur #\,)
     (handle-range-quantifier state min-val))
    
    (t
     (error "Синтаксическая ошибка: некорректный символ '~A' в диапазоне в позиции ~A"
            cur (parser-state-index state)))))

(defun validate-range (min-val max-val state)
  (when (null min-val)
    (error "МОЯ ОШИБКА, сюда min-val не должно было пройти: validate-range: (null min-val) в позиции ~A"
           (parser-state-index state)
    )
  )
  (when (and min-val max-val (> min-val max-val))
    (error "Синтаксическая ошибка: min (~A) не может быть больше max (~A) в позиции ~A"
           min-val max-val (parser-state-index state)
    )
  )
)

;; Парсит диапазон {min,max} и возвращает точечную пару (min . max)
;; Вызывается, когда текущий символ — '{'
(defun parse-range-quantifier (state)
  (parser-next state) ; пропуск '{'
  
  (let ((min-val (parse-first-number state)))
    (let ((cur (check-next-char-or-error state)))
      (let ((range (dispatch-quantifier state min-val cur)))
        (validate-range (car range) (cdr range) state)
        
        range)))) ; return