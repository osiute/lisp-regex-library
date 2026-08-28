;; Парсинг диапазонов квантификаторов вида {n}, {n,}, {,m}, {n,m}
(in-package :regex-library)

;; Парсит диапазон {min,max} и возвращает точечную пару (min . max)
;; Вызывается, когда текущий символ — '{'
(defun parse-range-quantifier (state)
  (parser-next state)
  
  (let ((min-val nil)
        (max-val nil))
    
    ;; 1. Читаем первое (левое, минимальное) число, если оно есть
    (if (digit-char-p (or (parser-peek state) #\Nul))
        (setf min-val (parser-parse-number state))
        (setf min-val 0) ; Если числа нет (например {,5}), min = 0
     )
    
    (let ((cur (parser-peek state)))
      (unless cur
        (error "Синтаксическая ошибка: незакрытый диапазон '{' в позиции ~A"
               (parser-state-index state))
       )
      
      (cond
        ;; Вариант A: Точное количество {n}
        ((eql cur #\})
         (parser-next state) ; Пропускаем '}'
         (unless min-val
           (error "Синтаксическая ошибка: пустой диапазон {} в позиции ~A"
                  (parser-state-index state))
          )
         (setf max-val min-val))
        
        ;; Вариант B: Запятая {n,m}, {n,} или {,m}
        ((eql cur #\,)
         (parser-next state) ; Пропускаем ','
         (let ((next-cur (parser-peek state)))
           (unless next-cur
             (error "Синтаксическая ошибка: незакрытый диапазон '{' в позиции ~A"
                    (parser-state-index state))
            )
           (if (eql next-cur #\})
               (progn
                 (parser-next state) ; Пропускаем '}'
                 (setf max-val nil)) ; Без верхней границы {n,}
               (progn
                 (unless (digit-char-p next-cur)
                   (error "Синтаксическая ошибка: ожидалась цифра в позиции ~A"
                          (parser-state-index state))
                  )
                 (setf max-val (parser-parse-number state))
                 (unless (eql (parser-peek state) #\})
                   (error "Синтаксическая ошибка: ожидалась '}' в позиции ~A"
                          (parser-state-index state))
                  )
                 (parser-next state) ; Пропускаем '}'
                )
            )
          ))
        
        (t
         (error "Синтаксическая ошибка: некорректный символ '~A' в диапазоне в позиции ~A"
                cur (parser-state-index state)))
       )
     )
    
    (when (and min-val max-val (> min-val max-val))
      (error "Синтаксическая ошибка: min (~A) не может быть больше max (~A) в позиции ~A"
             min-val max-val (parser-state-index state))
     )
    
    (when (and (= min-val 0) (= max-val 0))
      (error "Синтаксическая ошибка: диапазон пуст {} в позиции ~A" 
             (parser-state-index state))
    ) 

    (cons min-val max-val)
   )
)