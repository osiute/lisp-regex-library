;; Реализует функционал посимвольного сканера строки реглярного выражения для парсинга.

(in-package :regex-library)

;; Состояние сканера парсера
(defstruct parser-state
  (str "" :type simple-string)
  (index 0 :type fixnum)
  (len 0 :type fixnum)
)

;; Возвращает текущий символ без сдвига указателя.
;; Если указатель находится за пределами строки, возвращает nil.
(defun parser-peek (state)
  (if (< (parser-state-index state) (parser-state-len state))
    (char (parser-state-str state) (parser-state-index state))
    nil
  )
)

;; Возвращает текущий символ и сдвигает указатель вперед
;; Если указатель находится за пределами строки, возвращает nil.
(defun parser-next (state)
  (let ((cur (parser-peek state))) 
    (when cur
      (incf (parser-state-index state))
     )
    cur
   )
)

;; Если текущий символ совпадает с target-char, делает сдвиг указателя вперёд, возвращает T;
;; иначе возвращает nil без сдвига указателя.
(defun parser-match-p (state target-char)
  (when (eql (parser-peek state) target-char)
    (parser-next state)
    T
  )
)

;; Парсит целое положительное число из текущей позиции (для повторов {m,n}).
;; Возвращает число как fixnum, сдвигая указатель на конец числа (следующий символ после последней цифры).
;; Если из текущей позиции число не найдено (текущая позиция не является цифрой), вызывает ошибку)
(defun parser-parse-number (state)
  (let ((acc 0)
        (found-number-p nil)) 
    (loop
      (let ((cur (parser-peek state)))
        (unless (and cur (digit-char-p cur))
          (return)
        )
        (setf acc (+ (* acc 10) (digit-char-p cur))) ; дописать цифру справа
        (setf found-number-p T)
        (parser-next state)
      )
    )
    (unless found-number-p
      (error "Синтаксическая ошибка: ожидалось число в позиции ~A, обнаружено — ~A" (parser-state-index state) (parser-peek state))
    )
    acc
  )
)