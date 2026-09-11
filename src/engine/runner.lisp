(in-package :regex-library)

;; ----------------------------------------------------------------------------------
;; Вычисление 6-битной маски контекста
;; ----------------------------------------------------------------------------------

(defun word-char-at-p (text idx len char-mode)
  (and (>= idx 0)
       (< idx len)
       (word-char-p (char text idx) char-mode)
  )
)

(defun compute-context-mask (text i len char-mode)
  (let ((mask 0)
        (prev-ch (when (> i 0) (char text (1- i))))
        (curr-ch (when (< i len) (char text i)))
        (next-ch (when (< (1+ i) len) (char text (1+ i)))))
    ;; 0 разряд: ^ (начало строки)
    (when (or (= i 0) (and prev-ch (char-newline-p prev-ch)))
      (setf mask (logior mask #b000001)))
    ;; 1 разряд: \A (начало текста)
    (when (= i 0)
      (setf mask (logior mask #b000010)))
    ;; 2 разряд: $ (конец строки)
    (when (or (= i (1- len)) (and next-ch (char-newline-p next-ch)))
      (setf mask (logior mask #b000100)))
    ;; 3 разряд: \Z (почти конец текста)
    (when (or (= i (1- len))
              (and (= i (- len 2)) (char-newline-p next-ch)))
      (setf mask (logior mask #b001000)))
    ;; 4 разряд: \z (конец текста)
    (when (= i (1- len))
      (setf mask (logior mask #b010000)))
    ;; 5 разряд: \b (граница слова)
    (let ((prev-w (if (word-char-at-p text (1- i) len char-mode) 1 0))
          (curr-w (if (word-char-at-p text i len char-mode) 1 0)))
      (when (= (logxor prev-w curr-w) 1)
        (setf mask (logior mask #b100000))))
    mask
  )
)

;; ----------------------------------------------------------------------------------
;; Прямые проходы
;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;; Обратный проход
;; ----------------------------------------------------------------------------------