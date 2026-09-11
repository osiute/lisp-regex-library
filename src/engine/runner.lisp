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
        (curr-ch (when (< i len) (char text i))))
    ;; 0 разряд: ^ (начало строки)
    (when (or (= i 0) (and prev-ch (char-newline-p prev-ch)))
      (setf mask (logior mask #b000001)))
    ;; 1 разряд: \A (начало текста)
    (when (= i 0)
      (setf mask (logior mask #b000010)))
    ;; 2 разряд: $ (конец строки)
    (when (or (= i len) (and curr-ch (char-newline-p curr-ch)))
      (setf mask (logior mask #b000100)))
    ;; 3 разряд: \Z (почти конец текста)
    (when (or (= i len)
              (and (= i (1- len)) (char-newline-p curr-ch)))
      (setf mask (logior mask #b001000)))
    ;; 4 разряд: \z (конец текста)
    (when (= i len)
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

;; Ищет первое терминальное состояние на диапазоне [left-bound, right-bound] (включительно).
;; Возвращает через values индекс текста, на котором произошло совпадение, а также индекс состояния.
;; Если терминального состояния нет, возвращает (values nil nil), если терминальное состояние не встретилось.
;; Если терминальное взятое начальное состояние сразу оказалось терминальным (например, для pattern=""),
;; возращает left-bound - 1.
(defun unanchored-direct-pass-to-first-terminal (regex text left-bound right-bound)
  (if (= right-bound (1- left-bound)) ; пустая подстрока
    (assert (< right-bound (length text)) ()
      "unanchored-direct-pass-to-first-terminal: left-bound = ~A, right-bound = ~A, (length text) = ~A. Я НАПИСАЛ УЖАСНУЮ ПРОГРАММУ!!! ЭТА ФУНКЦИЯ ДОЛЖНА ВЫЗЫВАТЬСЯ С ПРАВИЛЬНЫМИ ГРАНИЦАМИ."
                                                    left-bound right-bound (length text))
    (assert (and (>= right-bound left-bound) (< right-bound (length text))) ()
      "unanchored-direct-pass-to-first-terminal: left-bound = ~A, right-bound = ~A, (length text) = ~A. Я НАПИСАЛ УЖАСНУЮ ПРОГРАММУ!!! ЭТА ФУНКЦИЯ ДОЛЖНА ВЫЗЫВАТЬСЯ С ПРАВИЛЬНЫМИ ГРАНИЦАМИ."
                                                    left-bound right-bound (length text))
  )
  (let* ((dfa (regex-direct-dfa regex))
         (mode (regex-builtin-char-class-mode regex))
         (len (length text))
         (eq-table (regex-eq-classes-table regex))
         (ctx (compute-context-mask text left-bound len mode))
         (curr-state (dfa-get-start-state dfa ctx t))) ; t — для unanchored-p
    
    (loop for k from left-bound to (1+ right-bound) do
      (when (dfa-accept-state-p dfa curr-state)
        (return (values (1- k) curr-state)) ; возвращение k - 1, т.к. принимающее состояние получено на предыдущем шаге.
      )
      (when (= k (1+ right-bound)) (return (values nil nil)))
      (let* ((ch (char text k))
             (eq-cls (char-to-class-id ch eq-table))
             (next-ctx (compute-context-mask text (1+ k) len mode)))
        (setf curr-state (dfa-step-state dfa curr-state eq-cls next-ctx))
        (assert (and curr-state (> curr-state -1)) () 
          "unanchored-direct-pass-to-first-terminal: cur-state (~A) < 0. Я НАПИСАЛ УЖАСНУЮ ПРОГРАММУ!!! НЕПРИВЯЗАННЫЙ ДКА НЕ ДОЛЖЕН ПОПАДАТЬ В ТУПИК ДО ПЕРВОГО ТЕРМИНАЛЬНОГО СОСТОЯНИЯ."
                                                     curr-state)
      )
    )
  )
)

;; ----------------------------------------------------------------------------------
;; Обратный проход
;; ----------------------------------------------------------------------------------