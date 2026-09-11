(in-package :regex-library)

;; ==================================================================================
;; Вспомогательные функции
;; ==================================================================================

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

(declaim (inline compute-start-state-id))
(defun compute-start-state-id (regex text idx &key unanchored-p)
  (let* ((mode (regex-builtin-char-class-mode regex))
        (len (length text))
        (ctx (compute-context-mask text idx len mode))
        (dfa (regex-direct-dfa regex)))
    (dfa-get-start-state dfa ctx unanchored-p)
  )
)

;; ==================================================================================
;; Прямые проходы
;; ==================================================================================
;; Ищут терминальное состояние на диапазоне [left-bound, right-bound] (включительно).
;; Возвращает (values text-idx terminal-state-id) или (value nil nil)
;; lazy — ищет первое терминальное состояние;
;; greedy — ищет последнее терминальное состояние, до которого можно дотянуться.
;; anchored — вычисляет только ветку с left-bound;
;; unanchored — для каждого посещённого индекса добавляет новую ветку для вычисления,
;; пока не дойдёт до right-bound (или до первого терминального для lazy).

(defun lazy-unanchored-direct-pass (regex text left-bound right-bound
                                                 &key unanchored-state-id)
  (when (null unanchored-state-id)
    (setf unanchored-state-id (compute-start-state-id regex text left-bound :unanchored-p t))
  )
  
  (direct-pass-logic regex text unanchored-state-id left-bound right-bound
                    :return-on-first-terminal-p t)
)

(defun greedy-anchored-direct-pass (regex text left-bound right-bound
                                    &key anchored-state-id)
  (when (null anchored-state-id)
    (setf anchored-state-id (compute-start-state-id regex text left-bound :unanchored-p nil))
  )

  (direct-pass-logic regex text anchored-state-id left-bound right-bound
                      :return-on-first-terminal-p nil)
)

(defun lazy-anchored-direct-pass (regex text left-bound right-bound
                                  &key anchored-state-id)
  (when (null anchored-state-id)
    (setf anchored-state-id (compute-start-state-id regex text left-bound :unanchored-p nil))
  )

  (direct-pass-logic regex text anchored-state-id left-bound right-bound
                    :return-on-first-terminal-p t)
)

;; ==================================================================================
;; Обратный проход
;; ==================================================================================


;; ==================================================================================
;; Основаная логика
;; ==================================================================================

;; Ищет терминальное состояние на диапазоне [left-bound, right-bound] (включительно).
;; return-on-first-terminal-p определяет жадность поиска.
;; Возвращает через values индекс текста, на котором произошло совпадение, а также id состояния.
;; Если терминального состояния нет, возвращает (values nil nil), если терминальное состояние не встретилось.
;; Если взятое начальное состояние сразу оказалось терминальным (единственным или первым в зависимости от return-on-first-terminal-p) 
;; (например, для pattern=""), возращает (values left-bound - 1 dfa-state-id).
(defun direct-pass-logic (regex text start-state-id left-bound right-bound
                                   &key return-on-first-terminal-p)
  (if (= right-bound (1- left-bound)) ; пустая подстрока
    (assert (< right-bound (length text)) ()
      "direct-pass-logic: left-bound = ~A, right-bound = ~A, (length text) = ~A. Я НАПИСАЛ УЖАСНУЮ ПРОГРАММУ!!! ЭТА ФУНКЦИЯ ДОЛЖНА ВЫЗЫВАТЬСЯ С ПРАВИЛЬНЫМИ ГРАНИЦАМИ."
                                                    left-bound right-bound (length text))
    (assert (and (>= right-bound left-bound) (< right-bound (length text))) ()
      "direct-pass-logic: left-bound = ~A, right-bound = ~A, (length text) = ~A. Я НАПИСАЛ УЖАСНУЮ ПРОГРАММУ!!! ЭТА ФУНКЦИЯ ДОЛЖНА ВЫЗЫВАТЬСЯ С ПРАВИЛЬНЫМИ ГРАНИЦАМИ."
                                                    left-bound right-bound (length text))
  )
  
  (let* ((dfa (regex-direct-dfa regex))
         (mode (regex-builtin-char-class-mode regex))
         (len (length text))
         (eq-table (regex-eq-classes-table regex))
         (curr-state-id start-state-id)
         (last-accept-i nil)
         (last-accept-state-id nil))
    
    (loop for k from left-bound to (1+ right-bound) do
      (when (dfa-accept-state-p dfa curr-state-id)
        (setf last-accept-i (1- k)) ; -1, т.к. переход в терминальное произошёл на прошлом индексе.
        (setf last-accept-state-id curr-state-id)
        (when return-on-first-terminal-p
          (return (values last-accept-i last-accept-state-id))
        )
      )
      (when (= k (1+ right-bound)) (return (values last-accept-i last-accept-state-id)))
      (let* ((ch (char text k))
             (eq-cls (char-to-class-id ch eq-table))
             (next-ctx (compute-context-mask text (1+ k) len mode)))
        (setf curr-state-id (dfa-step-state dfa curr-state-id eq-cls next-ctx))
        (when (or (null curr-state-id) (< curr-state-id 0)) ; попадание в тупик
          (return (values last-accept-i last-accept-state-id))
        )
      )
    )
  )
)