;; Функционал проходов по межсимвольным позициям текста TEXT для поиска вхождений, соответствующих регулярному выражению REGEX.
;; Для любой строки длины N существует N + 1 межсимвольная позиция.
;; Для строки "abc" межсимвольные позиции имеют следующий вид: 0 'a' 1 'b' 2 'c' 3.
;; Для пустой строки "" есть только одна межсимвольная позиция: 0.
;; Проходы осуществляются по межсимвольным позициям, а не по символам, 
;;    что позволяет корректно обрабатывать контексты начала и конца строки, а также границы слов.
;; left-bound-pos и right-bound-pos — строгие интервалы [left-bound-pos, right-bound-pos] межсимвольных позиций,
;;    на которых осуществляется проход.

(in-package :regex-library)

;; ==================================================================================
;; Вспомогательные inline-функции.
;; ==================================================================================

(declaim (inline compute-direct-start-state-id))
(defun compute-direct-start-state-id (regex text pos-idx &key unanchored-p)
  (let* ((mode (regex-builtin-char-class-mode regex))
         (len (length text))
         ;; Контекст начальной межсимвольной позиции pos-idx
         (ctx (compute-context-mask text pos-idx len mode))
         (dfa (regex-direct-dfa regex)))
    (dfa-get-start-state dfa ctx unanchored-p)
  )
)

(declaim (inline compute-reverse-start-state-id))
(defun compute-reverse-start-state-id (regex text pos-idx &key unanchored-p)
  (let* ((mode (regex-builtin-char-class-mode regex))
         (len (length text))
         ;; Контекст начальной межсимвольной позиции pos-idx
         (ctx (compute-context-mask text pos-idx len mode))
         (dfa (regex-reversed-dfa regex)))
    (dfa-get-start-state dfa ctx unanchored-p)
  )
)

(declaim (inline validate-pass-bounds))
(defun validate-pass-bounds (func-name left-bound-pos right-bound-pos text-len)
  (assert (and (>= left-bound-pos 0)
               (<= left-bound-pos right-bound-pos)
               (<= right-bound-pos text-len))
          ()
          "engine/runner.lisp/~A: Я СДЕЛАЛ УЖАСНУЮ ПРОГРАММУ! Некорректные границы: [~A, ~A] для длины ~A"
          func-name left-bound-pos right-bound-pos text-len)
)

;; ==================================================================================
;; Обёртки над основной логикой проходов.
;; ==================================================================================

(defun lazy-unanchored-direct-pass (regex text left-bound-pos right-bound-pos
                                         &key unanchored-state-id)
  (validate-pass-bounds 'lazy-unanchored-direct-pass left-bound-pos right-bound-pos (length text))
  (let* ((unanchored-state-id (or unanchored-state-id 
                                (compute-direct-start-state-id 
                                      regex text left-bound-pos 
                                      :unanchored-p t))))
    (direct-pass-logic regex text unanchored-state-id 
                      left-bound-pos right-bound-pos
                      :return-on-first-terminal-p t)
  )
)

(defun greedy-anchored-direct-pass (regex text left-bound-pos right-bound-pos
                                    &key anchored-state-id)
  (validate-pass-bounds 'greedy-anchored-direct-pass left-bound-pos right-bound-pos (length text))
  (let* ((anchored-state-id (or anchored-state-id 
                                (compute-direct-start-state-id 
                                      regex text left-bound-pos 
                                      :unanchored-p nil))))
    (direct-pass-logic regex text anchored-state-id 
                      left-bound-pos right-bound-pos
                      :return-on-first-terminal-p nil)
  )
)

(defun lazy-anchored-direct-pass (regex text left-bound-pos right-bound-pos
                                  &key anchored-state-id)
  (validate-pass-bounds 'lazy-anchored-direct-pass left-bound-pos right-bound-pos (length text))
  
)

(defun greedy-unanchored-reverse-pass (regex text left-bound-pos right-bound-pos
                                       &key unanchored-state-id)
  (validate-pass-bounds 'greedy-unanchored-reverse-pass left-bound-pos right-bound-pos (length text))
)

(defun greedy-anchored-reverse-pass (regex text left-bound-pos right-bound-pos
                                     &key anchored-state-id)
  (validate-pass-bounds 'greedy-anchored-reverse-pass left-bound-pos right-bound-pos (length text))
  
)

;; ==================================================================================
;; Основная логика проходов по межсимвольным позициям.
;; ==================================================================================

;; Прямой проход: слева направо по межсимвольным позициям от left-bound-pos до right-bound-pos.
(defun direct-pass-logic (regex text start-state-id left-bound-pos right-bound-pos
                                   &key return-on-first-terminal-p)
  (let ((dfa (regex-direct-dfa regex))
        (mode (regex-builtin-char-class-mode regex))
        (len (length text))
        (eq-table (regex-eq-classes-table regex))
        (curr-pos left-bound-pos)
        (curr-state start-state-id)
        (last-accept nil))
    (loop
      ;; 1. Проверка на принимающее состояние
      (when (dfa-accept-state-p dfa curr-state)
        (setf last-accept curr-pos)
        (when return-on-first-terminal-p
          (return (values curr-pos curr-state))
        )
      )
      ;; 2. Проверка достижения правой границы
      (when (= curr-pos right-bound-pos)
        (return (values last-accept curr-state))
      )
      ;; 3. Вычисление шага и контекста для целевой позиции next-pos
      (let* ((next-pos (1+ curr-pos))
             (next-char-cls (char-to-class-id (char text curr-pos) eq-table))
             (ctx (compute-context-mask text next-pos len mode))
             (next-state (dfa-step-state dfa curr-state next-char-cls ctx)))
        ;; Тупик
        (when (or (null next-state) (< next-state 0))
          (return (values last-accept next-state))
        )
        (setf curr-pos next-pos
              curr-state next-state)
      )
    )
  )
)

;; Обратный проход: справа налево по межсимвольным позициям от right-bound-pos до left-bound-pos
(defun reverse-pass-logic (regex text start-state-id left-bound-pos right-bound-pos
                                    &key return-on-first-terminal-p)
  (let ((dfa (regex-reversed-dfa regex))
        (mode (regex-builtin-char-class-mode regex))
        (len (length text))
        (eq-table (regex-eq-classes-table regex))
        (curr-pos right-bound-pos)
        (curr-state start-state-id)
        (last-accept nil))
    (loop
      ;; 1. Проверка на принимающее состояние
      (when (dfa-accept-state-p dfa curr-state)
        (setf last-accept curr-pos)
        (when return-on-first-terminal-p
          (return (values curr-pos curr-state))
        )
      )
      ;; 2. Проверка достижения левой границы
      (when (= curr-pos left-bound-pos)
        (return (values last-accept curr-state))
      )
      ;; 3. Вычисление шага и контекста для целевой позиции next-pos (слева)
      (let* ((next-pos (1- curr-pos))
             (next-char-cls (char-to-class-id (char text (1- curr-pos)) eq-table))
             (ctx (compute-context-mask text next-pos len mode))
             (next-state (dfa-step-state dfa curr-state next-char-cls ctx)))
        ;; Тупик
        (when (or (null next-state) (< next-state 0))
          (return (values last-accept next-state))
        )
        (setf curr-pos next-pos
              curr-state next-state)
      )
    )
  )
)

;; ==================================================================================
;; Вспомогательные функции для расчета контекста позиций.
;; ==================================================================================

(defun compute-word-boundary-bit (left-ch right-ch mode)
  (let ((left-word-p (and left-ch (word-char-p left-ch mode)))
        (right-word-p (and right-ch (word-char-p right-ch mode))))
    (if (not (eq (not left-word-p) (not right-word-p)))
      #b100000
      0
    )
  )
)

;; Вычисляет битовую маску контекста для межсимвольной позиции p (0 <= p <= len).
;; left-ch  — символ слева от позиции p (text[p-1])
;; right-ch — символ справа от позиции p (text[p])
(defun compute-context-mask (text p len mode)
  (let ((mask 0)
        (left-ch (when (> p 0) (char text (1- p))))
        (right-ch (when (< p len) (char text p))))
    ;; ^
    (when (or (= p 0) (and left-ch (char-newline-p left-ch))) (setf mask (logior mask #b000001)))
    ;; \A
    (when (= p 0) (setf mask (logior mask #b000010)))
    ;; $
    (when (or (= p len) (and right-ch (char-newline-p right-ch))) (setf mask (logior mask #b000100)))
    ;; \Z
    (when (or (= p len) (and (= p (1- len)) right-ch (char-newline-p right-ch))) (setf mask (logior mask #b001000)))
    ;; \z
    (when (= p len) (setf mask (logior mask #b010000)))
    ;; \b
    (logior mask (compute-word-boundary-bit left-ch right-ch mode))
  )
)