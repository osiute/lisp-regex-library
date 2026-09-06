(in-package :regex-library)

(defun build-nfa (pattern)
  (let* ((ast-root (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast-root))
         (nfa (build-nfa-from-ast ast-root eq-table)))
    nfa)
)

(defun make-test-dfa (regex-str &key (max-states 1000))
  (let ((nfa (build-nfa regex-str)))
    (make-lazy-dfa nfa :max-states max-states)
  )
)

(deftest run-dfa-tests "dfa/main (step & cache)"
  (test-dfa-alternation-and-cache #'assert-true #'assert-equal)
  (test-dfa-quantifier-accept #'assert-true #'assert-equal)
  (test-dfa-context-anchors #'assert-true #'assert-equal)
  (test-dfa-cache-flush #'assert-true #'assert-equal)
)

;;; ----------------------------------------------------------------------------
;;; 1. Тестирование "abc|zxc" (Cold/Hot Path, ветвление и dead states)
;;; ----------------------------------------------------------------------------

(defun test-dfa-alternation-and-cache (assert-true-fn assert-equal-fn)
  (let* ((dfa (make-test-dfa "abc|zxc"))
         (s0 (dfa-get-start-state dfa #b0)))
    ;; Проверка ветки "abc"
        (let* ((s1 (dfa-step-state dfa s0 1 #b0))  ; Class 1 = 'a'
          (s2 (dfa-step-state dfa s1 2 #b0))  ; Class 2 = 'b'
          (s3 (dfa-step-state dfa s2 3 #b0))) ; Class 3 = 'c'
      (funcall assert-true-fn (> s1 0) "abc|zxc: переход по 'a' создаёт новое состояние")
      (funcall assert-equal-fn (dfa-accept-state-p dfa s1) nil "abc|zxc: s1 не принимающее")
      (funcall assert-equal-fn (dfa-accept-state-p dfa s2) nil "abc|zxc: s2 не принимающее")
      (funcall assert-true-fn (dfa-accept-state-p dfa s3) "abc|zxc: s3 (\"abc\") принимающее")
      
      ;; Проверка Hot Path (повторный шаг отдается из кэша)
      (let ((s1-cached (dfa-step-state dfa s0 1 #b0)))
        (funcall assert-equal-fn s1-cached s1 "abc|zxc: Hot Path возвращает то же состояние s1")
      )
    )

    ;; Проверка ветки "zxc"
        (let* ((z1 (dfa-step-state dfa s0 7 #b0))  ; Class 7 = 'z'
          (z2 (dfa-step-state dfa z1 5 #b0))  ; Class 5 = 'x'
          (z3 (dfa-step-state dfa z2 3 #b0))) ; Class 3 = 'c'
      (funcall assert-true-fn (> z1 0) "abc|zxc: переход по 'z' создаёт состояние ветки zxc")
      (funcall assert-true-fn (dfa-accept-state-p dfa z3) "abc|zxc: z3 (\"zxc\") принимающее")
    )

    ;; Проверка символов не из алфавита (тупиковые состояния -1)
    (let ((dead-id (dfa-step-state dfa s0 4 #b0))) ; Class 4 отсутствует в НКА
      (funcall assert-equal-fn dead-id -1 "abc|zxc: невалидный класс возвращает -1")
      ;; Проверка Hot Path для тупикового состояния
      (funcall assert-equal-fn (dfa-step-state dfa s0 4 #b0) -1 "abc|zxc: Hot Path для тупика возвращает -1")
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; 2. Тестирование "(abc){3,4}" (Счётчики и флаги accept-p)
;;; ----------------------------------------------------------------------------

(defun test-dfa-quantifier-accept (assert-true-fn assert-equal-fn)
  (let* ((dfa (make-test-dfa "(abc){3,4}"))
         (cur (dfa-get-start-state dfa #b0))
         ;; Массив классов для 4 повторов "abc"
         (pattern #(1 2 3 1 2 3 1 2 3 1 2 3)))
    
    ;; Шаги 1-8: Проверяем, что до 3-го полного повтора состояние НЕ принимающее
    (dotimes (i 8)
      (setf cur (dfa-step-state dfa cur (aref pattern i) #b0))
      (funcall assert-equal-fn (dfa-accept-state-p dfa cur) nil 
               (format nil "(abc){3,4}: шаг ~A не является принимающим" (1+ i)))
    )

    ;; Шаг 9: Завершение 3-го повтора "abc" -> должно стать принимающим
    (setf cur (dfa-step-state dfa cur (aref pattern 8) #b0)) ; Class 3 ('c')
    (funcall assert-true-fn (dfa-accept-state-p dfa cur) 
             "(abc){3,4}: шаг 9 (3-й повтор) становится принимающим")

    ;; Шаги 10-11: Начало 4-го повтора ("a", "b") -> не принимающее
    (setf cur (dfa-step-state dfa cur (aref pattern 9) #b0))
    (funcall assert-equal-fn (dfa-accept-state-p dfa cur) nil "(abc){3,4}: шаг 10 не принимающий")
    (setf cur (dfa-step-state dfa cur (aref pattern 10) #b0))
    (funcall assert-equal-fn (dfa-accept-state-p dfa cur) nil "(abc){3,4}: шаг 11 не принимающий")

    ;; Шаг 12: Завершение 4-го повтора -> снова принимающее
    (setf cur (dfa-step-state dfa cur (aref pattern 11) #b0))
    (funcall assert-true-fn (dfa-accept-state-p dfa cur) 
             "(abc){3,4}: шаг 12 (4-й повтор) остаётся принимающим")

    ;; Шаг 13: 5-я итерация недопустима -> возврат -1
    (let ((over-step (dfa-step-state dfa cur 1 #b0)))
      (funcall assert-equal-fn over-step -1 "(abc){3,4}: 5-й повтор уходит в тупик -1")
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; 3. Тестирование "^.*\bпока!$" (Зависимость от контекста)
;;; ----------------------------------------------------------------------------

(defun test-dfa-context-anchors (assert-true-fn assert-equal-fn)
  (let* ((dfa (make-test-dfa "^.*\\bпока!$"))
         ;; Маски контекстов
         (ctx-start-line #b1)        ; Бит 0: начало строки ^
         (ctx-word-boundary #b100000)    ; Бит 5: граница слова \b
         (ctx-end-line #b100))         ; Бит 2: конец строки $
    
    ;; 1. Проверка контекста начала строки ^
    (let ((s-no-start (dfa-get-start-state dfa #b0)) ; 1 состояния всего. 0 состояние ДКА — множество НКА #(0)
          (s-with-start (dfa-get-start-state dfa ctx-start-line))) ; 2 состояния всего. 1 состояние ДКА — множество НКА #(0 1 2 4 5 6)
      
      ;; Без контекста ^ стартовое состояние не может сделать шаг по .*
      (funcall assert-equal-fn (dfa-step-state dfa s-no-start 12 #b0) -1 ; сохраняет переход из 0 в -1 по class-id = 12
               "^.*\\bпока!$: без контекста ^ шаг по 'п' приводит в тупик")
      
      ;; С контекстом ^ переходим в состояние цикла .*
      (let ((s-loop (dfa-step-state dfa s-with-start 12 #b0))) ; Class 12 = 'п'
        (funcall assert-true-fn (> s-loop 0) ; s-loop = 2 #(2 3 5 6), сохраняет переход из 0 в 2 по class-id = 12
                 "^.*\\bпока!$: с контекстом ^ переход успешный")
      )
    )

    ;; 2. Проверка контекста границы слова \b и конца строки $
    (let* ((s-cur (dfa-get-start-state dfa ctx-start-line)) ; s-cur = 1 #(0 1 2 4 5 6)
           ;; Без границы слова переход по 'п' уходит в тупик или обычный .*
           (dead-b (dfa-step-state dfa s-cur 12 #b0)) ; dead-b = 2 #(2 3 5 6)
           ;; С границей слова \b совершается переход к цепочке "пока!"
           (s-before-p (dfa-step-state dfa s-cur 5 ctx-word-boundary))) ; s-p = 3 #(2 3 5 6 7 8) — стоим прям перед переходом по 'п'
      
      (funcall assert-true-fn (/= dead-b s-before-p) 
               "^.*\\bпока!$: контекст \\b порождает отличное состояние")
      
      ;; Проходим символы 'п', 'о', 'к', 'а'
            (let* ((s-p (dfa-step-state dfa s-before-p 12 #b0)) ; Class 12 = 'п'
                   (s-o (dfa-step-state dfa s-p 11 #b0))   ; Class 11 = 'о'
                   (s-k (dfa-step-state dfa s-o 9 #b0))    ; Class 9 = 'к'
                   (s-a (dfa-step-state dfa s-k 7 #b0))    ; Class 7 = 'а'
             ;; Шаг по '!' без маски $
             (s-excl-no-end (dfa-step-state dfa s-a 5 #b0))
             ;; Шаг по '!' с маской $
             (s-excl-end (dfa-step-state dfa s-a 5 ctx-end-line)))
        
        (funcall assert-equal-fn (dfa-accept-state-p dfa s-excl-no-end) nil 
                 "^.*\\bпока!$: без контекста $ состояние НЕ принимающее")
        (funcall assert-true-fn (dfa-accept-state-p dfa s-excl-end) 
                 "^.*\\bпока!$: с контекстом $ состояние становится принимающим")
      )
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; 4. Тестирование сброса кэша (Cache Flush / max-states)
;;; ----------------------------------------------------------------------------

(defun test-dfa-cache-flush (assert-true-fn assert-equal-fn)
  ;; Создаём ДКА с ограничением всего в 2 состояния
  (let* ((dfa (make-test-dfa "abc|zxc" :max-states 2))
         (s0 (dfa-get-start-state dfa #b0))) ; Занимает узел 0 (count = 1)
    
    (funcall assert-equal-fn (dfa-cache-count dfa) 1 "Cache Flush: кэш содержит 1 состояние (s0)")
    
    ;; Делаем 1-й шаг -> создаёт узел 1 (count = 2, кэш полон)
    (let ((s1 (dfa-step-state dfa s0 1 #b0)))
      (funcall assert-equal-fn (dfa-cache-count dfa) 2 "Cache Flush: кэш заполнен до предела (2/2)")
      
      ;; Делаем 2-й шаг -> должен спровоцировать ensure-cache-space! и сброс кэша
      (let ((s2 (dfa-step-state dfa s1 2 #b0)))
        (funcall assert-true-fn (>= s2 0) "Cache Flush: шаг после сброса кэша выполнен успешно")
        ;; После сброса кэша старые состояния очищаются, создаётся новое под индексом 0
        (funcall assert-true-fn (<= (dfa-cache-count dfa) 2) 
                 "Cache Flush: размер кэша сброшен и не превышает max-states")
      )
    )
  )
)