;; Алгоритмы перехода по детерминированному графу (по состоянию, символу и контексту).
(in-package :regex-library)


;; Упаковывает пары (class-id, context) в один fixnum.
;; Под маску контекста (0..63) отводятся 6 младших бит.
(declaim (inline pack-transition-key))
(defun pack-transition-key (class-id context)
  (declare (type fixnum class-id context))
  (logior (ash class-id 6) context)
)

;; Вычисляет и возвращает итоговый канонический вектор nfa-состояний для целевого состояния ДКА
(defun compute-target-nfa-set (dfa cur-nfa-set class-id context)
  (let* ((nfa (dfa-nfa dfa))
         (queue (dfa-nfa-buffer-queue dfa))
         (visited (dfa-nfa-buffer-visited dfa))
         (targets-before-closure (compute-nfa-class-id-transitions
                                  nfa cur-nfa-set class-id
                                  :queue queue :visited visited)))
    (if (or (null targets-before-closure) (= 0 (length targets-before-closure)))
      nil
      (compute-nfa-closure nfa targets-before-closure context :queue queue :visited visited)
    )
  )
)

(defun cold-path (dfa cur-dfa-state cur-dfa-transitions class-id context key)
  (let* ((cur-nfa-set (dfa-state-nfa-set cur-dfa-state))
         (target-nfa-set (compute-target-nfa-set dfa cur-nfa-set class-id context)))
    (when (null target-nfa-set)
      (setf (gethash key cur-dfa-transitions) -1)
      (return-from cold-path -1))
    (multiple-value-bind (existing-id foundp) 
        (gethash target-nfa-set (dfa-state-map dfa))
      (if foundp
        (progn
          (setf (gethash key cur-dfa-transitions) existing-id)
          existing-id
        )
        ;; else
        (cold-path-no-dfa-state dfa target-nfa-set cur-dfa-transitions key)
      )
    )
  )
)

(defun cold-path-no-dfa-state (dfa target-nfa-set cur-dfa-transitions key)
  (if (ensure-cache-space! dfa) ; вернёт t, если очистил кэш
    (get-or-register-dfa-state! dfa target-nfa-set)
    ;; else
    (let ((target-id (get-or-register-dfa-state! dfa target-nfa-set)))
      (setf (gethash key cur-dfa-transitions) target-id)
      target-id
    )
  )
)

;; ------------------------------------------------------------------------
;; Основные функции (API для dfa/main.lisp)
;; ------------------------------------------------------------------------

;; Вычисляет целевое состояние ДКА для перехода, возвращая его id.
(defun dfa-step (dfa cur-dfa-state-id class-id context)
  (let* ((cur-state (aref (dfa-states dfa) cur-dfa-state-id))
         (transitions (dfa-state-transitions cur-state))
         (key (pack-transition-key class-id context)))
    (multiple-value-bind (target-state-id foundp) (gethash key transitions)
      (if foundp 
        target-state-id ; hot path
        (cold-path dfa cur-state transitions class-id context key)
      )
    )
  )
)