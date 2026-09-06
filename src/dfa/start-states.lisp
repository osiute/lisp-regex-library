;; Изолирует логику работы с фиксированным вектором start-states (64 состояния, где индекс — битовая маска контекста) объекта dfa.
(in-package :regex-library)

;;; ----------------------------------------------------------------------------
;;; Вычисление и кэширование
;;; ----------------------------------------------------------------------------

(defun compute-start-nfa-closure (dfa initial-context)
  (let* ((nfa (dfa-nfa dfa))
         (start-vec (vector (nfa-start-state nfa))))
    (compute-nfa-closure nfa start-vec initial-context
                         :queue (dfa-nfa-buffer-queue dfa)
                         :visited (dfa-nfa-buffer-visited dfa))
  )
)

(defun compute-and-cache-start-state! (dfa initial-context)
  (let ((closure (compute-start-nfa-closure dfa initial-context)))
    (ensure-cache-space! dfa)
    (let ((dfa-start-state-id (get-or-register-dfa-state! dfa closure)))
      (setf (aref (dfa-start-states dfa) initial-context) dfa-start-state-id)
      dfa-start-state-id
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Интерфейс получения стартового состояния
;;; ----------------------------------------------------------------------------

;; Возвращает ID стартового состояния ДКА для заданного маской контекста
(defun get-dfa-start-state! (dfa initial-context)
  (declare (type fixnum initial-context))
  (let ((cached-id (aref (dfa-start-states dfa) initial-context)))
    (if (>= cached-id 0)
        cached-id
        (compute-and-cache-start-state! dfa initial-context)
    )
  )
)