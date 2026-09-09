;; Изолирует логику работы с фиксированным вектором start-states (64 состояния, где индекс — битовая маска контекста) объекта dfa.
(in-package :regex-library)

;;; ----------------------------------------------------------------------------
;;; Вычисление и кэширование
;;; ----------------------------------------------------------------------------

(defun compute-start-nfa-closure (dfa initial-context unanchored-p)
  (let* ((nfa (dfa-nfa dfa))
         (start-vec (if unanchored-p (vector (nfa-unanchored-start-state nfa))
                                     (vector (nfa-anchored-start-state nfa)))))
    (compute-nfa-closure nfa start-vec initial-context
                         :queue (dfa-nfa-buffer-queue dfa)
                         :visited (dfa-nfa-buffer-visited dfa))
  )
)

(defun compute-and-cache-start-state! (dfa initial-context packed-key unanchored-p)
  (let ((closure (compute-start-nfa-closure dfa initial-context unanchored-p)))
    (ensure-cache-space! dfa)
    (let ((dfa-start-state-id (get-or-register-dfa-state! dfa closure)))
      (setf (aref (dfa-start-states dfa) packed-key) dfa-start-state-id)
      dfa-start-state-id
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Интерфейс получения стартового состояния
;;; ----------------------------------------------------------------------------

;; Возвращает ID стартового состояния ДКА, заданного маской контекста и типом автомата.
(defun get-dfa-start-state! (dfa initial-context &key (unanchored-p nil))
  (declare (type fixnum initial-context))
  ;; 6 разряд в двоичном представлении индекса начального состояния в start-stated устанавливается в 1,
  ;; если ищем начальное состояние для привязанного ДКА.
  (let* ((packed-key (logior (ash (if unanchored-p 1 0) 6) initial-context)) 
         (cached-id (aref (dfa-start-states dfa) packed-key)))
    (if (>= cached-id 0)
        cached-id
        (compute-and-cache-start-state! dfa initial-context packed-key unanchored-p)
    )
  )
)