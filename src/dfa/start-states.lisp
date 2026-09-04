;; Изолирует логику работы с фиксированным вектором start-states (64 состояния, где индекс — битовая маска контекста) объекта dfa.
(in-package :regex-library)

;;; ----------------------------------------------------------------------------
;;; Инициализация и сброс
;;; ----------------------------------------------------------------------------

(defun reset-start-states! (dfa)
  (fill (dfa-start-states dfa) -1)
)

;;; ----------------------------------------------------------------------------
;;; Вычисление и кэширование
;;; ----------------------------------------------------------------------------

(defun compute-start-nfa-closure (dfa initial-context)
  (let* ((nfa (dfa-nfa dfa))
         (start-vec (vector (nfa-start-state nfa))))
    (compute-nfa-closure nfa start-vec initial-context
                         :queue (dfa-closure-queue dfa)
                         :visited (dfa-closure-visited dfa))
  )
)

(defun compute-and-cache-start-state! (dfa initial-context)
  (let* ((closure (compute-start-nfa-closure dfa initial-context))
         ;; Сохранение состояния в общем реестре ДКА
         (state-id (get-or-register-dfa-state! dfa closure)))
    ;; Сохранение вычисленного ID под соответствующим индексом контекста
    (setf (aref (dfa-start-states dfa) initial-context) state-id)
    state-id
  )
)

;;; ----------------------------------------------------------------------------
;;; Интерфейс получения стартового состояния
;;; ----------------------------------------------------------------------------

;; Возвращает ID стартового состояния ДКА для заданного маской контекста
(defun get-dfa-start-state (dfa initial-context)
  (let ((cached-id (aref (dfa-start-states dfa) initial-context)))
    (if (>= cached-id 0)
        cached-id
        (compute-and-cache-start-state! dfa initial-context)
    )
  )
)