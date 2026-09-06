;; Очистка детерминизированных (закэшированных) состояний объекта dfa.
(in-package :regex-library)

(declaim (inline dfa-cache-full-p))
(defun dfa-cache-full-p (dfa)
  (>= (length (dfa-states dfa)) 
      (dfa-max-states dfa))
)

(declaim (inline flush-dfa-cache!))
(defun flush-dfa-cache! (dfa)
  (clrhash (dfa-state-map dfa))
  (setf (fill-pointer (dfa-states dfa)) 0)
  (fill (dfa-start-states dfa) -1)
)

;; Возвращает t, если кэш был очищен, иначе nil.
(defun ensure-cache-space! (dfa)
  (when (dfa-cache-full-p dfa)
    (flush-dfa-cache! dfa)
    t
  )
)