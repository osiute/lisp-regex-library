;; Очистка детерминизированных (закэшированных) состояний объекта dfa.
(in-package :regex-library)

(defun dfa-cache-full-p (dfa)
  (>= (length (dfa-states dfa)) 
      (dfa-max-states dfa))
)

(defun flush-dfa-cache! (dfa)
  (clrhash (dfa-state-map dfa))
  (setf (fill-pointer (dfa-states dfa)) 0)
  (reset-start-states! dfa)
)

(defun ensure-cache-space! (dfa)
  (when (dfa-cache-full-p dfa)
    (flush-dfa-cache! dfa)
  )
)