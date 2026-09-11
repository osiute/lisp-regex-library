;; regex-match-p, regex-search-p
(in-package :regex-library)

(defun regex-search-p (regex text &key left-bound right-bound)
  (when (null left-bound) (setf left-bound 0))
  (when (null right-bound) (setf right-bound (1- (length text))))
  (if (= right-bound (1- left-bound)) ; пустая подстрока
    (assert (< right-bound (length text)) ()
      "regex-search-p: неверно установлены границы: left-bound = ~A, right-bound = ~A, (length text) = ~A"
                                                    left-bound right-bound (length text))
    (assert (and (>= right-bound left-bound) (< right-bound (length text))) ()
      "regex-search-p: неверно установлены границы: left-bound = ~A, right-bound = ~A, (length text) = ~A"
                                                    left-bound right-bound (length text))
  )
  (not (null (lazy-unanchored-direct-pass regex text left-bound right-bound)))                                  
)