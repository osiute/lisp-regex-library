;; regex-match-p, regex-search-p
(in-package :regex-library)

(defun regex-search-p (regex text &key left-bound right-bound)
  (when (null left-bound) (setf left-bound 0))
  (when (null right-bound) (setf right-bound (1- (length text))))
  (assert-bounds (length text) left-bound right-bound "regex-search-p")
  (not (null (lazy-unanchored-direct-pass regex text left-bound right-bound)))                                  
)

(defun regex-match-p (regex text &key left-bound right-bound)
  (when (null left-bound) (setf left-bound 0))
  (when (null right-bound) (setf right-bound (1- (length text))))
  (assert-bounds (length text) left-bound right-bound "regex-match-p")
  (let ((k (greedy-anchored-direct-pass regex text left-bound right-bound)))
    (and k (= k right-bound))
  )
)

;; ==================================================================================
;; Вспомогательные функции
;; ==================================================================================
(defun assert-bounds (text-length left-bound right-bound func-name)
  (if (= right-bound (1- left-bound)) ; пустая подстрока
    (assert (< right-bound text-length) ()
      "~A: неверно установлены границы: left-bound = ~A, right-bound = ~A, text-length = ~A"
                                                    func-name left-bound right-bound text-length)
    (assert (and (>= right-bound left-bound) (< right-bound text-length)) ()
      "~A: неверно установлены границы: left-bound = ~A, right-bound = ~A, text-length = ~A"
                                                    func-name right-bound text-length)
  )
)