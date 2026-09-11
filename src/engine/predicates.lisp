;; regex-match-p, regex-search-p
(in-package :regex-library)

(defun regex-search-p (regex text &key start end)
  (when (null start) (setf start 0))
  (when (null end) (setf end (length text)))
  (assert-bounds (length text) start end "regex-search-p")
  (not (null (lazy-unanchored-direct-pass regex text start (1- end)))) ; во внутренней реализации right-bound включается в диапазон                                  
)

(defun regex-match-p (regex text &key start end)
  (when (null start) (setf start 0))
  (when (null end) (setf end (length text)))
  (assert-bounds (length text) start end "regex-match-p")
  (let ((k (greedy-anchored-direct-pass regex text start (1- end)))) ; во внутренней реализации right-bound включается в диапазон
    (and k (= k (1- end)))
  )
)

;; ==================================================================================
;; Вспомогательные функции
;; ==================================================================================
(defun assert-bounds (text-length start end func-name)
  (assert (and (>= end start) (<= end text-length)) ()
    "~A: неверно установлены границы: start = ~A, end = ~A, text-length = ~A"
                                                  func-name end text-length)
)