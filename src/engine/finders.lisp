;; regex-find-first, regex-find-last, regex-find-all
(in-package :regex-library)

;; Возвращает пару (match-start . match-end) для первого совпадения по семантике Leftmost или NIL, если совпадений нет.
(defun regex-find-first-match-bounds (regex text &key start end shortest-p)
  (when (null start) (setf start 0))
  (when (null end) (setf end (length text)))
  (assert-bounds (length text) start end "regex-find-first-match-bounds")
  (let ((left-bound start) (right-bound (1- end)))
    (multiple-value-bind (leftmost-k potential-rightest-end)
        (compute-leftmost-k-and-potential-rightest-end regex text left-bound right-bound)
      (when (not leftmost-k) 
        (return-from regex-find-first-match-bounds nil)
      )
      (let ((j
              (if shortest-p
                (lazy-anchored-direct-pass regex text leftmost-k potential-rightest-end)
                (greedy-anchored-direct-pass regex text leftmost-k potential-rightest-end))))
        (values leftmost-k (1+ j)) ;; 1+, т.к. match-end не включается в диапазон
      )
    )
  ) 
)

;; ========================================================
;; Вспомогательные функции
;; ========================================================

(defun compute-leftmost-k-and-potential-rightest-end (regex text left-bound right-bound)
  (multiple-value-bind (first-terminal-k pre)
  (compute-first-terminal-k-and-potential-rightest-end regex text left-bound right-bound)
    (when (not first-terminal-k)
      (return-from compute-leftmost-k-and-potential-rightest-end 
        (values nil nil)
      )
    )

    (let ((leftmost-k (compute-leftmost-k regex text left-bound first-terminal-k pre)))
      (values leftmost-k pre)
    )
  )
)

(defun compute-first-terminal-k-and-potential-rightest-end (regex text left-bound right-bound)
  (multiple-value-bind (first-terminal-k last-state-id)
  (lazy-unanchored-direct-pass regex text left-bound right-bound)
    (when (not first-terminal-k)
      (return-from compute-first-terminal-k-and-potential-rightest-end 
        (values nil nil)
      )
    )

    (let* ((dfa (regex-direct-dfa regex))
           (anchored-state-id (get-dfa-state-id-without-unanchored-start dfa last-state-id))
           (pre (greedy-anchored-direct-pass regex text (1+ first-terminal-k) right-bound 
                                        :anchored-state-id anchored-state-id)))
      (values first-terminal-k pre)
    )
  )
)

(defun compute-leftmost-k (regex text left-bound first-terminal-k pre)
  (let* ((last-state-id (nth-value 1 (greedy-unanchored-reverse-pass regex text first-terminal-k pre)))
         (dfa (regex-reversed-dfa regex))
         (anchored-state-id (get-dfa-state-id-without-unanchored-start dfa last-state-id))
         (leftmost-k (greedy-anchored-reverse-pass regex text left-bound
                      (1- first-terminal-k) :anchored-state-id anchored-state-id)))
    leftmost-k
  )
)

;; Извлекает ID состояния ДКА, исключая из его NFA-множества unanchored-start-state
(defun get-dfa-state-id-without-unanchored-start (dfa state-id)
  (let* ((state (aref (dfa-states dfa) state-id))
         (old-set (dfa-state-nfa-set state))
         (unanchored-id (nfa-unanchored-start-state (dfa-nfa dfa)))
         (old-len (length old-set))
         (new-set (make-array (1- old-len) :element-type 'fixnum))
         (write-idx 0))
    ;; Заполнение нового вектора всеми состояниями НКА, кроме unanchored-id
    (dotimes (read-idx old-len)
      (let ((nfa-id (aref old-set read-idx)))
        (unless (= nfa-id unanchored-id)
          (setf (aref new-set write-idx) nfa-id)
          (incf write-idx)
        )
      )
    )
    (dfa-get-state dfa new-set)
  )
)