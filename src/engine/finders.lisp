(in-package :regex-library)

(defun first-match-span (regex text &key (start 0) (end (length text)) shortest-p)
  "Возвращает точечную пару (START-MATCH . END-MATCH) для первого совпадения REGEX в TEXT.
  Если совпадение не найдено, возвращает NIL.

  Поиск осуществляется на заданном полуинтервале [START, END) по семантике Leftmost
  с учётом стратегии длины совпадения (SHORTEST-P).

  REGEX — скомпилированное регулярное выражение (объект REGEX).
  TEXT — строка для поиска.
  START, END — границы полуинтервала [START, END), на котором осуществляется поиск.
  SHORTEST-P — флаг выборки: NIL для поиска наидлиннейшего совпадения (Longest),
                             T для поиска наикратчайшего (Shortest).
  По умолчанию ищется самое длинное самое левое совпадение на диапазоне всей строки
  (START = 0, END = (LENGTH TEXT), SHORTEST-P = NIL)."
  (assert-bounds (length text) start end "first-match-span")
  (let ((left-bound start) (right-bound (1- end)))
    (multiple-value-bind (leftmost-k potential-rightest-end)
        (compute-leftmost-k-and-potential-rightest-end regex text left-bound right-bound)
      (when (not leftmost-k) 
        (return-from first-match-span nil)
      )
      (let ((j
              (if shortest-p
                (lazy-anchored-direct-pass regex text leftmost-k potential-rightest-end)
                (greedy-anchored-direct-pass regex text leftmost-k potential-rightest-end))))
        (cons leftmost-k (1+ j)) ;; 1+, т.к. match-end не включается в диапазон
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
  (when (= first-terminal-k (1- left-bound)) ; Пустая строка
    (return-from compute-leftmost-k left-bound)
  )
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