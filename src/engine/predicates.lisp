(in-package :regex-library)

(defun contains-p (regex text &key (start 0) (end (length text)))
  "Возвращает T, если на участке строки TEXT в полуинтервале [START, END)
  существует хотя бы одна подстрока, соответствующая регулярному выражению REGEX, иначе — NIL.

  REGEX — скомпилированное регулярное выражение (объект REGEX).
  TEXT — строка для проверки.
  START, END — границы проверяемого полуинтервала [START, END).
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT))."

  (assert-bounds (length text) start end "contains-p")
  (not (null (lazy-unanchored-direct-pass regex text start (1- end)))) ; во внутренней реализации right-bound включается в диапазон                                  
)

(defun match-p (regex text &key (start 0) (end (length text)))
  "Возвращает T, если весь участок строки TEXT в полуинтервале [START, END) полностью
  соответствует регулярному выражению REGEX, иначе — NIL.

  REGEX — скомпилированное регулярное выражение (объект REGEX).
  TEXT — строка для проверки.
  START, END — границы проверяемого полуинтервала [START, END).
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT))."
  (assert-bounds (length text) start end "match-p")
  (let ((k (greedy-anchored-direct-pass regex text start (1- end))))
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