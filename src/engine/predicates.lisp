(in-package :regex-library)

(defun contains-p (regex text &key (start 0) end)
  "Возвращает T, если на участке строки TEXT в полуинтервале [START, END)
  существует хотя бы одна подстрока, соответствующая регулярному выражению REGEX, иначе — NIL.

  REGEX — скомпилированное регулярное выражение (объект REGEX);
  TEXT — строка для проверки;
  START, END — границы проверяемого полуинтервала [START, END).
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT)).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT))."

  (setf end (or end (length text)))
  (assert-bounds (length text) start end 'contains-p)
  (let ((first-terminal-pos (lazy-unanchored-direct-pass regex text start end)))
    (not (null first-terminal-pos))
  )
)

(defun matches-p (regex text &key (start 0) end)
  "Возвращает T, если весь участок строки TEXT в полуинтервале [START, END) полностью
  соответствует регулярному выражению REGEX, иначе — NIL.

  REGEX — скомпилированное регулярное выражение (объект REGEX);
  TEXT — строка для проверки;
  START, END — границы проверяемого полуинтервала [START, END).
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT)).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT))."
  
  (setf end (or end (length text)))
  (assert-bounds (length text) start end 'match-p)
  (let ((last-terminal-pos (greedy-anchored-direct-pass regex text start end)))
    (and 
      last-terminal-pos
      (= last-terminal-pos end)
    )
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