(in-package :regex-library)

(defun contains-p (regex text &key (start 0) end builtin-char-class-mode)
  "Возвращает T, если на участке строки TEXT в полуинтервале [START, END)
  существует хотя бы одна подстрока, соответствующая регулярному выражению REGEX, иначе — NIL.

  Параметры:
    REGEX — скомпилированный объект REGEX или строка с паттерном.
    TEXT — строка для проверки;
    START, END — границы проверяемого полуинтервала [START, END).
    BUILTIN-CHAR-CLASS-MODE — режим встроенных классов (\\d, \\w, \\s и т.д.): :unicode или :ascii.
      Задаётся ТОЛЬКО если REGEX является строкой (по умолчанию :unicode). Если REGEX передаётся
      как скомпилированный объект, передача этого параметра вызовет ошибку.
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT)).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT))."

  (let ((end (or end (length text)))
        (regex (ensure-regex-object regex builtin-char-class-mode 'contains-p)))
    (assert-bounds (length text) start end 'contains-p)
    (let ((first-terminal-pos (lazy-unanchored-direct-pass regex text start end)))
      (not (null first-terminal-pos))
    )
  )
)

(defun matches-p (regex text &key (start 0) end builtin-char-class-mode)
  "Возвращает T, если весь участок строки TEXT в полуинтервале [START, END) полностью
  соответствует регулярному выражению REGEX, иначе — NIL.

  Параметры:
    REGEX — скомпилированный объект REGEX или строка с паттерном.
    TEXT — строка для проверки;
    START, END — границы проверяемого полуинтервала [START, END).
    BUILTIN-CHAR-CLASS-MODE — режим встроенных классов (\\d, \\w, \\s и т.д.): :unicode или :ascii.
      Задаётся ТОЛЬКО если REGEX является строкой (по умолчанию :unicode). Если REGEX передаётся
      как скомпилированный объект, передача этого параметра вызовет ошибку.
  По умолчанию проверяется вся строка (START = 0, END = (LENGTH TEXT)).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT))."
  
  (let ((end (or end (length text)))
        (regex (ensure-regex-object regex builtin-char-class-mode 'matches-p)))
    (assert-bounds (length text) start end 'matches-p)
    (let ((last-terminal-pos (greedy-anchored-direct-pass regex text start end)))
      (and 
        last-terminal-pos
        (= last-terminal-pos end)
      )
    )
  )
)