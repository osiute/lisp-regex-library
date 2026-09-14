(in-package :regex-library)

(defun replace-all (regex text replacement &key (start 0) end shortest-p builtin-char-class-mode)
  "Возвращает новую строку, где все вхождения REGEX в TEXT заменены на REPLACEMENT.

  Поиск и замена осуществляются на заданном полуинтервале [START, END) по семантике
  Leftmost с учётом стратегии длины совпадения (SHORTEST-P). Префикс [0, START) и
  суффикс [END, (LENGTH TEXT)) сохраняются в итоговой строке без изменений.

  Параметры:
    REGEX — скомпилированный объект REGEX или строка с паттерном.
    TEXT — исходная строка;
    REPLACEMENT — строка, вставляемая вместо каждого совпадения;
    START, END — границы полуинтервала [START, END), на котором осуществляется поиск;
    SHORTEST-P — флаг выборки: NIL — для замен по наидлиннейшим совпадениям (Longest),
                               T — для замен по наикратчайшим (Shortest).
    BUILTIN-CHAR-CLASS-MODE — режим встроенных классов (\\d, \\w, \\s и т.д.): :unicode или :ascii.
      Задаётся ТОЛЬКО если REGEX является строкой (по умолчанию :unicode). Если REGEX передаётся
      как скомпилированный объект, передача этого параметра вызовет ошибку.

  По умолчанию заменяются все самые левые самые длинные непересекающиеся совпадения на диапазоне всей строки
  (START = 0, END = (LENGTH TEXT), SHORTEST-P = NIL).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT)."
  (let ((real-end (or end (length text)))
        (regex (ensure-regex-object regex builtin-char-class-mode 'replace-all)))
    (assert-bounds (length text) start real-end 'replace-all)
    (with-output-to-string (s)
      (let ((i 0))
        (do-match-spans ((m-start m-end) regex text :start start :end real-end :shortest-p shortest-p)
          ;; Локальный префикс
          (write-string text s :start i :end m-start)
          ;; Вхождение → строка замены
          (write-string replacement s)
          (setf i m-end)
        )
        ;; Глобальный суффикс
        (write-string text s :start i :end (length text))
      )
    )
  )
)

(defun split (regex text &key (start 0) end shortest-p omit-empty-p builtin-char-class-mode)
  "Разбивает TEXT на список подстрок по разделителям, соответствующим REGEX.

  Разбиение осуществляется исключительно на заданном полуинтервале [START, END)
  по семантике Leftmost с учётом стратегии длины совпадения (SHORTEST-P).
  Фрагменты за пределами [START, END) в результат не попадают.

  Параметры:
    REGEX — скомпилированный объект REGEX или строка с паттерном.
    TEXT — строка для разбиения;
    START, END — границы полуинтервала [START, END), на котором осуществляется разбиение;
    SHORTEST-P — флаг выборки: NIL — для совпадений по наидлиннейшим разделителям (Longest),
                               T — для совпадений по наикратчайшим (Shortest);
    OMIT-EMPTY-P — предикатный флаг: T — исключать пустые подстроки (\"\") из результата,
                                     NIL — сохранять все подстроки.
    BUILTIN-CHAR-CLASS-MODE — режим встроенных классов (\\d, \\w, \\s и т.д.): :unicode или :ascii.
      Задаётся ТОЛЬКО если REGEX является строкой (по умолчанию :unicode). Если REGEX передаётся
      как скомпилированный объект, передача этого параметра вызовет ошибку.

  По умолчанию разбивается вся строка с сохранением пустых элементов
  (START = 0, END = (LENGTH TEXT), SHORTEST-P = NIL, OMIT-EMPTY-P = NIL).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT)."
  (let ((real-end (or end (length text)))
        (regex (ensure-regex-object regex builtin-char-class-mode 'split)))
    (assert-bounds (length text) start real-end 'split)
    (let ((i start)
          (acc nil))
      (do-match-spans ((m-start m-end) regex text :start start :end real-end :shortest-p shortest-p)
        (let ((part (subseq text i m-start)))
          (when (or (not omit-empty-p) (plusp (length part)))
            (push part acc)))
        (setf i m-end))
      (let ((last-part (subseq text i real-end)))
        (when (or (not omit-empty-p) (plusp (length last-part)))
          (push last-part acc)))
      (nreverse acc)
    )
  )
)