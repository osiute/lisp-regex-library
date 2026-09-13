(in-package :regex-library)

(defun split (regex text &key (start 0) end shortest-p omit-empty-p)
  "Разбивает TEXT на список подстрок по разделителям, соответствующим REGEX.

  Разбиение осуществляется исключительно на заданном полуинтервале [START, END)
  по семантике Leftmost с учётом стратегии длины совпадения (SHORTEST-P).
  Фрагменты за пределами [START, END) в результат не попадают.

  REGEX — скомпилированное регулярное выражение (объект REGEX);
  TEXT — строка для разбиения;
  START, END — границы полуинтервала [START, END), на котором осуществляется разбиение;
  SHORTEST-P — флаг выборки: NIL — для совпадений по наидлиннейшим разделителям (Longest),
                             T — для совпадений по наикратчайшим (Shortest);
  OMIT-EMPTY-P — предикатный флаг: T — исключать пустые подстроки (\"\") из результата,
                                   NIL — сохранять все подстроки.

  По умолчанию разбивается вся строка с сохранением пустых элементов
  (START = 0, END = (LENGTH TEXT), SHORTEST-P = NIL, OMIT-EMPTY-P = NIL).
  При явном указании NIL для END значение последнего воспринимается как END = (LENGTH TEXT)."
  (let ((real-end (or end (length text))))
    (assert-bounds (length text) start real-end "split")
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