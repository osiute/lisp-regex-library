(in-package :regex-library)

;; Вспомогательная функция для чтения всего содержимого файла в строку
(defun read-file-as-string (filepath)
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content
    )
  )
)

;; Хелпер для проверки работы count-disjoint-matches на файлах
(defun check-count-disjoint-matches-file (assert-equal-fn pattern filepath expected
                                          &key (mode :unicode) start end shortest-p)
  (let* ((text (read-file-as-string filepath))
         (re (compile-regex pattern mode))
         (actual (count-disjoint-matches re text
                                         :start (or start 0)
                                         :end (or end (length text))
                                         :shortest-p shortest-p))
        )
    (funcall assert-equal-fn
             actual
             expected
             (format nil "Файл: '~A', Паттерн: '~A', Режим: ~A, Границы: [~A, ~A)"
                     filepath pattern mode (or start 0) (or end (length text)))
    )
  )
)

;; Набор тестов для проверки подсчёта непересекающихся совпадений на больших текстах
(deftest run-count-disjoint-matches-tests "engine/"
  (check-count-disjoint-matches-file #'assert-equal "мир|Мир" "texts/war-and-peace.txt" 293 :mode :unicode)
  (check-count-disjoint-matches-file #'assert-equal "\\bмир\\b" "texts/war-and-peace.txt" 32 :mode :unicode)
  (check-count-disjoint-matches-file #'assert-equal "\\b(война|Война)\\b" "texts/war-and-peace.txt" 50 :mode :unicode)
  (check-count-disjoint-matches-file #'assert-equal "том|Том" "texts/war-and-peace.txt" 1891 :mode :unicode)
  (check-count-disjoint-matches-file #'assert-equal "\\b\\w+\\b" "texts/war-and-peace.txt" 292198 :mode :unicode)
)