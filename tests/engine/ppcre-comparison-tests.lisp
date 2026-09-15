(in-package :regex-library)

;; =========================================================================
;; 1. Вспомогательные функции чтения файлов и генерации строк
;; =========================================================================

(defun read-file-as-string (filepath)
  "Считывает всё содержимое файла по указанному пути в строку."
  (with-open-file (stream filepath :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content
    )
  )
)

(defun make-failing-string (count)
  "Создаёт патологическую строку из COUNT символов 'a' и символа 'X' на конце."
  (concatenate 'string (make-string count :initial-element #\a) "X"))

;; =========================================================================
;; 2. Вспомогательные функции замера времени и подсчёта совпадений
;; =========================================================================

(defun measure-execution-seconds (fn)
  "Выполняет FN без аргументов и возвращает (VALUES RESULT SECONDS)."
  (let* ((start-time (get-internal-real-time))
         (result (funcall fn))
         (end-time (get-internal-real-time))
         (elapsed (/ (- end-time start-time)
                     (float internal-time-units-per-second))))
    (values result elapsed)
  )
)

(defun benchmark-text-file (pattern filepath &key (mode :unicode))
  "Выполняет замеры времени и совпадений обоих движков на содержимом файла."
  (let ((text (read-file-as-string filepath)))
    (multiple-value-bind (actual my-time)
        (measure-execution-seconds
         (lambda () (count-disjoint-matches (compile-regex pattern mode) text)))
      (multiple-value-bind (expected ppcre-time)
          (measure-execution-seconds
           (lambda () (cl-ppcre:count-matches pattern text)))
        (values actual my-time expected ppcre-time))
    )
  )
)

(defun check-and-benchmark-ppcre (assert-equal-fn pattern filepath &key (mode :unicode))
  "Сравнивает число совпадений с cl-ppcre и выводит результаты вместе с таймингами."
  (multiple-value-bind (actual my-time expected ppcre-time)
      (benchmark-text-file pattern filepath :mode mode)
    (format t "  [Совпадений: ~D] '~A' | REGEX-LIBRARY: ~,4Fc | PPCRE: ~,4Fc~%"
            actual pattern my-time ppcre-time)
    (funcall assert-equal-fn actual expected
             (format nil "PPCRE Match | Файл: '~A', Паттерн: '~A'" filepath pattern))))

(defun benchmark-pathological-pair (pattern text)
  "Возвращает количество совпадений и время работы обоих алгоритмов."
  (multiple-value-bind (count time-my)
      (measure-execution-seconds
       (lambda () (count-disjoint-matches (compile-regex pattern :unicode) text))
      )
    (multiple-value-bind (expected time-ppcre)
        (measure-execution-seconds
         (lambda () (cl-ppcre:count-matches pattern text)))
      (declare (ignore expected))
      (values count time-my time-ppcre)
    )
  )
)

(defun run-single-pathological-step (pattern n)
  "Выполняет один шаг стресс-теста для длины N и выводит совпадения с временем."
  (let ((text (make-failing-string n)))
    (multiple-value-bind (count time-my time-ppcre)
        (benchmark-pathological-pair pattern text)
      (format t "  N = ~2D | Совпадений: ~D | REGEX-LIBRARY: ~,6Fc | PPCRE: ~,6Fc~%"
              n count time-my time-ppcre)
    )
  )
)

;; =========================================================================
;; 3. Тестовые сюиты для отчёта
;; =========================================================================

(deftest run-ppcre-war-and-peace-suite "engine/ppcre-war-and-peace"
  "Набор 1: тесты для проверки правильной работы подсчёта вхождений.
  В качестве текста используется произведение «Война и мир»."
  (let ((file-path "texts/war-and-peace.txt"))
    (check-and-benchmark-ppcre #'assert-equal 
                              "Пьер|Наташа|Болконский|Ростов|Безухов" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "князь|княгиня|граф|графиня" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "\\b(М|м)ир\\b" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "\\b(В|в)ойна\\b" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "\\b[А-Яа-я]+(ов|ева|ин)\\b" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "\\b\\w+\\b" 
                              file-path)
    (format t "~%")                              
    (check-and-benchmark-ppcre #'assert-equal 
                              "\\w" 
                              file-path)
  )
)

(defun run-ppcre-pathological-suite (&key (start 12) (end 28) (step 2))
  "Набор 2: Динамический замер роста времени при катастрофическом возврате.~%"
  (format t "=== Модуль engine/ppcre-pathological-suite: проверка катастрафиеского возврата ===~%")
  (format t "=== В качестве паттерна регулярного выражения используется '^(a+)+$' ===~%")
  (format t "=== Начало тестирования: ===~%")
  (loop for n from start to end by step do
    (run-single-pathological-step "^(a+)+$" n)
  )
  (format t "=== Модуль engine/ppcre-pathological-suite: тестирование завершено ===~%")
  t
)