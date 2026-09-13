(in-package :regex-library)

;; ============================================================================
;; Вспомогательный хелпер для тестирования split
;; ============================================================================

(defun check-split (assert-equal-fn pattern text expected
                    &key (mode :unicode) start end shortest-p omit-empty-p)
  (let* ((re (compile-regex pattern mode))
         (actual-s (or start 0))
         (actual-e (or end (length text)))
         (actual (split re text :start actual-s
                                :end actual-e
                                :shortest-p shortest-p
                                :omit-empty-p omit-empty-p))
         (test-name (format nil "Паттерн: '~A', Текст: '~A', Режим: ~A, Диапазон: [~A, ~A), shortest-p: ~A, omit-empty-p: ~A"
                            pattern text mode actual-s actual-e shortest-p omit-empty-p)))
    (funcall assert-equal-fn actual expected test-name)
  )
)

;; ============================================================================
;; 0. Пустая строка и совпадения нулевой длины
;; ============================================================================

(defun test-split-empty (assert-equal-fn)
  ;; Пустой текст
  (check-split assert-equal-fn "," "" '(""))
  (check-split assert-equal-fn "," "" nil :omit-empty-p t)
  ;; Пустой паттерн (посимвольное разбиение текста)
  (check-split assert-equal-fn "" "abc" '("" "a" "b" "c" ""))
  (check-split assert-equal-fn "" "abc" '("a" "b" "c") :omit-empty-p t)
  ;; Разделители нулевой длины (квантификатор *)
  (check-split assert-equal-fn "a*" "bbb" '("" "b" "b" "b" ""))
  (check-split assert-equal-fn "a*" "bbb" '("b" "b" "b") :omit-empty-p t)
)

;; ============================================================================
;; 1. Простые точные вхождения и базовое разделение
;; ============================================================================

(defun test-split-simple (assert-equal-fn)
  ;; Обычное деление по единичному символу
  (check-split assert-equal-fn "," "a,b,c" '("a" "b" "c"))
  ;; Деление по составной строке-разделителю
  (check-split assert-equal-fn "::" "one::two::three" '("one" "two" "three"))
  ;; Отсутствие разделителя в тексте (возвращает исходную строку)
  (check-split assert-equal-fn "," "abc" '("abc"))
  ;; Полное совпадение строки с разделителем
  (check-split assert-equal-fn "abc" "abc" '("" ""))
)

;; ============================================================================
;; 2. Разделители на границах и смежные разделители (N разделителей -> N+1 строк)
;; ============================================================================

(defun test-split-edge-delimiters (assert-equal-fn)
  ;; Разделитель в самом начале и в самом конце строки
  (check-split assert-equal-fn "," ",a,b," '("" "a" "b" ""))
  ;; Несколько разделителей подряд (пустые подстроки между ними)
  (check-split assert-equal-fn "," "a,,b" '("a" "" "b"))
  (check-split assert-equal-fn "," "a,,,b" '("a" "" "" "b"))
  ;; Текст состоит исключительно из разделителей
  (check-split assert-equal-fn "," ",," '("" "" ""))
  (check-split assert-equal-fn "," "," '("" ""))
)

;; ============================================================================
;; 3. Фильтрация пустых подстрок через :OMIT-EMPTY-P
;; ============================================================================

(defun test-split-omit-empty (assert-equal-fn)
  ;; Игнорирование пустых подстрок на границах и в середине
  (check-split assert-equal-fn "," ",a,,b," '("a" "b") :omit-empty-p t)
  (check-split assert-equal-fn "," ",," nil :omit-empty-p t)
  (check-split assert-equal-fn "\\s+" "  foo   bar  " '("foo" "bar") :omit-empty-p t)
)

;; ============================================================================
;; 4. Семантика длины совпадения разделителя (:SHORTEST-P)
;; ============================================================================

(defun test-split-shortest-p (assert-equal-fn)
  ;; Жадный разделитель \s+ против ленивого разделителя по 1 пробелу
  (check-split assert-equal-fn "\\s+" "a   b" '("a" "b"))
  (check-split assert-equal-fn "\\s+" "a   b" '("a" "" "" "b") :shortest-p t)
  ;; Жадные и ленивые интервалы в разделителях
  (check-split assert-equal-fn "a{1,3}" "xaaxaaay" '("x" "x" "y"))
  (check-split assert-equal-fn "a{1,3}" "xaaxaaay" '("x" "" "x" "" "" "y") :shortest-p t)
)

;; ============================================================================
;; 5. Встроенные символьные классы в режиме :ASCII
;; ============================================================================

(defun test-split-builtins-ascii (assert-equal-fn)
  ;; Разделитель по пробельным символам \s+
  (check-split assert-equal-fn "\\s+" (format nil "hello~Aworld~Ctest" #\tab #\NewLine)
               '("hello" "world" "test") :mode :ascii)
  ;; Разделитель по не-буквенным символам \W+
  (check-split assert-equal-fn "\\W+" "foo, bar; baz!" '("foo" "bar" "baz" "") :mode :ascii)
  ;; Разделитель по цифровым группам \d+
  (check-split assert-equal-fn "\\d+" "abc123def456ghi" '("abc" "def" "ghi") :mode :ascii)
)

;; ============================================================================
;; 6. Юникод и кириллические шаблоны в режиме :UNICODE
;; ============================================================================

(defun test-split-builtins-unicode (assert-equal-fn)
  ;; Кириллический разделитель
  (check-split assert-equal-fn "или" "яблокоилигрушаилибанан" '("яблоко" "груша" "банан") :mode :unicode)
  ;; Разделитель по небуквенным символам Юникода
  (check-split assert-equal-fn "\\W+" "Привет, Мир! Тест." '("Привет" "Мир" "Тест" "") :mode :unicode)
  ;; Неразрывные пробелы (\u00A0) в классе \s+
  (check-split assert-equal-fn "\\s+" (format nil "первый~Aвторой" #\u00A0) '("первый" "второй") :mode :unicode)
)

;; ============================================================================
;; 7. Ограничение подстроки разбиения через :START и :END
;; ============================================================================

(defun test-split-subranges (assert-equal-fn)
  ;; Выделение строго заданного диапазона [START, END) без префиксов/суффиксов
  (check-split assert-equal-fn "," "AAA,BBB,CCC" '("BBB") :start 4 :end 7)
  (check-split assert-equal-fn "," "AAA,BBB,CCC" '("" "BBB") :start 3 :end 7)
  (check-split assert-equal-fn "," "AAA,BBB,CCC" '("BBB" "") :start 4 :end 8)
  (check-split assert-equal-fn "," "111,222,333,444" '("222" "333") :start 4 :end 11)
)

;; ============================================================================
;; 8. Комплексные составные шаблоны и граница слова (\b)
;; ============================================================================

(defun test-split-complex (assert-equal-fn)
  ;; Разделители по границе слова \b
  (check-split assert-equal-fn "\\b" "a b" '("" "a" " " "b" ""))
  ;; Разделитель по HTML-тегам
  (check-split assert-equal-fn "<[^>]+>" "<h1>Title</h1><p>Text</p>" '("" "Title" "" "Text" ""))
  (check-split assert-equal-fn "<[^>]+>" "<h1>Title</h1><p>Text</p>" '("Title" "Text") :omit-empty-p t)
  ;; Разделители с опциональными пробелами вокруг запятой (CSV-стиль)
  (check-split assert-equal-fn "\\s*,\\s*" "val1 , val2 ,val3" '("val1" "val2" "val3"))
)

;; ============================================================================
;; Точка входа для запуска тестов функции split
;; ============================================================================

(deftest run-split-tests "engine/split"
  (test-split-empty #'assert-equal)
  (test-split-simple #'assert-equal)
  (test-split-edge-delimiters #'assert-equal)
  (test-split-omit-empty #'assert-equal)
  (test-split-shortest-p #'assert-equal)
  (test-split-builtins-ascii #'assert-equal)
  (test-split-builtins-unicode #'assert-equal)
  (test-split-subranges #'assert-equal)
  (test-split-complex #'assert-equal)
)