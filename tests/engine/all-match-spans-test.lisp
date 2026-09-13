(in-package :regex-library)

;; ============================================================================
;; Вспомогательный хелпер для тестирования all-match-spans
;; ============================================================================

(defun check-all-match-spans (assert-equal-fn pattern text expected
                              &key (mode :unicode) start end shortest-p)
  (let* ((re (compile-regex pattern mode))
         (actual-s (or start 0))
         (actual-e (or end (length text)))
         (actual (all-match-spans re text :start actual-s
                                          :end actual-e
                                          :shortest-p shortest-p))
         (test-name (format nil "Паттерн: '~A', Текст: '~A', Режим: ~A, Диапазон: [~A, ~A), shortest-p: ~A"
                            pattern text mode actual-s actual-e shortest-p)))
    (funcall assert-equal-fn actual expected test-name)
  )
)

;; ============================================================================
;; 0. Пустая строка и совпадения нулевой длины
;; ============================================================================

(defun test-all-match-spans-empty (assert-equal-fn)
  ;; Пустой паттерн и генерация нулевых совпадений на каждой позиции
  (check-all-match-spans assert-equal-fn "" "" '((0 . 0)))
  (check-all-match-spans assert-equal-fn "" "abc" '((0 . 0) (1 . 1) (2 . 2) (3 . 3)))
  (check-all-match-spans assert-equal-fn "abc" "" nil)
  ;; Нулевые совпадения квантификатора * с продвижением указателя
  (check-all-match-spans assert-equal-fn "a*" "bbb" '((0 . 0) (1 . 1) (2 . 2) (3 . 3)))
  (check-all-match-spans assert-equal-fn "a*" "bbb" '((1 . 1) (2 . 2) (3 . 3)) :start 1)
  (check-all-match-spans assert-equal-fn "a*" "aaa" '((0 . 3) (3 . 3)))
  (check-all-match-spans assert-equal-fn "a*" "aaa" '((0 . 0) (1 . 1) (2 . 2) (3 . 3)) :shortest-p t)
)

;; ============================================================================
;; 1. Простые точные вхождения и последовательный поиск
;; ============================================================================

(defun test-all-match-spans-simple (assert-equal-fn)
  ;; Полное совпадение единичного вхождения
  (check-all-match-spans assert-equal-fn "abc" "abc" '((0 . 3)))
  ;; Поиск всех непересекающихся вхождений
  (check-all-match-spans assert-equal-fn "cat" "cat dog cat" '((0 . 3) (8 . 11)))
  (check-all-match-spans assert-equal-fn "123" "a123b123c123" '((1 . 4) (5 . 8) (9 . 12)))
  ;; Отсутствие совпадений
  (check-all-match-spans assert-equal-fn "abc" "xyz" nil)
  (check-all-match-spans assert-equal-fn "test" "testing test" '((0 . 4) (8 . 12)))
)

;; ============================================================================
;; 2. Семантика Leftmost-Longest (Жадный режим для всех вхождений)
;; ============================================================================

(defun test-all-match-spans-leftmost-longest (assert-equal-fn)
  ;; Выбор наибольших совпадений на каждом шаге итерации
  (check-all-match-spans assert-equal-fn "a|ab|abc" "abcde abc" '((0 . 3) (6 . 9)))
  (check-all-match-spans assert-equal-fn "foo|foobar" "foobar foo" '((0 . 6) (7 . 10)))
  ;; Жадный поиск всех групп цифр
  (check-all-match-spans assert-equal-fn "\\d+" "item123 item4567 item8" '((4 . 7) (12 . 16) (21 . 22)))
  (check-all-match-spans assert-equal-fn "a+" "baaaa c aa" '((1 . 5) (8 . 10)))
  (check-all-match-spans assert-equal-fn "ab|bc" "abc bc" '((0 . 2) (4 . 6)))
)

;; ============================================================================
;; 3. Семантика Leftmost-Shortest (Ленивый режим для всех вхождений)
;; ============================================================================

(defun test-all-match-spans-leftmost-shortest (assert-equal-fn)
  ;; Ленивое разбиение повторений на минимальные фрагменты
  (check-all-match-spans assert-equal-fn "a+" "baaaa" '((1 . 2) (2 . 3) (3 . 4) (4 . 5)) :shortest-p t)
  (check-all-match-spans assert-equal-fn "(ab)+" "ababab" '((0 . 2) (2 . 4) (4 . 6)) :shortest-p t)
  (check-all-match-spans assert-equal-fn "a|ab|abc" "abcde" '((0 . 1)) :shortest-p t)
  ;; Ленивый поиск цифр (по 1 цифре на шаг)
  (check-all-match-spans assert-equal-fn "\\d+" "123" '((0 . 1) (1 . 2) (2 . 3)) :shortest-p t)
  (check-all-match-spans assert-equal-fn "[0-9]{2,5}" "123456" '((0 . 2) (2 . 4) (4 . 6)) :shortest-p t)
)

;; ============================================================================
;; 4. Квантификаторы (*, +, ?, {m,n})
;; ============================================================================

(defun test-all-match-spans-quantifiers (assert-equal-fn)
  ;; Интервальные диапазоны повторений
  (check-all-match-spans assert-equal-fn "a{2,4}" "aaaaa" '((0 . 4)))
  (check-all-match-spans assert-equal-fn "a{2,4}" "aaaaa" '((0 . 2) (2 . 4)) :shortest-p t)
  (check-all-match-spans assert-equal-fn "a{2}" "aaaaaa" '((0 . 2) (2 . 4) (4 . 6)))
  ;; Опциональное вхождение ?
  (check-all-match-spans assert-equal-fn "colou?r" "color and colour" '((0 . 5) (10 . 16)))
  (check-all-match-spans assert-equal-fn "(ab)+" "ababa ab" '((0 . 4) (6 . 8)))
)

;; ============================================================================
;; 5. Встроенные символьные классы в режиме :ASCII
;; ============================================================================

(defun test-all-match-spans-builtins-ascii (assert-equal-fn)
  (check-all-match-spans assert-equal-fn "\\d+" "abc123def456ghi789" '((3 . 6) (9 . 12) (15 . 18)) :mode :ascii)
  (check-all-match-spans assert-equal-fn "\\w+" "hello world test" '((0 . 5) (6 . 11) (12 . 16)) :mode :ascii)
  (check-all-match-spans assert-equal-fn "\\s+" "a b  c   d" '((1 . 2) (3 . 5) (6 . 9)) :mode :ascii)
  (check-all-match-spans assert-equal-fn "\\D+" "123abc456def" '((3 . 6) (9 . 12)) :mode :ascii)
  (check-all-match-spans assert-equal-fn "\\w+" "Привет world" '((7 . 12)) :mode :ascii)
)

;; ============================================================================
;; 6. Юникод и кириллические шаблоны в режиме :UNICODE
;; ============================================================================

(defun test-all-match-spans-builtins-unicode (assert-equal-fn)
  ;; Поиск всех слов на кириллице
  (check-all-match-spans assert-equal-fn "\\w+" "Привет мир тест" '((0 . 6) (7 . 10) (11 . 15)) :mode :unicode)
  (check-all-match-spans assert-equal-fn "[а-я]+" "123тест456проверка" '((3 . 7) (10 . 18)) :mode :unicode)
  (check-all-match-spans assert-equal-fn "[А-ЯA-Z]+" "abcТЕСТxyzМОРЕ" '((3 . 7) (10 . 14)) :mode :unicode)
  ;; Эскейпы \uXXXX
  (check-all-match-spans assert-equal-fn "\\u0430+" "бааб аа" '((1 . 3) (5 . 7)) :mode :unicode)
)

;; ============================================================================
;; 7. Ограничение подстроки поиска через :START и :END
;; ============================================================================

(defun test-all-match-spans-subranges (assert-equal-fn)
  (check-all-match-spans assert-equal-fn "\\d+" "123abc456def789" '((0 . 3) (6 . 9) (12 . 15)) :start 0)
  (check-all-match-spans assert-equal-fn "\\d+" "123abc456def789" '((6 . 9) (12 . 15)) :start 3)
  (check-all-match-spans assert-equal-fn "\\d+" "123abc456def789" '((6 . 9)) :start 3 :end 12)
  (check-all-match-spans assert-equal-fn "\\d+" "123abc456def789" nil :start 3 :end 6)
)

;; ============================================================================
;; 8. Комплексные составные шаблоны (Email, IP, HTML)
;; ============================================================================

(defun test-all-match-spans-complex (assert-equal-fn)
  ;; Список всех Email адресов
  (check-all-match-spans assert-equal-fn "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}"
                         "info@example.com and support@test.org" '((0 . 16) (21 . 37)))
  ;; Список всех IP адресов
  (check-all-match-spans assert-equal-fn "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
                         "IP1: 192.168.1.1 IP2: 10.0.0.1" '((5 . 16) (22 . 30)))
  ;; Все HTML теги параграфов
  (check-all-match-spans assert-equal-fn "<p>[^<]*</p>"
                         "<p>one</p><p>two</p>" '((0 . 10) (10 . 20)))
  ;; Поиск отдельных слов по границам \b
  (check-all-match-spans assert-equal-fn "\\bcat\\b" "cat category cat scatter cat" '((0 . 3) (13 . 16) (25 . 28)))
)

;; ============================================================================
;; 9. Позиционные якоря (^, $, \A, \z, \b)
;; ============================================================================

(defun test-all-match-spans-with-anchors (assert-equal-fn)
  ;; Якоря начала и конца текста (срабатывают не более одного раза)
  (check-all-match-spans assert-equal-fn "^cat" "cat dog" '((0 . 3)))
  (check-all-match-spans assert-equal-fn "dog$" "cat dog" '((4 . 7)))
  (check-all-match-spans assert-equal-fn "\\Astart" "start middle end" '((0 . 5)))
  (check-all-match-spans assert-equal-fn "end\\z" "start middle end" '((13 . 16)))
  ;; Границы всех изолированных слов
  (check-all-match-spans assert-equal-fn "\\b\\w+\\b" "one two three" '((0 . 3) (4 . 7) (8 . 13)))
)

;; ============================================================================
;; Точка входа для запуска тестов поиска всех вхождений
;; ============================================================================

(deftest run-all-match-spans-tests "engine/all-match-spans"
  (test-all-match-spans-empty #'assert-equal)
  (test-all-match-spans-simple #'assert-equal)
  (test-all-match-spans-leftmost-longest #'assert-equal)
  (test-all-match-spans-leftmost-shortest #'assert-equal)
  (test-all-match-spans-quantifiers #'assert-equal)
  (test-all-match-spans-builtins-ascii #'assert-equal)
  (test-all-match-spans-builtins-unicode #'assert-equal)
  (test-all-match-spans-subranges #'assert-equal)
  (test-all-match-spans-complex #'assert-equal)
  (test-all-match-spans-with-anchors #'assert-equal)
)