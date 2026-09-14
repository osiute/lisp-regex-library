(in-package :regex-library)

;; ============================================================================
;; Вспомогательный хелпер для тестирования first-match-span
;; ============================================================================

(defun check-first-match-span (assert-equal-fn pattern text expected
                         &key (mode :unicode) start end shortest-p)
  (let* ((re (compile-regex pattern mode))
         (actual-s (or start 0))
         (actual-e (or end (length text)))
         (actual (first-match-span re text :start actual-s
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

(defun test-first-match-span-empty (assert-equal-fn)
  ;; Пустой паттерн и пустой/непустой текст
  (check-first-match-span assert-equal-fn "" "" '(0 . 0))
  (check-first-match-span assert-equal-fn "" "abc" '(0 . 0))
  (check-first-match-span assert-equal-fn "abc" "" nil)
  ;; Нулевые совпадения квантификатора *
  (check-first-match-span assert-equal-fn "a*" "" '(0 . 0))
  (check-first-match-span assert-equal-fn "a*" "bbb" '(0 . 0))
  (check-first-match-span assert-equal-fn "a*" "bbb" '(1 . 1) :start 1)
  (check-first-match-span assert-equal-fn "a*" "aaa" '(0 . 3))
  (check-first-match-span assert-equal-fn "a*" "aaa" '(0 . 0) :shortest-p t)
)

;; ============================================================================
;; 1. Простые точные вхождения и смещение поиска
;; ============================================================================

(defun test-first-match-span-simple (assert-equal-fn)
  ;; Полное совпадение
  (check-first-match-span assert-equal-fn "abc" "abc" '(0 . 3))
  ;; Вхождение в середине строки
  (check-first-match-span assert-equal-fn "abc" "xxabcxx" '(2 . 5))
  ;; Отсутствие совпадения
  (check-first-match-span assert-equal-fn "abc" "xyz" nil)
  ;; Несколько одинаковых вхождений (должно находит первое)
  (check-first-match-span assert-equal-fn "cat" "cat dog cat" '(0 . 3))
  (check-first-match-span assert-equal-fn "cat" "dog cat bird cat" '(4 . 7))
  (check-first-match-span assert-equal-fn "123" "a123b123c" '(1 . 4))
  (check-first-match-span assert-equal-fn "test" "testing test" '(0 . 4))
)

;; ============================================================================
;; 2. Семантика Leftmost-Longest (Жадный режим)
;; ============================================================================

(defun test-first-match-span-leftmost-longest (assert-equal-fn)
  ;; Выбор наидлиннейшей ветки из одного старта
  (check-first-match-span assert-equal-fn "1234|012|0123|3245678" "0123456789" '(0 . 4))
  (check-first-match-span assert-equal-fn "a|ab|abc" "abcde" '(0 . 3))
  (check-first-match-span assert-equal-fn "foo|foobar" "foobar" '(0 . 6))
  ;; Жадный поиск цифр в середине текста
  (check-first-match-span assert-equal-fn "\\d+" "fjkdasjfasdjfasd53fjdhasfkdshafjkasd432432432" '(16 . 18))
  ;; Преимущество более левого старта над более длинным поздним
  (check-first-match-span assert-equal-fn "a|bbbbb" "a bbbbb" '(0 . 1))
  (check-first-match-span assert-equal-fn "a|b" "ba" '(0 . 1))
  (check-first-match-span assert-equal-fn "ab|bc" "abc" '(0 . 2))
)

;; ============================================================================
;; 3. Семантика Leftmost-Shortest (Ленивый режим)
;; ============================================================================

(defun test-first-match-span-leftmost-shortest (assert-equal-fn)
  ;; Выбор короткой ветки из одного старта
  (check-first-match-span assert-equal-fn "1234|012|0123|3245678" "0123456789" '(0 . 3) :shortest-p t)
  (check-first-match-span assert-equal-fn "a|ab|abc" "abcde" '(0 . 1) :shortest-p t)
  (check-first-match-span assert-equal-fn "foo|foobar" "foobar" '(0 . 3) :shortest-p t)
  ;; Ленивый поиск 1 цифры (минимальное удовлетворение \d+)
  (check-first-match-span assert-equal-fn "\\d+" "fjkdasjfasdjfasd53fjdhasfkdshafjkasd432432432" '(16 . 17) :shortest-p t)
  (check-first-match-span assert-equal-fn "a+" "baaaa" '(1 . 2) :shortest-p t)
  (check-first-match-span assert-equal-fn "(ab)+" "ababab" '(0 . 2) :shortest-p t)
  (check-first-match-span assert-equal-fn "[0-9]{2,5}" "123456" '(0 . 2) :shortest-p t)
)

;; ============================================================================
;; 4. Квантификаторы (*, +, ?, {m,n})
;; ============================================================================

(defun test-first-match-span-quantifiers (assert-equal-fn)
  ;; Точный и интервальный диапозоны повторений
  (check-first-match-span assert-equal-fn "a{2,4}" "aaaaa" '(0 . 4))
  (check-first-match-span assert-equal-fn "a{2,4}" "aaaaa" '(0 . 2) :shortest-p t)
  (check-first-match-span assert-equal-fn "a{3}" "aaaaa" '(0 . 3))
  (check-first-match-span assert-equal-fn "a{3}" "aaaaa" '(0 . 3) :shortest-p t)
  ;; Опциональное вхождение ?
  (check-first-match-span assert-equal-fn "colou?r" "color" '(0 . 5))
  (check-first-match-span assert-equal-fn "colou?r" "colour" '(0 . 6))
  (check-first-match-span assert-equal-fn "(ab)+" "ababa" '(0 . 4))
)

;; ============================================================================
;; 5. Встроенные символьные классы в режиме :ASCII
;; ============================================================================

(defun test-first-match-span-builtins-ascii (assert-equal-fn)
  (check-first-match-span assert-equal-fn "\\d+" "abc123def456" '(3 . 6) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\w+" "   hello   world" '(3 . 8) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\s+" (format nil "foo ~A~Abar" #\tab #\NewLine) '(3 . 6) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\D+" "123abc456" '(3 . 6) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\W+" "abc!!!def" '(3 . 6) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\S+" "   test   " '(3 . 7) :mode :ascii)
  (check-first-match-span assert-equal-fn "\\w+" "Привет" nil :mode :ascii)
)

;; ============================================================================
;; 6. Юникод и кириллические шаблоны в режиме :UNICODE
;; ============================================================================

(defun test-first-match-span-builtins-unicode (assert-equal-fn)
  ;; Кириллица распознается как \w
  (check-first-match-span assert-equal-fn "\\w+" "   Привет   мир" '(3 . 9) :mode :unicode)
  (check-first-match-span assert-equal-fn "[а-я]+" "123тест456" '(3 . 7) :mode :unicode)
  (check-first-match-span assert-equal-fn "[А-ЯA-Z]+" "abcТЕСТxyz" '(3 . 7) :mode :unicode)
  ;; Эскейп-последовательности \uXXXX
  (check-first-match-span assert-equal-fn "\\u0430+" "баааб" '(1 . 4) :mode :unicode)
  (check-first-match-span assert-equal-fn "\\u0430+" "баааб" '(1 . 2) :mode :unicode :shortest-p t)
  (check-first-match-span assert-equal-fn "\\s+" (format nil "служебный~Aпробел" #\u00A0) '(9 . 10) :mode :unicode)
)

;; ============================================================================
;; 7. Ограничение подстроки поиска через :START и :END
;; ============================================================================

(defun test-first-match-span-subranges (assert-equal-fn)
  (check-first-match-span assert-equal-fn "\\d+" "123abc456def789" '(0 . 3) :start 0)
  (check-first-match-span assert-equal-fn "\\d+" "123abc456def789" '(6 . 9) :start 3)
  (check-first-match-span assert-equal-fn "\\d+" "123abc456def789" '(6 . 8) :start 3 :end 8)
  (check-first-match-span assert-equal-fn "\\d+" "123abc456def789" nil :start 3 :end 6)
  ;; Поиск пустой строки на суженном интервале
  (check-first-match-span assert-equal-fn "a*" "bbb" '(2 . 2) :start 2 :end 3)
)

;; ============================================================================
;; 8. Комплексные составные шаблоны (Email, IP, даты, HTML)
;; ============================================================================

(defun test-first-match-span-complex (assert-equal-fn)
  ;; Email адрес
  (check-first-match-span assert-equal-fn "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}"
                    "Contact us at info@example.com or support@test.org" '(14 . 30))
  ;; IP адрес
  (check-first-match-span assert-equal-fn "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
                    "Server IP is 192.168.1.100 active" '(13 . 26))
  ;; HTML теги
  (check-first-match-span assert-equal-fn "<p>[^<]*</p>"
                    "<div><p>hello world</p></div>" '(5 . 23))
  ;; Изолированные слова c границами \b
  (check-first-match-span assert-equal-fn "\\bcat\\b" "category cat scatter" '(9 . 12))
  (check-first-match-span assert-equal-fn "\\b\\w+\\b" "  hello  " '(2 . 7))
)

;; ============================================================================
;; 9. Позиционные якоря (^, $, \A, \z, \b)
;; ============================================================================

(defun test-first-match-span-with-anchors (assert-equal-fn)
  ;; Начало (^) и конец ($) строки/текста
  (check-first-match-span assert-equal-fn "^cat" "cat dog" '(0 . 3))
  (check-first-match-span assert-equal-fn "^dog" "cat dog" nil)
  (check-first-match-span assert-equal-fn "dog$" "cat dog" '(4 . 7))
  (check-first-match-span assert-equal-fn "cat$" "cat dog" nil)
  ;; Абсолютное начало (\A) и конец (\z)
  (check-first-match-span assert-equal-fn "\\Astart" "start end" '(0 . 5))
  (check-first-match-span assert-equal-fn "end\\z" "start end" '(6 . 9))
  ;; Границы слов (\b)
  (check-first-match-span assert-equal-fn "\\bword\\b" "a word here" '(2 . 6))
  (check-first-match-span assert-equal-fn "\\bword\\b" "awordhere" nil)
)

(in-package :regex-library)

;; ============================================================================
;; 10. Чистые позиции якорей (нулевая длина совпадения)
;; ============================================================================

(defun test-first-match-span-anchors-pure (assert-equal-fn)
  ;; Начало строки ^ и \A
  (check-first-match-span assert-equal-fn "^" "hello" '(0 . 0))
  (check-first-match-span assert-equal-fn "^" "" '(0 . 0))
  (check-first-match-span assert-equal-fn "\\A" "hello" '(0 . 0))
  (check-first-match-span assert-equal-fn "\\A" "" '(0 . 0))
  
  ;; Конец строки $ и \z
  (check-first-match-span assert-equal-fn "$" "hello" '(5 . 5))
  (check-first-match-span assert-equal-fn "$" "" '(0 . 0))
  (check-first-match-span assert-equal-fn "\\z" "hello" '(5 . 5))
  (check-first-match-span assert-equal-fn "\\z" "" '(0 . 0))

  ;; Границы слов \b и не-границы \B
  (check-first-match-span assert-equal-fn "\\b" "a" '(0 . 0))
  (check-first-match-span assert-equal-fn "\\b" "a" '(1 . 1) :start 1)
  (check-first-match-span assert-equal-fn "\\b" "  abc" '(2 . 2))
  (check-first-match-span assert-equal-fn "\\B" "abc" '(1 . 1))
  (check-first-match-span assert-equal-fn "\\B" "a" nil)
)

;; ============================================================================
;; 11. Составные паттерны: текст + якори (^text, text$, ^text$)
;; ============================================================================

(defun test-first-match-span-anchors-composite (assert-equal-fn)
  ;; Совпадение с началом (^ и \A)
  (check-first-match-span assert-equal-fn "^hello" "hello world" '(0 . 5))
  (check-first-match-span assert-equal-fn "^world" "hello world" nil)
  (check-first-match-span assert-equal-fn "\\Astart" "start process" '(0 . 5))
  (check-first-match-span assert-equal-fn "\\Aprocess" "start process" nil)

  ;; Совпадение с концом ($ и \z)
  (check-first-match-span assert-equal-fn "world$" "hello world" '(6 . 11))
  (check-first-match-span assert-equal-fn "hello$" "hello world" nil)
  (check-first-match-span assert-equal-fn "stop\\z" "full stop" '(5 . 9))
  (check-first-match-span assert-equal-fn "full\\z" "full stop" nil)

  ;; Строгое полное совпадение (^text$ и \Atext\z)
  (check-first-match-span assert-equal-fn "^exact$" "exact" '(0 . 5))
  (check-first-match-span assert-equal-fn "^exact$" "exact match" nil)
  (check-first-match-span assert-equal-fn "^exact$" "not exact" nil)
  (check-first-match-span assert-equal-fn "\\A12345\\z" "12345" '(0 . 5))
  (check-first-match-span assert-equal-fn "\\A12345\\z" "123456" nil)
)

;; ============================================================================
;; 12. Выделение слов через \b / \B и поддиапазоны (:START / :END)
;; ============================================================================

(defun test-first-match-span-anchors-words-and-ranges (assert-equal-fn)
  ;; Выделение изолированных слов через \b
  (check-first-match-span assert-equal-fn "\\bcat\\b" "cat" '(0 . 3))
  (check-first-match-span assert-equal-fn "\\bcat\\b" "a cat here" '(2 . 5))
  (check-first-match-span assert-equal-fn "\\bcat\\b" "copycat" nil)
  (check-first-match-span assert-equal-fn "\\bcat\\b" "category" nil)
  (check-first-match-span assert-equal-fn "\\bкот\\b" "кот котик кот" '(0 . 3))
  (check-first-match-span assert-equal-fn "\\bкот\\b" "кот котик кот" '(10 . 13) :start 3)

  ;; Поиск внутри слов с помощью \B
  (check-first-match-span assert-equal-fn "\\Bcat\\B" "scatty" '(1 . 4))
  (check-first-match-span assert-equal-fn "\\Bкот\\B" "мяукотгав" '(3 . 6))
  (check-first-match-span assert-equal-fn "\\Bкот\\B" "кот" nil)
)

;; ============================================================================
;; Точка входа для запуска тестов поиска первого вхождения
;; ============================================================================

(deftest run-first-match-span-tests "engine/first-match-span"
  (test-first-match-span-empty #'assert-equal)
  (test-first-match-span-simple #'assert-equal)
  (test-first-match-span-leftmost-longest #'assert-equal)
  (test-first-match-span-leftmost-shortest #'assert-equal)
  (test-first-match-span-quantifiers #'assert-equal)
  (test-first-match-span-builtins-ascii #'assert-equal)
  (test-first-match-span-builtins-unicode #'assert-equal)
  (test-first-match-span-subranges #'assert-equal)
  (test-first-match-span-complex #'assert-equal)
  (test-first-match-span-with-anchors #'assert-equal)
  (test-first-match-span-anchors-pure #'assert-equal)
  (test-first-match-span-anchors-composite #'assert-equal)
  (test-first-match-span-anchors-words-and-ranges #'assert-equal)

)