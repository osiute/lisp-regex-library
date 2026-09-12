(in-package :regex-library)

;; ============================================================================
;; Вспомогательные функции для тестирования contains-p
;; ============================================================================

;; Хелпер для проверки выполнения contains-p с заданными параметрами
(defun check-contains-p (assert-equal-fn pattern text expected
                       &key (mode :unicode) start end)
  (let* ((re (compile-regex pattern mode))
         (actual (if (or start end)
                     (contains-p re text
                                     :start (or start 0)
                                     :end (or end (length text)))
                     (contains-p re text)
                 )
         )
    )
    (funcall assert-equal-fn
             (not (null actual))
             expected
             (format nil "Паттерн: '~A', Текст: '~A', Режим: ~A, Границы: [~A, ~A)"
                     pattern text mode (or start 0) (or end (length text))
             )
    )
  )
)

;; ============================================================================
;; 1. Простые выражения ("abc")
;; ============================================================================

(defun test-regex-contains-simple (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-contains-p assert-equal-fn "abc" "abc" t)
  (check-contains-p assert-equal-fn "abc" "hello abc world" t)
  (check-contains-p assert-equal-fn "abc" "123abc456" t :start 3 :end 7)

  ;; Негативные тесты (подстрока отсутствует)
  (check-contains-p assert-equal-fn "abc" "ab" nil)
  (check-contains-p assert-equal-fn "abc" "cba" nil)
  (check-contains-p assert-equal-fn "abc" "" nil)
  (check-contains-p assert-equal-fn "abc" "123abc456" nil :start 0 :end 4)
)

;; ============================================================================
;; 2. Альтернация ("abc|bcde", "абв|эюя", "012345|234")
;; ============================================================================

(defun test-regex-contains-alternation (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-contains-p assert-equal-fn "abc|bcde" "abc" t)
  (check-contains-p assert-equal-fn "abc|bcde" "bcde" t)
  (check-contains-p assert-equal-fn "абв|эюя" "эюя" t :mode :unicode)
  (check-contains-p assert-equal-fn "012345|234" "prefix_234_suffix" t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-contains-p assert-equal-fn "abc|bcde" "ab" nil)
  (check-contains-p assert-equal-fn "абв|эюя" "аб" nil :mode :unicode)
  (check-contains-p assert-equal-fn "012345|234" "012" nil)
)

;; ============================================================================
;; 3. Квантификаторы ("(abcde)+", "(авфы)*", "[0-9a-z]{5}", "[0-9a-z]{3,5}", "\w?")
;; ============================================================================

(defun test-regex-contains-quantifiers (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-contains-p assert-equal-fn "(abcde)+" "abcdeabcde" t)
  (check-contains-p assert-equal-fn "(авфы)*" "" t :mode :unicode)
  (check-contains-p assert-equal-fn "[0-9a-z]{5}" "test a1b2c test" t)
  (check-contains-p assert-equal-fn "[0-9a-z]{3,5}" "1234" t)
  (check-contains-p assert-equal-fn "\\w?" "!!!" t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-contains-p assert-equal-fn "(abcde)+" "abcd" nil)
  (check-contains-p assert-equal-fn "[0-9a-z]{5}" "a1b2" nil)
  (check-contains-p assert-equal-fn "[0-9a-z]{3,5}" "12" nil)
)

;; ============================================================================
;; 4. Встроенные символы и Юникод (\w, \s, \S, \W, \d, \D, \uXXXX, \u{XXXXXX}, .)
;; ============================================================================

(defun test-regex-contains-builtins-pos (assert-equal-fn)
  ;; Позитивные тесты для встроенных классов
  (check-contains-p assert-equal-fn "\\w+" "тест" t :mode :unicode)
  (check-contains-p assert-equal-fn "\\d+" "val: 42" t)
  (check-contains-p assert-equal-fn "\\s+" "hello world" t)
  (check-contains-p assert-equal-fn "\\S+" "  abc  " t)
  (check-contains-p assert-equal-fn "\\u0041" "A" t)
  (check-contains-p assert-equal-fn "\\u{0430}" "а" t :mode :unicode)
  (check-contains-p assert-equal-fn "." "x" t)
)

(defun test-regex-contains-builtins-neg (assert-equal-fn)
  ;; Негативные тесты (различие :ascii и :unicode, отсутствие совпадений)
  (check-contains-p assert-equal-fn "\\w+" "тест" nil :mode :ascii)
  (check-contains-p assert-equal-fn "\\d+" "no digits here" nil)
  (check-contains-p assert-equal-fn "\\S+" "   " nil)
  (check-contains-p assert-equal-fn "\\u0041" "B" nil)
  (check-contains-p assert-equal-fn "." "" nil)
)

;; ============================================================================
;; 5. Сложные комбинированные выражения
;; ============================================================================

(defun test-regex-contains-complex (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-contains-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "user@domain.com" t :mode :ascii)
  (check-contains-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "админ@домен.рф" t :mode :unicode)
  (check-contains-p assert-equal-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc123 " t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-contains-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "user@domain" nil :mode :ascii)
  (check-contains-p assert-equal-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc1 " nil)
)

;; ============================================================================
;; 6. Позиционные якоря (^, $, \A, \z, \b) для contains-p
;; ============================================================================

(defun test-contains-p-with-anchors (assert-true-fn)
  ;; Начало (^) и конец ($) строки/текста
  (check-contains-p assert-true-fn "^cat" "cat dog" t)
  (check-contains-p assert-true-fn "^dog" "cat dog" nil)
  (check-contains-p assert-true-fn "dog$" "cat dog" t)
  (check-contains-p assert-true-fn "cat$" "cat dog" nil)
  ;; Абсолютное начало (\A) и конец (\z)
  (check-contains-p assert-true-fn "\\Astart" "start end" t)
  (check-contains-p assert-true-fn "\\Aend" "start end" nil)
  (check-contains-p assert-true-fn "end\\z" "start end" t)
  ;; Границы слов (\b)
  (check-contains-p assert-true-fn "\\bword\\b" "a word here" t)
  (check-contains-p assert-true-fn "\\bword\\b" "awordhere" nil)
)

;; ============================================================================
;; Точка входа для запуска тестов модуля engine (contains-p)
;; ============================================================================

(deftest run-contains-p-tests "engine/contains-p"
  (test-regex-contains-simple #'assert-equal)
  (test-regex-contains-alternation #'assert-equal)
  (test-regex-contains-quantifiers #'assert-equal)
  (test-regex-contains-builtins-pos #'assert-equal)
  (test-regex-contains-builtins-neg #'assert-equal)
  (test-regex-contains-complex #'assert-equal)
  (test-contains-p-with-anchors #'assert-equal)
)