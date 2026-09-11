(in-package :regex-library)

;; ============================================================================
;; Вспомогательные функции для тестирования regex-search-p
;; ============================================================================

;; Хелпер для проверки выполнения regex-search-p с заданными параметрами
(defun check-search-p (assert-equal-fn pattern text expected
                       &key (mode :unicode) start end)
  (let* ((re (compile-regex pattern mode))
         (actual (if (or start end)
                     (regex-search-p re text
                                     :start (or start 0)
                                     :end (or end (length text)))
                     (regex-search-p re text)
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

(defun test-regex-search-simple (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-search-p assert-equal-fn "abc" "abc" t)
  (check-search-p assert-equal-fn "abc" "hello abc world" t)
  (check-search-p assert-equal-fn "abc" "123abc456" t :start 3 :end 7)

  ;; Негативные тесты (подстрока отсутствует)
  (check-search-p assert-equal-fn "abc" "ab" nil)
  (check-search-p assert-equal-fn "abc" "cba" nil)
  (check-search-p assert-equal-fn "abc" "" nil)
  (check-search-p assert-equal-fn "abc" "123abc456" nil :start 0 :end 4)
)

;; ============================================================================
;; 2. Альтернация ("abc|bcde", "абв|эюя", "012345|234")
;; ============================================================================

(defun test-regex-search-alternation (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-search-p assert-equal-fn "abc|bcde" "abc" t)
  (check-search-p assert-equal-fn "abc|bcde" "bcde" t)
  (check-search-p assert-equal-fn "абв|эюя" "эюя" t :mode :unicode)
  (check-search-p assert-equal-fn "012345|234" "prefix_234_suffix" t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-search-p assert-equal-fn "abc|bcde" "ab" nil)
  (check-search-p assert-equal-fn "абв|эюя" "аб" nil :mode :unicode)
  (check-search-p assert-equal-fn "012345|234" "012" nil)
)

;; ============================================================================
;; 3. Квантификаторы ("(abcde)+", "(авфы)*", "[0-9a-z]{5}", "[0-9a-z]{3,5}", "\w?")
;; ============================================================================

(defun test-regex-search-quantifiers (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-search-p assert-equal-fn "(abcde)+" "abcdeabcde" t)
  (check-search-p assert-equal-fn "(авфы)*" "" t :mode :unicode)
  (check-search-p assert-equal-fn "[0-9a-z]{5}" "test a1b2c test" t)
  (check-search-p assert-equal-fn "[0-9a-z]{3,5}" "1234" t)
  (check-search-p assert-equal-fn "\\w?" "!!!" t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-search-p assert-equal-fn "(abcde)+" "abcd" nil)
  (check-search-p assert-equal-fn "[0-9a-z]{5}" "a1b2" nil)
  (check-search-p assert-equal-fn "[0-9a-z]{3,5}" "12" nil)
)

;; ============================================================================
;; 4. Встроенные символы и Юникод (\w, \s, \S, \W, \d, \D, \uXXXX, \u{XXXXXX}, .)
;; ============================================================================

(defun test-regex-search-builtins-pos (assert-equal-fn)
  ;; Позитивные тесты для встроенных классов
  (check-search-p assert-equal-fn "\\w+" "тест" t :mode :unicode)
  (check-search-p assert-equal-fn "\\d+" "val: 42" t)
  (check-search-p assert-equal-fn "\\s+" "hello world" t)
  (check-search-p assert-equal-fn "\\S+" "  abc  " t)
  (check-search-p assert-equal-fn "\\u0041" "A" t)
  (check-search-p assert-equal-fn "\\u{0430}" "а" t :mode :unicode)
  (check-search-p assert-equal-fn "." "x" t)
)

(defun test-regex-search-builtins-neg (assert-equal-fn)
  ;; Негативные тесты (различие :ascii и :unicode, отсутствие совпадений)
  (check-search-p assert-equal-fn "\\w+" "тест" nil :mode :ascii)
  (check-search-p assert-equal-fn "\\d+" "no digits here" nil)
  (check-search-p assert-equal-fn "\\S+" "   " nil)
  (check-search-p assert-equal-fn "\\u0041" "B" nil)
  (check-search-p assert-equal-fn "." "" nil)
)

;; ============================================================================
;; 5. Сложные комбинированные выражения
;; ============================================================================

(defun test-regex-search-complex (assert-equal-fn)
  ;; Позитивные тесты (подстрока существует)
  (check-search-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "user@domain.com" t :mode :ascii)
  (check-search-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "админ@домен.рф" t :mode :unicode)
  (check-search-p assert-equal-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc123 " t)

  ;; Негативные тесты (подстрока отсутствует)
  (check-search-p assert-equal-fn "^\\w+@\\w+\\.\\w+$" "user@domain" nil :mode :ascii)
  (check-search-p assert-equal-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc1 " nil)
)

;; ============================================================================
;; Точка входа для запуска тестов модуля engine (regex-search-p)
;; ============================================================================

(deftest run-regex-search-p-tests "engine/regex-search-p"
  (test-regex-search-simple #'assert-equal)
  (test-regex-search-alternation #'assert-equal)
  (test-regex-search-quantifiers #'assert-equal)
  (test-regex-search-builtins-pos #'assert-equal)
  (test-regex-search-builtins-neg #'assert-equal)
  (test-regex-search-complex #'assert-equal)
)