(in-package :regex-library)

;; ============================================================================
;; Вспомогательные функции для тестирования match-p
;; ============================================================================

;; Хелпер для проверки выполнения match-p через assert-true
(defun check-match-p (assert-true-fn pattern text expected
                      &key (mode :unicode) start end)
  (let* ((re (compile-regex pattern mode))
         (actual (if (or start end)
                     (match-p re text
                                    :start (or start 0)
                                    :end (or end (length text)))
                     (match-p re text)
                 )
         )
         (matched-p (not (null actual)))
         (success (if expected matched-p (not matched-p)))
         (test-name (format nil "Паттерн: '~A', Текст: '~A', Режим: ~A, Ожидалось: ~A"
                            pattern text mode expected)))
    (funcall assert-true-fn success test-name)
  )
)

;; ============================================================================
;; 0. Пустая строка и пустые выражения
;; ============================================================================

(defun test-match-empty (assert-true-fn)
  ;; Позитивные тесты
  (check-match-p assert-true-fn "" "" t)
  (check-match-p assert-true-fn "a*" "" t)
  (check-match-p assert-true-fn "(abc)*" "" t)

  ;; Негативные тесты
  (check-match-p assert-true-fn "abc" "" nil)
  (check-match-p assert-true-fn "" "abc" nil)
)

;; ============================================================================
;; 1. Простые выражения ("abc")
;; ============================================================================

(defun test-match-simple (assert-true-fn)
  ;; Позитивные тесты (полное совпадение)
  (check-match-p assert-true-fn "abc" "abc" t)
  (check-match-p assert-true-fn "abc" "xxabcxx" t :start 2 :end 5)

  ;; Негативные тесты (частичное совпадение не должно проходить match-p)
  (check-match-p assert-true-fn "abc" "xxabcxx" nil)
  (check-match-p assert-true-fn "abc" "ab" nil)
  (check-match-p assert-true-fn "abc" "abcd" nil)
)

;; ============================================================================
;; 2. Альтернация ("abc|bcde", "абв|эюя", "012345|234")
;; ============================================================================

(defun test-match-alternation (assert-true-fn)
  ;; Позитивные тесты
  (check-match-p assert-true-fn "abc|bcde" "abc" t)
  (check-match-p assert-true-fn "abc|bcde" "bcde" t)
  (check-match-p assert-true-fn "абв|эюя" "эюя" t :mode :unicode)
  (check-match-p assert-true-fn "012345|234" "234" t)

  ;; Негативные тесты
  (check-match-p assert-true-fn "abc|bcde" "abcd" nil)
  (check-match-p assert-true-fn "абв|эюя" "аб" nil :mode :unicode)
  (check-match-p assert-true-fn "012345|234" "prefix_234_suffix" nil)
)

;; ============================================================================
;; 3. Квантификаторы ("(abcde)+", "(авфы)*", "[0-9a-z]{5}", "[0-9a-z]{3,5}", "\w?")
;; ============================================================================

(defun test-match-quantifiers (assert-true-fn)
  ;; Позитивные тесты
  (check-match-p assert-true-fn "(abcde)+" "abcdeabcde" t)
  (check-match-p assert-true-fn "(авфы)*" "авфыавфы" t :mode :unicode)
  (check-match-p assert-true-fn "[0-9a-z]{5}" "a1b2c" t)
  (check-match-p assert-true-fn "[0-9a-z]{3,5}" "1234" t)
  (check-match-p assert-true-fn "\\w?" "a" t)
  (check-match-p assert-true-fn "\\w?" "" t)

  ;; Негативные тесты
  (check-match-p assert-true-fn "(abcde)+" "abcdeabcd" nil)
  (check-match-p assert-true-fn "[0-9a-z]{5}" "test a1b2c test" nil)
  (check-match-p assert-true-fn "[0-9a-z]{3,5}" "123456" nil)
)

;; ============================================================================
;; 4. Встроенные символы и Юникод (\w, \s, \S, \W, \d, \D, \uXXXX, \u{XXXXXX}, .)
;; ============================================================================

(defun test-match-builtins-pos (assert-true-fn)
  ;; Позитивные тесты
  (check-match-p assert-true-fn "\\w+" "тест" t :mode :unicode)
  (check-match-p assert-true-fn "\\d+" "12345" t)
  (check-match-p assert-true-fn "\\s+" (format nil "~A~A~A~A~A" #\Space #\Tab #\Page #\Newline #\Return) t :mode :ascii)
  (check-match-p assert-true-fn "\\s+" (format nil "~A~A~A~A~A~A" #\U00A0 #\Space #\Tab #\Page #\Newline #\Return) t :mode :unicode)
  (check-match-p assert-true-fn "\\S+" "abc" t)
  (check-match-p assert-true-fn "\\u0041" "A" t)
  (check-match-p assert-true-fn "\\u{0430}" "а" t :mode :unicode)
  (check-match-p assert-true-fn "." "x" t)
)

(defun test-match-builtins-neg (assert-true-fn)
  ;; Негативные тесты
  (check-match-p assert-true-fn "\\w+" "тест" nil :mode :ascii)
  (check-match-p assert-true-fn "\\d+" "val: 42" nil)
  (check-match-p assert-true-fn "\\S+" "  abc  " nil)
  (check-match-p assert-true-fn "." "xx" nil)
  (check-match-p assert-true-fn "\\s+" (format nil "~A~A~A~A~A~A" #\U00A0 #\Space #\Tab #\Page #\Newline #\Return) nil :mode :ascii)
)

;; ============================================================================
;; 5. Сложные комбинированные выражения
;; ============================================================================

(defun test-match-complex (assert-true-fn)
  ;; Позитивные тесты
  (check-match-p assert-true-fn "\\w+@\\w+\\.\\w+" "user@domain.com" t :mode :ascii)
  (check-match-p assert-true-fn "\\w+@\\w+\\.\\w+" "админ@домен.рф" t :mode :unicode)
  (check-match-p assert-true-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc123 " t)

  ;; Негативные тесты
  (check-match-p assert-true-fn "\\w+@\\w+\\.\\w+" "user@domain" nil :mode :ascii)
  (check-match-p assert-true-fn "(abc|def)+[0-9]{2,4}\\s*" "defabc1 " nil)
)

;; ============================================================================
;; Точка входа для запуска тестов модуля engine (match-p)
;; ============================================================================

(deftest run-match-p-tests "engine/match-p"
  (test-match-empty #'assert-true)
  (test-match-simple #'assert-true)
  (test-match-alternation #'assert-true)
  (test-match-quantifiers #'assert-true)
  (test-match-builtins-pos #'assert-true)
  (test-match-builtins-neg #'assert-true)
  (test-match-complex #'assert-true)
)