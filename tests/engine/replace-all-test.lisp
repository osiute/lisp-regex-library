(in-package :regex-library)

;; ============================================================================
;; Вспомогательный хелпер для тестирования replace-all
;; ============================================================================

(defun check-replace-all (assert-equal-fn pattern text replacement expected
                         &key (mode :unicode) start end shortest-p)
  (let* ((re (compile-regex pattern mode))
         (actual-s (or start 0))
         (actual-e (or end (length text)))
         (actual (replace-all re text replacement
                              :start actual-s
                              :end actual-e
                              :shortest-p shortest-p))
         (test-name (format nil "Паттерн: '~A', Замена: '~A', Текст: '~A', Диапазон: [~A, ~A), shortest-p: ~A"
                            pattern replacement text actual-s actual-e shortest-p)))
    (funcall assert-equal-fn actual expected test-name)
  )
)

;; ============================================================================
;; 0. Пустая строка, удаление подстрок и совпадения нулевой длины
;; ============================================================================

(defun test-replace-all-empty (assert-equal-fn)
  ;; Замена в пустом тексте
  (check-replace-all assert-equal-fn "a" "" "X" "")
  ;; Пустой паттерн вставляет замену между всеми символами
  (check-replace-all assert-equal-fn "" "abc" "-" "-a-b-c-")
  ;; Удаление совпадений (replacement = "")
  (check-replace-all assert-equal-fn "a" "banana" "" "bnn")
  (check-replace-all assert-equal-fn "\\d+" "a123b456c" "" "abc")
  ;; Нулевые совпадения квантификатора *
  (check-replace-all assert-equal-fn "a*" "b" "X" "XbX")
)

;; ============================================================================
;; 1. Простая замена и проверка защищённости от рекурсии
;; ============================================================================

(defun test-replace-all-simple (assert-equal-fn)
  ;; Единичные и множественные замены
  (check-replace-all assert-equal-fn "cat" "cat dog cat" "bird" "bird dog bird")
  (check-replace-all assert-equal-fn "foo" "foobarfoobaz" "QUX" "QUXbarQUXbaz")
  ;; Паттерн не найден (строка не изменяется)
  (check-replace-all assert-equal-fn "xyz" "hello world" "abc" "hello world")
  ;; Замена, содержащая сам паттерн (проверка отсутствия зацикливания)
  (check-replace-all assert-equal-fn "a" "banana" "aa" "baanaanaa")
  (check-replace-all assert-equal-fn "1" "121" "11" "11211")
)

;; ============================================================================
;; 2. Сохранение префикса и суффикса при ограничении :START и :END
;; ============================================================================

(defun test-replace-all-subranges (assert-equal-fn)
  ;; Замена строго внутри указанного окна
  (check-replace-all assert-equal-fn "a" "banana" "X" "banXna" :start 2 :end 5)
  (check-replace-all assert-equal-fn "a" "banana" "X" "bXnana" :start 0 :end 3)
  ;; Совпадение левее start игнорируется
  (check-replace-all assert-equal-fn "b" "banana" "X" "banana" :start 1)
  ;; Совпадение правее end игнорируется
  (check-replace-all assert-equal-fn "a" "banana" "X" "bananX" :start 4 :end 6)
)

;; ============================================================================
;; 3. Жадный (:SHORTEST-P NIL) и ленивый (:SHORTEST-P T) режимы
;; ============================================================================

(defun test-replace-all-quantifiers (assert-equal-fn)
  ;; Идущие подряд совпадения
  (check-replace-all assert-equal-fn "a" "aaa" "X" "XXX")
  (check-replace-all assert-equal-fn "aa" "aaaa" "X" "XX")
  ;; Сравнение жадной и ленивой замены
  (check-replace-all assert-equal-fn "a+" "baaab" "X" "bXb")
  (check-replace-all assert-equal-fn "a+" "baaab" "X" "bXXXb" :shortest-p t)
  (check-replace-all assert-equal-fn "a{1,3}" "xaaxaaay" "Z" "xzxZy")
  (check-replace-all assert-equal-fn "a{1,3}" "xaaxaaay" "Z" "xZZxZZZy" :shortest-p t)
)

;; ============================================================================
;; 4. Встроенные символьные классы и Юникод
;; ============================================================================

(defun test-replace-all-builtins (assert-equal-fn)
  ;; ASCII символьные классы
  (check-replace-all assert-equal-fn "\\d+" "Order #123 on 2026-09-13" "N"
                     "Order #N on N-N-N" :mode :ascii)
  (check-replace-all assert-equal-fn "\\s+" "a  b   c" " " "a b c" :mode :ascii)
  ;; Юникод и кириллица
  (check-replace-all assert-equal-fn "[а-я]+" "123тест456" "СЛОВО"
                     "123СЛОВО456" :mode :unicode)
  (check-replace-all assert-equal-fn "\\s+" (format nil "текст1~Aтекст2" #\u00A0) " "
                     "текст1 текст2" :mode :unicode)
)

;; ============================================================================
;; 5. Позиционные якоря (^, $, \b)
;; ============================================================================

(defun test-replace-all-anchors (assert-equal-fn)
  ;; Якоря начала и конца строки
  (check-replace-all assert-equal-fn "^" "World" "Hello " "Hello World")
  (check-replace-all assert-equal-fn "$" "Hello" " World" "Hello World")
  ;; Границы слов \b
  (check-replace-all assert-equal-fn "\\bcat\\b" "the cat in category" "dog"
                     "the dog in category")
)

;; ============================================================================
;; 6. Сложные тексты: Замена изолированных слов и фразеологизмов
;; ============================================================================

(defun test-replace-all-complex-words (assert-equal-fn)
  ;; Замена точных слов "кот" -> "собака", не задевая слова с суффиксами
  (let ((src "У меня есть кот. Мой кот умеет мяукать. Гавкать кот не умеет, ведь это не пёс какой-то.")
        (expected "У меня есть собака. Мой собака умеет мяукать. Гавкать собака не умеет, ведь это не пёс какой-то."))
    (check-replace-all assert-equal-fn "\\bкот\\b" src "собака" expected)
  )
  ;; Замена с учётом регистра букв
  (let ((src "Кот сидит на окне, а другой кот бегает по двору.")
        (expected "Собака сидит на окне, а другой собака бегает по двору."))
    (check-replace-all assert-equal-fn "\\b(К|к)от\\b" src "Собака" expected)
  )
)

;; ============================================================================
;; 7. Сложные тексты: Очистка разметки HTML и нормализация текста
;; ============================================================================

(defun test-replace-all-complex-html (assert-equal-fn)
  ;; Вырезание всех HTML-тегов из документа
  (let ((src "<div><h1>Заголовок</h1><p>Первый параграф.</p><p>Второй параграф.</p></div>")
        (expected "ЗаголовокПервый параграф.Второй параграф."))
    (check-replace-all assert-equal-fn "<[^>]+>" src "" expected)
  )
  ;; Замена HTML тегов на безопасные маркеры
  (let ((src "<p>Текст1</p><p>Текст2</p>")
        (expected "[PARAGRAPH]Текст1[PARAGRAPH][PARAGRAPH]Текст2[PARAGRAPH]"))
    (check-replace-all assert-equal-fn "</?p>" src "[PARAGRAPH]" expected)
  )
)

;; ============================================================================
;; 8. Сложные тексты: Анонимизация персональных данных (Muting/Redaction)
;; ============================================================================

(defun test-replace-all-complex-redaction (assert-equal-fn)
  ;; Скрытие всех Email адресов в тексте обращения
  (let ((src "Пишите на support@test.org или sales@example.com для связи.")
        (expected "Пишите на [СКРЫТО] или [СКРЫТО] для связи."))
    (check-replace-all assert-equal-fn "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}"
                       src "[СКРЫТО]" expected)
  )
  ;; Маскирование IP-адресов в логах сервера
  (let ((src "Ошибка подключения к 192.168.1.100 и 10.0.0.1 на порту 8080.")
        (expected "Ошибка подключения к [IP] и [IP] на порту 8080."))
    (check-replace-all assert-equal-fn "\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"
                       src "[IP]" expected)
  )
)

;; ============================================================================
;; 9. Сложные тексты: Нормализация пробелов и форматирование логов
;; ============================================================================

(defun test-replace-all-complex-normalization (assert-equal-fn)
  ;; Нормализация множественных пробелов и переносов
  (let ((src (format nil "Текст   с   лишними    пробелами~%~%и переносами."))
        (expected "Текст с лишними пробелами и переносами."))
    (check-replace-all assert-equal-fn "\\s+" src " " expected)
  )
  ;; Замена дат формата ДД/ММ/ГГГГ на плейсхолдер
  (let ((src "Отчёты за 12/05/2024 и 15/09/2026 готовы.")
        (expected "Отчёты за [ДАТА] и [ДАТА] готовы."))
    (check-replace-all assert-equal-fn "\\d{2}/\\d{2}/\\d{4}" src "[ДАТА]" expected)
  )
)

;; ============================================================================
;; Точка входа для запуска всех тестов функции replace-all
;; ============================================================================

(deftest run-replace-all-tests "engine/replace-all"
  (test-replace-all-empty #'assert-equal)
  (test-replace-all-simple #'assert-equal)
  (test-replace-all-subranges #'assert-equal)
  (test-replace-all-quantifiers #'assert-equal)
  (test-replace-all-builtins #'assert-equal)
  (test-replace-all-anchors #'assert-equal)
  (test-replace-all-complex-words #'assert-equal)
  (test-replace-all-complex-html #'assert-equal)
  (test-replace-all-complex-redaction #'assert-equal)
  (test-replace-all-complex-normalization #'assert-equal)
)