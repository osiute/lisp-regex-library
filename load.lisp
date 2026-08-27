;; Автоматическое подключение библиотеки и тестов (чтобы каждый раз не прописывать подключение библиотек в sbcl вручную)
(require :asdf)

(push #p"./" asdf:*central-registry*) ; Включение текущей директории в диапазон поиска проектов для asdf

(format t "~%[1/2] Загрузка основной системы...~%")
(asdf:load-system :regex-library)

(format t "~%[2/2] Загрузка тестовой системы...~%")
(asdf:load-system :regex-library/tests)

(format t "~%=== Проект успешно загружен! ===~%")
(format t "Доступные тесты:~%")
(format t "~2TParser:~%")
(format t "~4T(run-state-tests)~%")
(format t "~4T(run-char-class-tests)~%")
(format t "~4T(run-range-quantifier-tests)~%")
(format t "~4T(run-grammar-atoms-tests)~%")
(format t "~4T(run-grammar-quantification-tests)~%")
(format t "~4T(run-grammar-concatenation-tests)~%")
(format t "~4T(run-grammar-expression-tests)~%")
(format t "~4T(run-parser-tests)~%")
(format t "~2TUnicode:~%")
(format t "~2TNFA:~%")
(format t "~2TDFA:~%")
(format t "~2TEngine (main):~%~%")
(format t "!!! НЕОБХОДИМО ПРОПИСАТЬ (in-package :regex-library) !!!~%~%")