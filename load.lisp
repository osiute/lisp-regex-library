;; Автоматическое подключение библиотеки и тестов (чтобы каждый раз не прописывать подключение библиотек в sbcl вручную)
(require :asdf)

(push #p"./" asdf:*central-registry*) ; Включение текущей директории в диапазон поиска проектов для asdf

(format t "~%[1/2] Загрузка основной системы...~%")
(asdf:load-system :regex-library)

(format t "~%[2/2] Загрузка тестовой системы...~%")
(asdf:load-system :regex-library/tests)

(format t "~%=== Проект успешно загружен! ===~%")
(format t "Доступные тесты:~%")
(format t "(run-parser-tests)~%")
(format t "(run-unicode-tests)~%")
(format t "(run-nfa-tests)~%")
(format t "(run-dfa-tests)~%")
(format t "(run-engine-tests)~%~%")
(format t "!!! НЕОБХОДИМО ПРОПИСАТЬ (in-package :regex-library) !!!~%~%")