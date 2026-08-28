;; Автоматическое подключение библиотеки и тестов (чтобы каждый раз не прописывать подключение библиотек в sbcl вручную)
(require :asdf)

(push #p"./" asdf:*central-registry*) ; Включение текущей директории в диапазон поиска проектов для asdf

(format t "~%[1/2] Загрузка основной системы...~%")
(asdf:load-system :regex-library)

(format t "~%[2/2] Загрузка тестовой системы...~%")
(asdf:load-system :regex-library/tests)

(in-package :regex-library)

(defun print-help()
  (format t "--------------------------------------------------~%")
  (format t "Доступные тесты:~%")
  (format t "~4T(run-tests)~%~%")
  (format t "~2TParser:~%")
  (format t "~4T(run-state-tests)~%")
  (format t "~4T(run-char-class-tests)~%")
  (format t "~4T(run-range-quantifier-tests)~%")
  (format t "~4T(run-grammar-atoms-tests)~%")
  (format t "~4T(run-grammar-quantifier-tests)~%")
  (format t "~4T(run-grammar-concatenation-tests)~%")
  (format t "~4T(run-grammar-expression-tests)~%")
  (format t "~2TUnicode:~%")
  (format t "~4T(run-endpoints-collector-tests)~%")
  (format t "~2TNFA:~%")
  (format t "~2TDFA:~%")
  (format t "~2TEngine (main):~%")
  (format t "--------------------------------------------------~%")
  (format t "Доступные команды:~%")
  (format t "~2T(parse-regex PATTERN)~%")
  (format t "~2T(print-ast-node NODE)~%")
  (format t "~2T(papre PATTERN) — parse and print regex ~%")
  (format t "~2T(reload) — перезагрузить load.lisp в REPL ~%")
  (format t "~2T(print-help)~%")
  (format t "--------------------------------------------------~%")
)

;;------------------------------------------------------------------------------
;; сокращения
;;------------------------------------------------------------------------------
;; papre — parse and print regex
(defun papre (pattern)
  (print-ast-node (parse-regex pattern))
)
;; 
(defun reload ()
  (load "load.lisp")
)

(format t "~%=== Проект успешно загружен! ===~%")
(print-help)
(format t "!!! НЕОБХОДИМО ПРОПИСАТЬ (in-package :regex-library) !!!~%~%")