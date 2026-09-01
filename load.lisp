;; Автоматическое подключение библиотеки и тестов (чтобы каждый раз не прописывать подключение библиотек в sbcl вручную)
(require :asdf)

(push #p"./" asdf:*central-registry*) ; Включение текущей директории в диапазон поиска проектов для asdf

(format t "~%[1/2] Загрузка основной системы...~%")
(asdf:load-system :regex-library)

(format t "~%[2/2] Загрузка тестовой системы...~%")
(asdf:load-system :regex-library/tests)

(in-package :regex-library)

(defparameter +dot-file-name+ "nfa.dot")

(defun print-help()
  (format t "--------------------------------------------------~%")
  (format t "Доступные тесты:~%")
  (format t "~4T(run-all-tests)~%~%")
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
  (format t "~4T(run-endpoints-converter-tests)~%")
  (format t "~4T(run-unicode-tests)~%")
  (format t "~4T(run-char-class-to-class-ids-tests)~%")
  (format t "~2TNFA:~%")
  (format t "~4T(run-nfa-thompson-tests)~%")
  (format t "~4T(run-nfa-closure-tests)~%")
  (format t "~2TDFA:~%")
  (format t "~2TEngine (main):~%")
  (format t "--------------------------------------------------~%")
  (format t "Доступные команды:~%")
  (format t "~2T(parse-regex PATTERN)~%")
  (format t "~2T(print-ast-node NODE)~%")
  (format t "~2T(papre PATTERN) — parse and print regex ~%")
  (format t "~2T(create-nfa PATTERN)~%")
  (format t "~2T(generate-dot-on-nfa NFA)~%")
  (format t "~2T(nfa-dot PATTERN FILE-NAME) — записать внутреннее представление НКА, сгенерированного по паттерну, в файл nfa.dot~%")

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

(defun create-nfa (pattern)
  (let* ((ast-root (parse-regex pattern))
         (eq-table (make-equivalence-table-from-ast ast-root))
         (nfa (build-nfa-from-ast ast-root eq-table)))
    nfa)
)

(defun generate-dot-on-nfa (nfa file-name)
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (nfa-to-dot nfa out)
  )
)

(defun nfa-dot (pattern &optional file-name)
  (when (null file-name)
    (setf file-name +dot-file-name+)
  )
  (let ((nfa-to-output (create-nfa pattern)))
    (generate-dot-on-nfa nfa-to-output file-name)
  )
)

(format t "~%=== Проект успешно загружен! ===~%")
(print-help)
(format t "!!! НЕОБХОДИМО ПРОПИСАТЬ (in-package :regex-library) !!!~%~%")