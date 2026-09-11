(in-package :regex-library)

(defstruct (regex
             (:constructor %make-regex)
             (:copier nil))
  "Скомпилированное регулярное выражение."
  (pattern "" :type string :read-only t)
  (builtin-char-class-mode :ascii :type symbol :read-only t)
  (direct-dfa nil :read-only t)
  (reversed-dfa nil :read-only t)
  (eq-classes-table nil :read-only t)
)

(defun compile-regex (pattern builtin-char-class-mode &key (max-dfa-states 1000))
  "Компилирует строковый PATTERN в объект структуры REGEX с учетом режима спецклассов.
  BUILTIN-CHAR-CLASS-MODE принимает значения :unicode или :ascii. Опередляет поведение встроенных классов \\d, \\w, \\s и т.д;
  MAX-DFA-STATES — finumx, определящий максимальное количество состояний ДКА. Для сложных выражений маленькое значение может замедлять работу.
  "
  (declare (type string pattern)
           (type (member :ascii :unicode) builtin-char-class-mode)
           (type fixnum max-dfa-states))
  (let* ((ast (parse-regex pattern :builtin-char-class-mode builtin-char-class-mode))
         (eq-classes-table (make-equivalence-table-from-ast ast)))
    ;; Создание автоматонов и итогового объекта REGEX
    (multiple-value-bind (dir-nfa rev-nfa) 
        (build-nfa-pair ast eq-classes-table)
      (multiple-value-bind (dir-dfa rev-dfa) 
          (build-dfa-pair dir-nfa rev-nfa max-dfa-states)
        (%make-regex :pattern pattern
                    :builtin-char-class-mode builtin-char-class-mode
                    :direct-dfa dir-dfa
                    :reversed-dfa rev-dfa
                    :eq-classes-table eq-classes-table)
      )
    )
  )
)

;; Строит прямой и обратный НКА по абстрактному синтаксическому дереву и таблице эквивалентности.
(defun build-nfa-pair (ast eq-classes-table)
  (let* ((dir-nfa (build-nfa-from-ast ast eq-classes-table))
         (rev-nfa (reverse-nfa dir-nfa)))
    (values dir-nfa rev-nfa)
  )
)

;; Создаёт объекты ленивых ДКА с ограничением на количество состояний на основе прямого и обратного НКА.
(defun build-dfa-pair (dir-nfa rev-nfa max-states)
  (let ((dir-dfa (make-lazy-dfa dir-nfa :max-states max-states))
        (rev-dfa (make-lazy-dfa rev-nfa :max-states max-states)))
    (values dir-dfa rev-dfa)
  )
)