;; Реализует парсинг символьных классов ([a-z], [^0-9], \d, \w, \s) и экранированных символов (\\, \|, \uXXXX и т.д.)
(in-package :regex-library)

(defun get-escaped-anchor-type (ch)
  (case ch
    (#\b :word-boundary)
    (#\B :non-word-boundary)
    (#\A :absolute-start-of-text)
    (#\z :absolute-end-of-text)
    (#\Z :absolute-end-of-text-or-newline)
    (t nil)
  )
)

(defun get-standard-escape-or-unicode-character (ch state)
  (case ch
    (#\n (code-char 10))
    (#\r (code-char 13))
    (#\t (code-char 9))
    ((#\\ #\. #\* #\+ #\? #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\|) ch)
    (#\u (parse-unicode-character state))
    (t nil)
  )
)

(declaim (inline builtin-capital-p))
(defun builtin-capital-p (ch)
  (or (eql ch #\D) (eql ch #\W) (eql ch #\S))
)

;; Парсит экранированный спецкласс (\d, \w, \s), обычный экранированный символ (\., \\, \| и т.д.),
;; управляющий символ (\n, \r, \t), юникод-символ (uXXXX, u{X+}) или якоря \b, \B, \A, \z, \Z.
;; builtin-char-class-mode — либо :unicode, либо :ascii.
(defun parse-escape-char-class (state builtin-char-class-mode)
  (parser-next state) ; пропускаем '\'
  (let ((escaped (parser-next state)))
    (unless escaped
      (error "Синтаксическая ошибка: незавершённая escape-последовательность в позиции ~A"
             (parser-state-index state))
    )
    (let ((builtin-ranges (get-builtin-char-class-ranges escaped builtin-char-class-mode))
          (escaped-anchor-type (get-escaped-anchor-type escaped)))
      (cond
        ((and builtin-ranges (builtin-capital-p escaped)) ; \D, \W, \S
          (make-ast-char-class :ranges builtin-ranges :negated-p t))
        (builtin-ranges ; \d, \w, \s
          (make-ast-char-class :ranges builtin-ranges :negated-p nil))
        ((builtin-capital-p escaped) (error "parser/char-class/parse-escape-char-class:
            Нет диапазонов для встроенной заглавной буквы. Я сделал плохую программу!"))
        (escaped-anchor-type (make-ast-anchor :type escaped-anchor-type))
        (t (progn
          (let ((standard (get-standard-escape-or-unicode-character escaped state)))
            (assert standard () "Синтаксическая ошибка: неизвестная escape-последовательность в позиции ~A"
                                 (1- (parser-state-index state)))
            (make-ast-literal :char standard)
          )
        ))
      )
    )
  )
)

;; Считывает следующий символ внутри [...] с учётом экранирования (\- -> -)
(defun parse-bracket-char (state)
  (let ((ch (parser-next state)))
    (if (and (eql ch #\\) (parser-peek state))
        (parser-next state) ; пропускаем '\' и берем экранированный символ
        ch
    )
  )
)

;; Завершает разбор диапазона 'start-char - end-char' и валидирует границы
(defun parse-bracket-range-end (state start-char)
  (parser-next state) ; пропускаем '-'
  (let ((end-char (parse-bracket-char state)))
    (unless end-char
      (error "Синтаксическая ошибка: незакрытый символьный класс в позиции ~A"
             (parser-state-index state))
    )
    (when (> (char-code start-char) (char-code end-char))
      (error "Синтаксическая ошибка: неверный порядок диапазона ('~A'-'~A') в позиции ~A"
             start-char end-char (parser-state-index state))
    )
    (cons start-char end-char)
  )
)

;; Предикат: является ли дефис литералом (в начале класса или перед ']')
(defun hyphen-literal-p (cur ranges state)
  (and (eql cur #\-)
       (or (null ranges)
           (eql (parser-peek state) #\])))
)

;; Считывает один элемент внутри [...] — литеральный дефис, диапазон или одиночный символ
(defun parse-bracket-element (state ranges)
  (let ((cur (parser-peek state)))
    (if (hyphen-literal-p cur ranges state)
        (progn
          (parser-next state)
          (cons #\- #\-))
        (let ((start-char (parse-bracket-char state)))
          ;; Проверяем, идет ли следом '-' и не закрывается ли сразу класс ']'
          (if (and (eql (parser-peek state) #\-)
                   (not (eql (char-at-offset state 1) #\])))
              (parse-bracket-range-end state start-char)
              (cons start-char start-char)
          )
        )
    )
  )
)

;; Безопасное получение символа из строки со смещением от текущего индекса
(defun char-at-offset (state offset)
  (let ((idx (+ (parser-state-index state) offset)))
    (if (< idx (parser-state-len state))
        (char (parser-state-str state) idx)
        nil
    )
  )
)

;; Главная функция: парсит скобочную группу [a-z0-9] или [^abc]
(defun parse-bracket-char-class (state)
  (parser-next state) ; пропускаем '['
  (let ((negated (parser-match-p state #\^))
        (ranges nil))
    (loop
      (let ((cur (parser-peek state)))
        (unless cur
          (error "Синтаксическая ошибка: незакрытый символьный класс '[' в позиции ~A"
                 (parser-state-index state))
        )
        (when (eql cur #\])
          (parser-next state) ; пропускаем ']'
          (return)
        )
        (push (parse-bracket-element state ranges) ranges)
      )
    )
    (make-ast-char-class :ranges (nreverse ranges) :negated-p negated)
  )
)