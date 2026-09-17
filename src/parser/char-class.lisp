;; Реализует парсинг символьных классов ([a-z], [^0-9], \d, \w, \s) и экранированных символов (\\, \|, \uXXXX и т.д.)
(in-package :regex-library)

(declaim (inline get-escaped-anchor-type))
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

(declaim (inline get-escape-literal))
(defun get-escape-literal (ch state)
  (case ch
    (#\n (code-char 10))
    (#\r (code-char 13))
    (#\t (code-char 9))
    ((#\\ #\. #\* #\+ #\? #\^ #\$ #\( #\) #\[ #\] #\{ #\} #\| #\-) ch)
    (#\u (parse-unicode-character state))
    (t nil)
  )
)

;; Парсит экранированный спецкласс (\d, \w, \s), обычный экранированный символ (\., \\, \| и т.д.),
;; управляющий символ (\n, \r, \t), юникод-символ (uXXXX, u{X+}) или якоря \b, \B, \A, \z, \Z.
;; builtin-char-class-mode — либо :unicode, либо :ascii.
(defun parse-escape-char-class (state builtin-char-class-mode)
  (parser-next state) ; пропускаем '\'
  (let ((escaped (parser-next state))
        (val nil))
    (unless escaped
      (error "Синтаксическая ошибка: незавершённая escape-последовательность в позиции ~A"
         (parser-state-index state))
    )
    (cond
      ((setf val (get-escaped-anchor-type escaped))
       (make-ast-anchor :type val))
      ((setf val (get-escape-literal escaped state))
       (make-ast-literal :char val))
      ((setf val (get-builtin-char-class-ranges escaped builtin-char-class-mode))
       (make-ast-char-class :ranges val :negated-p nil))
      (t
       (error "Синтаксическая ошибка: неизвестная escape-последовательность в позиции ~A"
              (1- (parser-state-index state))))
    )
  )
)

;; Разбирает escape-последовательность внутри [...] и возвращает сырой токен:
;; либо character, либо список консов. Для якорей выкидывает ошибку.
(defun parse-escape-char-in-brackets (state builtin-char-class-mode)
  (let ((escaped (parser-next state))
        (val nil))
    (unless escaped
      (error "Синтаксическая ошибка: незавершённая escape-последовательность в позиции ~A"
             (parser-state-index state)
      )
    )
    (cond
      ((setf val (get-escape-literal escaped state))
       val)
      ((setf val (or (get-builtin-char-class-ranges-positive escaped builtin-char-class-mode)
                     (get-builtin-char-class-ranges-complement escaped builtin-char-class-mode)))
       val)
      ((get-escaped-anchor-type escaped)
       (error "Синтаксическая ошибка: якорь '\\~A' недопустим внутри '[...]' в позиции ~A"
              escaped (parser-state-index state)
       ))
      (t
       (error "Синтаксическая ошибка: неизвестная escape-последовательность в позиции ~A"
              (1- (parser-state-index state))
       ))
    )
  )
)

;; Считывает один сырой токен внутри [...]: либо char, либо список cons-пар элементов
(defun parse-bracket-token (state builtin-char-class-mode)
  (let ((ch (parser-next state)))
    (if (and (eql ch #\\) (parser-peek state))
        (parse-escape-char-in-brackets state builtin-char-class-mode)
        ch
    )
  )
)

;; Завершает разбор диапазона 'start-char - end-char' и валидирует границы
(defun parse-bracket-range-end (state start-char builtin-char-class-mode)
  (parser-next state) ; пропускаем '-'
  (let ((end-token (parse-bracket-token state builtin-char-class-mode)))
    (unless (characterp end-token)
      (error "Синтаксическая ошибка: встроенный класс не может быть границей диапазона в позиции ~A"
             (parser-state-index state)
      )
    )
    (when (> (char-code start-char) (char-code end-token))
      (error "Синтаксическая ошибка: неверный порядок диапазона ('~A'-'~A') в позиции ~A"
             start-char end-token (parser-state-index state)
      )
    )
    (cons start-char end-token)
  )
)

;; Предикат: является ли дефис литералом (в начале класса или перед ']')
(defun hyphen-literal-p (cur ranges state)
  (and (eql cur #\-)
       (or (null ranges)
           (eql (parser-peek state) #\])))
)

;; Считывает один элемент внутри [...]: возвращает либо пара-конс (start . end),
;; либо список пар-консов (для встроенного спецкласса)
(defun parse-bracket-element (state ranges builtin-char-class-mode)
  (let ((cur (parser-peek state)))
    (if (hyphen-literal-p cur ranges state)
        (progn
          (parser-next state)
          (cons #\- #\-))
        (let ((token (parse-bracket-token state builtin-char-class-mode)))
          (cond
            ((characterp token)
             (if (and (eql (parser-peek state) #\-)
                      (not (eql (char-at-offset state 1) #\])))
                 (parse-bracket-range-end state token builtin-char-class-mode)
                 (cons token token)))
            ((listp token)
             token)
            (t
             (error "Синтаксическая ошибка: некорректный элемент в позиции ~A"
                    (parser-state-index state)
             ))
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
(defun parse-bracket-char-class (state builtin-char-class-mode)
  (parser-next state) ; пропускаем '['
  (let ((negated (parser-match-p state #\^))
        (ranges nil))
    (loop
      (let ((cur (parser-peek state)))
        (unless cur
          (error "Синтаксическая ошибка: незакрытый символьный класс '[' в позиции ~A"
                 (parser-state-index state)
          )
        )
        (when (eql cur #\])
          (parser-next state) ; пропускаем ']'
          (return))
        (let ((element (parse-bracket-element state ranges builtin-char-class-mode)))
          ;; Различаем список диапазонов от единичного cons-пара (start . end)
          (if (and (listp element) (consp (car element)))
              (setf ranges (append element ranges))
              (push element ranges)
          )
        )
      )
    )
    (make-ast-char-class :ranges ranges :negated-p negated)
  )
)