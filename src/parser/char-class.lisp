;; Реализует парсинг символьных классов ([a-z], [^0-9], \d, \w, \s)

(in-package :regex-library)

;; Возвращает диапазоны для спецклассов \d, \w, \s в формате ((start . end)...)
(defun get-builtin-char-class-ranges (char-code)
  (case char-code
    (#\d (list (cons #\0 #\9)))
    (#\w (list (cons #\a #\z)
               (cons #\A #\Z)
               (cons #\0 #\9)
               (cons #\_ #\_)))
    (#\s (list (cons #\Space #\Space)
               (cons #\Tab #\Tab)
               (cons #\Page #\Page)
               (cons (code-char 10) (code-char 10))
               (cons (code-char 13) (code-char 13))))
    (t nil)
  )
)

;; Парсит экранированный спецкласс (\d, \w, \s) или обычный экранированный символ (\., \\)
(defun parse-escape-char-class (state)
  (parser-next state) ; пропускаем '\'
  (let ((escaped (parser-next state)))
    (unless escaped
      (error "Синтаксическая ошибка: незавершённая escape-последовательность в позиции ~A"
             (parser-state-index state))
    )
    (let ((builtin-ranges (get-builtin-char-class-ranges escaped)))
      (if builtin-ranges
          (make-ast-char-class :ranges builtin-ranges :negated-p nil)
          (make-ast-char-class :ranges (list (cons escaped escaped)) :negated-p nil)
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
      (error "Синтаксическая ошибка: неверный порядок диапазона (~A-~A) в позиции ~A"
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