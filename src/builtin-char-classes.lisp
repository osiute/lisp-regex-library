(in-package :regex-library)

;; --------------------------------------------------------------------------
;; Константы ASCII диапазонов для спецклассов
;; --------------------------------------------------------------------------

(defparameter +ascii-w-ranges+
  (list
    (cons #\a #\z) (cons #\A #\Z)
    (cons #\0 #\9) (cons #\_ #\_)
  )
)

(defparameter +ascii-d-ranges+
  (list (cons #\0 #\9))
)

(defparameter +ascii-s-ranges+
  (list
    (cons #\Space #\Space) (cons #\Tab #\Tab) (cons #\Page #\Page)
    (cons (code-char 10) (code-char 10)) (cons (code-char 13) (code-char 13)) ; \n, \r
  )
)

;; --------------------------------------------------------------------------
;; Кэш-хранилища диапазонов Юникода (чтобы не вычислять каждый раз)
;; --------------------------------------------------------------------------

(defvar *unicode-w-ranges* nil)
(defvar *unicode-d-ranges* nil)
(defvar *unicode-s-ranges* nil)

;; --------------------------------------------------------------------------
;; Вспомогательные предикаты Юникода
;; --------------------------------------------------------------------------

;; Предикат проверки словесного символа в Юникоде
(defun unicode-word-char-p (ch)
  (or
    (alphanumericp ch)
    (char= ch #\_)
  )
)

;; Предикат проверки цифры в Юникоде
(defun unicode-digit-char-p (ch)
  (not (null (digit-char-p ch)))
)

;; Предикат проверки пробельного символа в Юникоде
(defun unicode-space-char-p (ch)
  (or
    (member ch '(#\Space #\Tab #\Page #\Newline #\Return))
    (char= ch (code-char 160))
  )
)

;; --------------------------------------------------------------------------
;; Генератор и кэширование диапазонов
;; --------------------------------------------------------------------------

;; Генератор диапазонов (start . end) для заданного предиката
(defun generate-char-ranges (predicate-fn)
  (let ((ranges nil) (start nil) (prev nil))
    (dotimes (code char-code-limit)
      (let* ((ch (code-char code))
             (match (and ch (funcall predicate-fn ch))))
        ;; Обработка начала, продолжения или завершения текущего диапазона
        (cond
          ((and match (null start))
            (setf start ch prev ch))
          ((and match start)
            (setf prev ch))
          ((and (not match) start)
            (push (cons start prev) ranges)
            (setf start nil))
        )
      )
    )
    ;; Последний диапазон, если символ был в конце таблицы
    (when start
      (push (cons start prev) ranges))
    (nreverse ranges)
  )
)

;; Возвращает кэшированный список Unicode-диапазонов для \w
(defun get-unicode-w-ranges ()
  (or
    *unicode-w-ranges*
    (setf *unicode-w-ranges*
          (generate-char-ranges #'unicode-word-char-p))
  )
)

;; Возвращает кэшированный список Unicode-диапазонов для \d
(defun get-unicode-d-ranges ()
  (or
    *unicode-d-ranges*
    (setf *unicode-d-ranges*
          (generate-char-ranges #'unicode-digit-char-p))
  )
)

;; Возвращает кэшированный список Unicode-диапазонов для \s
(defun get-unicode-s-ranges ()
  (or
    *unicode-s-ranges*
    (setf *unicode-s-ranges*
          (generate-char-ranges #'unicode-space-char-p))
  )
)

;; --------------------------------------------------------------------------
;; Главная функция получения диапазонов для парсера
;; --------------------------------------------------------------------------

;; Возвращает диапазоны пар (start . end) для спецкласса с учетом char-mode
(defun get-builtin-char-class-ranges (ch char-mode)
  (case ch
    ((#\w #\W)
      (case char-mode
        (:unicode (get-unicode-w-ranges))
        (:ascii +ascii-w-ranges+)
        (t
          (error "get-builtin-char-class-ranges: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    ((#\d #\D)
      (case char-mode
        (:unicode (get-unicode-d-ranges))
        (:ascii +ascii-d-ranges+)
        (t
          (error "get-builtin-char-class-ranges: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    ((#\s #\S)
      (case char-mode
        (:unicode (get-unicode-s-ranges))
        (:ascii +ascii-s-ranges+)
        (t
          (error "get-builtin-char-class-ranges: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (t
      nil
    )
  )
)

;; --------------------------------------------------------------------------
;; Предикаты проверки единичных символов
;; --------------------------------------------------------------------------

;; Проверка словесного символа в ASCII
(defun ascii-word-char-p (ch)
  (or
    (char<= #\a ch #\z)
    (char<= #\A ch #\Z)
    (char<= #\0 ch #\9)
    (char= ch #\_)
  )
)

;; Проверка словесного символа (\w) с учетом режима char-mode
(defun word-char-p (ch char-mode)
  (case char-mode
    (:unicode (unicode-word-char-p ch))
    (:ascii (ascii-word-char-p ch))
    (t
      (error "word-char-p: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
    )
  )
)

;; Проверка цифрового символа (\d) с учетом режима char-mode
(defun builtin-digit-char-p (ch char-mode)
  (case char-mode
    (:unicode (unicode-digit-char-p ch))
    (:ascii (char<= #\0 ch #\9))
    (t
      (error "builtin-digit-char-p: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
    )
  )
)

;; Проверка пробельного символа (\s) с учетом режима char-mode
(defun builtin-space-char-p (ch char-mode)
  (case char-mode
    (:unicode (unicode-space-char-p ch))
    (:ascii (not (null (member ch '(#\Space #\Tab #\Page #\Newline #\Return)))))
    (t
      (error "builtin-space-char-p: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
    )
  )
)
