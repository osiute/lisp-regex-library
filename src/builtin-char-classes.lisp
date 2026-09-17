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
(defvar *unicode-w-complement-ranges* nil)
(defvar *unicode-d-complement-ranges* nil)
(defvar *unicode-s-complement-ranges* nil)

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

(declaim (inline unicode-word-char-p
                 unicode-digit-char-p
                 unicode-space-char-p
                 ascii-word-char-p
                 ascii-not-word-char-p
                 ascii-not-digit-char-p
                 ascii-not-space-char-p
                 unicode-not-word-char-p
                 unicode-not-digit-char-p
                 unicode-not-space-char-p
                 char-newline-p
                 get-ascii-w-complement-ranges
                 get-ascii-d-complement-ranges
                 get-ascii-s-complement-ranges
                 get-builtin-char-class-ranges-positive
                 get-builtin-char-class-ranges-complement
                 get-builtin-char-class-ranges))

;; --------------------------------------------------------------------------
;; Генератор и кэширование диапазонов
;; --------------------------------------------------------------------------

;; Генератор диапазонов (start . end) для заданного предиката.
;; При необходимости можно ограничить верхнюю границу диапазона через LIMIT.
(defun generate-char-ranges (predicate-fn &optional (limit char-code-limit))
  (let ((ranges nil) (start nil) (prev nil))
    (dotimes (code limit)
      (let* ((ch (code-char code))
             (match (and ch (funcall predicate-fn ch))))
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
    (when start
      (push (cons start prev) ranges))
    (nreverse ranges)
  )
)

(defun get-ascii-w-complement-ranges ()
  (generate-char-ranges #'ascii-not-word-char-p 128))

(defun get-ascii-d-complement-ranges ()
  (generate-char-ranges #'ascii-not-digit-char-p 128))

(defun get-ascii-s-complement-ranges ()
  (generate-char-ranges #'ascii-not-space-char-p 128))

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

(defun get-unicode-w-complement-ranges ()
  (or
    *unicode-w-complement-ranges*
    (setf *unicode-w-complement-ranges*
          (generate-char-ranges #'unicode-not-word-char-p))
  )
)

(defun get-unicode-d-complement-ranges ()
  (or
    *unicode-d-complement-ranges*
    (setf *unicode-d-complement-ranges*
          (generate-char-ranges #'unicode-not-digit-char-p))
  )
)

(defun get-unicode-s-complement-ranges ()
  (or
    *unicode-s-complement-ranges*
    (setf *unicode-s-complement-ranges*
          (generate-char-ranges #'unicode-not-space-char-p))
  )
)

;; --------------------------------------------------------------------------
;; Главная функция получения диапазонов для парсера
;; --------------------------------------------------------------------------

(defun get-builtin-char-class-ranges-positive (ch char-mode)
  (case ch
    (#\w
      (case char-mode
        (:unicode (get-unicode-w-ranges))
        (:ascii +ascii-w-ranges+)
        (t
          (error "get-builtin-char-class-ranges-positive: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (#\d
      (case char-mode
        (:unicode (get-unicode-d-ranges))
        (:ascii +ascii-d-ranges+)
        (t
          (error "get-builtin-char-class-ranges-positive: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (#\s
      (case char-mode
        (:unicode (get-unicode-s-ranges))
        (:ascii +ascii-s-ranges+)
        (t
          (error "get-builtin-char-class-ranges-positive: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (t nil)
  )
)

(defun get-builtin-char-class-ranges-complement (ch char-mode)
  (case ch
    (#\W
      (case char-mode
        (:unicode (get-unicode-w-complement-ranges))
        (:ascii (get-ascii-w-complement-ranges))
        (t
          (error "get-builtin-char-class-ranges-complement: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (#\D
      (case char-mode
        (:unicode (get-unicode-d-complement-ranges))
        (:ascii (get-ascii-d-complement-ranges))
        (t
          (error "get-builtin-char-class-ranges-complement: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (#\S
      (case char-mode
        (:unicode (get-unicode-s-complement-ranges))
        (:ascii (get-ascii-s-complement-ranges))
        (t
          (error "get-builtin-char-class-ranges-complement: неизвестный char-mode ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ!!!" char-mode)
        )
      )
    )
    (t nil)
  )
)

(defun get-builtin-char-class-ranges (ch char-mode)
  (or
    (get-builtin-char-class-ranges-positive ch char-mode)
    (get-builtin-char-class-ranges-complement ch char-mode)
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

(defun ascii-not-word-char-p (ch)
  (not (ascii-word-char-p ch)))

(defun ascii-not-digit-char-p (ch)
  (not (char<= #\0 ch #\9)))

(defun ascii-not-space-char-p (ch)
  (not (member ch '(#\Space #\Tab #\Page #\Newline #\Return))))

(defun unicode-not-word-char-p (ch)
  (not (unicode-word-char-p ch)))

(defun unicode-not-digit-char-p (ch)
  (not (unicode-digit-char-p ch)))

(defun unicode-not-space-char-p (ch)
  (not (unicode-space-char-p ch)))

(defun char-newline-p (ch)
  (or (char= ch #\Newline)
      (char= ch #\Return)
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
