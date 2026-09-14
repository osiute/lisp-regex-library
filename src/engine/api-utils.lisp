(in-package :regex-library)

(declaim (inline ensure-regex-object))
(defun ensure-regex-object (regex mode func-name)
  (cond
    ;; Передан уже скомпилированный REGEX и при этом явный режим
    ((and (regex-p regex) mode)
      (error "engine/~A: Передан скомпилированный объект REGEX и одновременно указан :builtin-char-class-mode (~A). Mode задаётся при компиляции."
            func-name mode)
    )
    ;; Валидный объект REGEX
    ((regex-p regex)
      regex
    )
    
    ((stringp regex)
      ;; Если режим не указан, то устанавливается :unicode.
      (compile-regex regex (or mode :unicode))
    )
    
    (t
      (error "engine/~A: Недопустимый тип для REGEX: ~A. Ожидалась строка или объект REGEX."
             func-name (type-of regex))
    )
  )
)

(declaim (inline assert-bounds))
(defun assert-bounds (text-length start end func-name)
  (assert (and (>= start 0) (>= end start) (<= end text-length)) ()
    "~A: неверно установлены границы: start = ~A, end = ~A, text-length = ~A"
                                                  func-name start end text-length)
)