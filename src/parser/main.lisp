;; Публичный интефрейс модуля парсера
(in-package :regex-library)

(defun parse-regex (pattern)
  "Принимает строку PATTERN и возвращает построенное по нему AST-дерево как ast-node"
  (let ((state (make-parser-state :str pattern :len (length pattern))))
    (let ((ast (parse-expression state)))
      (when (parser-peek state)
        (error "Синтаксическая ошибка: неожиданный символ ~A в позиции ~A"
                                        (parser-peek state)
                                        (parser-state-index state)
        )
      )
      ast
    )
  )
)