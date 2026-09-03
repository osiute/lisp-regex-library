(in-package :regex-library)

;; Вспомогательная константа и макрос для точки (всех символов Unicode кроме \n, \r).
;; Для изменения семантики точки необходимо изменить +dot-all-chars+.
(defparameter +dot-all-chars+
  (list (cons (code-char 0) (code-char 9))
        (cons (code-char 11) (code-char 12))
        (cons (code-char 14) (code-char #x10FFFF)))
)
(defmacro make-ast-dot ()
  `(make-ast-char-class :ranges +dot-all-chars+ :negated-p nil) 
)

;; -----------------------------------------------------------------------------
;; Базовый узел AST
;; -----------------------------------------------------------------------------
(defstruct ast-node)

;; -----------------------------------------------------------------------------
;; Символьные узлы
;; -----------------------------------------------------------------------------
;; Узел для одиночного символа
(defstruct (ast-literal (:include ast-node))
  (char #\Nul :type character)
)

;; Узел символьного класса (например, [a-z0-9], \d)
(defstruct (ast-char-class (:include ast-node))
  (ranges nil :type list) ; список диапазонов вида ((start.end)...)
  (negated-p nil :type boolean) ; инвертировать диапазон 
)

;; -----------------------------------------------------------------------------
;; Алгебраические операторы
;; -----------------------------------------------------------------------------
;; Узел конкатенации списков выражений
(defstruct (ast-concat (:include ast-node))
  (elements nil :type list) ;
)

;; Узел альтернативы (|)
(defstruct (ast-alt (:include ast-node))
  (left nil :type ast-node)
  (right nil :type ast-node)
)

;; -----------------------------------------------------------------------------
;; Квантификаторы
;; -----------------------------------------------------------------------------
;; Замыкание Клини (*)
(defstruct (ast-star (:include ast-node))
  (child nil :type ast-node)
)

;; Один и более (+)
(defstruct (ast-plus (:include ast-node))
  (child nil :type ast-node)
)

;; Ноль или один (?)
(defstruct (ast-question (:include ast-node))
  (child nil :type ast-node)
)

;; Диапазон ({m,n})
(defstruct (ast-range (:include ast-node))
  (min 0 :type fixnum)
  (max nil) ; nil - отсутствие верхней границы
  (child nil :type ast-node)
)

;; -----------------------------------------------------------------------------
;; Якоря (^, $)
;; -----------------------------------------------------------------------------
(defstruct (ast-anchor (:include ast-node))
  ;; :start-of-line — ^, :end-of-line — $
  ;; :word-boundary — \b, :non-word-boundary — \B
  ;; :absolute-start-of-text — \A, :absolute-end-of-text — \z, absolute-end-of-text-or-newline — \Z
  (type :start-of-line :type keyword) ; 
)

;; -----------------------------------------------------------------------------
;; Узел пустого выражения (эпсилон)
;; -----------------------------------------------------------------------------
(defstruct (ast-empty (:include ast-node)))