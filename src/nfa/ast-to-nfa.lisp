;; Алгоритм Томпсона
(in-package :regex-library)

;; Внутренняя структура для представления промежуточных границ подграфа НКА.
;; Используется только во время алгоритма Томпсона на этапе сборки.
(defstruct nfa-fragment
  (start 0 :type fixnum)
  (accept 0 :type fixnum)
)

;; -----------------------------------------------------------------------------
;; Вспомогательные функции сборки элементов Томпсона
;; -----------------------------------------------------------------------------

;; Создает элементарный промежуточный фрагмент из 2 состояний и 1 ребра: s —label→ a
(defun make-basic-fragment (builder label)
  (let ((s (builder-add-state! builder))
        (a (builder-add-state! builder)))
    (builder-add-edge! builder s label a)
    (make-nfa-fragment :start s :accept a)
  )
)

;; Сшивает два фрагмента эпсилон-ребром (accept(f1) → start(f2))
(defun join-fragments! (builder f1 f2)
  (builder-add-edge! builder 
                     (nfa-fragment-accept f1) 
                     :epsilon 
                     (nfa-fragment-start f2)
  )
)

;; -----------------------------------------------------------------------------
;; Компиляция узлов AST (Алгоритм Томпсона)
;; -----------------------------------------------------------------------------

;; Пустой узел (эпсилон)
(defun compile-ast-empty (builder)
  (make-basic-fragment builder :epsilon)
)

;; Одиночный литерал (переход создаётся по его классу эквивалентности)
(defun compile-ast-literal (builder node eq-table)
  (let ((class-id (char-to-class-id (ast-literal-char node) eq-table)))
    (make-basic-fragment builder class-id)
  )
)

;; Символьный класс ([a-z], \d, [^0-9])
(defun compile-ast-char-class (builder node eq-table)
  (let ((s (builder-add-state! builder))
        (a (builder-add-state! builder))
        (class-ids (char-class-to-class-ids node eq-table )))
    ;; Для каждого class-id проверяем дубликаты и прокладываем параллельные рёбра
    ;; Если окажется, что уже существует переход по текущему классу, при этом он ведёт
    ;; в другое состояние — получим ошибку.
    (dolist (class-id class-ids)
      (check-or-add-class-edge! builder s class-id a)
    )
    (make-nfa-fragment :start s :accept a)
  )
)

;; Якоря (^ и $)
(defun compile-ast-anchor (builder node)
  (let ((lbl (if (eq (ast-anchor-type node) :start-of-line)
                 :anchor-start
                 :anchor-end)))
    (make-basic-fragment builder lbl)
  )
)

;; Конкатенация цепочки элементов
(defun compile-ast-concat (builder node eq-table)
  (let ((elems (ast-concat-elements node)))
    (if (null elems)
        (compile-ast-empty builder)
        (let* ((first-f (compile-ast-node builder (first elems) eq-table))
               (prev-f first-f))
          (dolist (child (rest elems))
            (let ((curr-f (compile-ast-node builder child eq-table)))
              (join-fragments! builder prev-f curr-f)
              (setf prev-f curr-f)
            )
          )
          (make-nfa-fragment :start (nfa-fragment-start first-f)
                             :accept (nfa-fragment-accept prev-f)
          )
        )
    )
  )
)

;; Альтернатива (left | right)
(defun compile-ast-alt (builder node eq-table)
  (let ((upper-f (compile-ast-node builder (ast-alt-left node) eq-table))
        (lower-f (compile-ast-node builder (ast-alt-right node) eq-table))
        (s (builder-add-state! builder))
        (a (builder-add-state! builder)))
    ;; Разводка из внешнего старта в входы ветвей
    (builder-add-edge! builder s :epsilon (nfa-fragment-start upper-f))
    (builder-add-edge! builder s :epsilon (nfa-fragment-start lower-f))
    ;; Сведение выходов ветвей во внешний финиш
    (builder-add-edge! builder (nfa-fragment-accept upper-f) :epsilon a)
    (builder-add-edge! builder (nfa-fragment-accept lower-f) :epsilon a)
    (make-nfa-fragment :start s :accept a)
  )
)

;; Замыкание Клини (*)
(defun compile-ast-star (builder node eq-table)
  (let ((f-inner (compile-ast-node builder (ast-star-child node) eq-table))
        (s (builder-add-state! builder))
        (a (builder-add-state! builder)))
    (builder-add-edge! builder s :epsilon (nfa-fragment-start f-inner))
    (builder-add-edge! builder s :epsilon a)
    (builder-add-edge! builder (nfa-fragment-accept f-inner) :epsilon (nfa-fragment-start f-inner))
    (builder-add-edge! builder (nfa-fragment-accept f-inner) :epsilon a)
    (make-nfa-fragment :start s :accept a)
  )
)

;; Один и более (+)
(defun compile-ast-plus (builder node eq-table)
  (let ((f-inner (compile-ast-node builder (ast-plus-child node) eq-table))
        (s (builder-add-state! builder))
        (a (builder-add-state! builder)))
    (builder-add-edge! builder s :epsilon (nfa-fragment-start f-inner))
    (builder-add-edge! builder (nfa-fragment-accept f-inner) :epsilon (nfa-fragment-start f-inner))
    (builder-add-edge! builder (nfa-fragment-accept f-inner) :epsilon a)
    (make-nfa-fragment :start s :accept a)
  )
)

;; Ноль или один (?)
(defun compile-ast-question (builder node eq-table)
  (let ((f-inner (compile-ast-node builder (ast-question-child node) eq-table))
        (s (builder-add-state! builder))
        (a (builder-add-state! builder)))
    (builder-add-edge! builder s :epsilon (nfa-fragment-start f-inner))
    (builder-add-edge! builder s :epsilon a)
    (builder-add-edge! builder (nfa-fragment-accept f-inner) :epsilon a)
    (make-nfa-fragment :start s :accept a)
  )
)

;; Диапазон повторений ({min, max})
(defun compile-ast-range (builder node eq-table)
  (let ((min-val (ast-range-min node))
        (max-val (ast-range-max node))
        (child (ast-range-child node))
        (frags nil))
    ;; 1. Собираем min обязательных копий
    (dotimes (i min-val)
      (push (compile-ast-node builder child eq-table) frags)
    )
    ;; 2. Собираем либо (max - min) опциональных копий, либо 1 star-копию (если max nil)
    (cond
      ((null max-val)
       (push (compile-ast-star builder (make-ast-star :child child) eq-table) frags))
      (t
       (dotimes (i (- max-val min-val))
         (push (compile-ast-question builder (make-ast-question :child child) eq-table) frags)
       )))
    ;; 3. Сшиваем всю цепочку через concat-логику
    (let ((ordered-frags (nreverse frags)))
      (if (null ordered-frags)
          (compile-ast-empty builder)
          (let ((prev-f (first ordered-frags)))
            (dolist (curr-f (rest ordered-frags))
              (join-fragments! builder prev-f curr-f)
              (setf prev-f curr-f)
            )
            (make-nfa-fragment :start (nfa-fragment-start (first ordered-frags))
                               :accept (nfa-fragment-accept prev-f)
            )
          )
      )
    )
  )
)

;; -----------------------------------------------------------------------------
;; Главная диспетчерская функция обхода AST
;; -----------------------------------------------------------------------------
(defun compile-ast-node (builder node eq-table)
  (typecase node
    (ast-empty      (compile-ast-empty builder))
    (ast-literal    (compile-ast-literal builder node eq-table))
    (ast-char-class (compile-ast-char-class builder node eq-table))
    (ast-anchor     (compile-ast-anchor builder node))
    (ast-concat     (compile-ast-concat builder node eq-table))
    (ast-alt        (compile-ast-alt builder node eq-table))
    (ast-star       (compile-ast-star builder node eq-table))
    (ast-plus       (compile-ast-plus builder node eq-table))
    (ast-question   (compile-ast-question builder node eq-table))
    (ast-range      (compile-ast-range builder node eq-table))
    (t (error "Неизвестный тип узла AST: ~A" node))
  )
)