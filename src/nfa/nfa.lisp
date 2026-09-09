;; Структура nfa, nfa-ende и nfa-builder
(in-package :regex-library)

;; Структура для представления направленного ребра НКА.
;; label может принимать значения:
;; - fixnum (Class ID) — переход по конкретному классу эквивалентности;
;; - :any-class-id — переход по любому классу эквивалентности (нужен для петли непривязанного стартового состояния);
;; - :epsilon — безусловный эпсилон-переход;
;; - :anchor-start ('^') — эпсилон-переход при условии начала строки (после \r, \n или на первом (i = 0) символе);
;; - :anchor-end ('$') — эпсилон-переход при условии конца строки (перед \r, \n или при завершении прочтения строки).
;; - :anchor-word-boundary ('\b')
;; - :anchor-non-word-boundary ('\B')
;; - :anchor-start-of-text ('\A')
;; - :anchor-end-of-text ('\z')
;; - :anchor-end-of-text-or-newline ('\Z')
(defstruct nfa-edge
  (label :epsilon)
  (target 0 :type fixnum)
)

;; Структура для представления НКА на базе вектора состояний.
(defstruct nfa
  ;; Массив списков исходящих рёбер (nfa-edge). Индекс элемента — это ID состояния.
  (states #() :type (simple-array list (*)))
  ;; Для поиска подстрок используется внешнее состояние старта (unanchored-start-state — индекс на него),
  ;; которое имеет эпсилон-переход во внутреннее стартовое состояние (anchored-start-state — индекс на него),
  ;; а также петлю в себя по label :any-class-id.
  ;; Это необходимо для поиска границ подстроки по семантике leftmost-longest.
  (unachored-start-state -1 :type fixnum)
  (anchored-start-state -1 :type fixnum)
  (accept-state -1 :type fixnum)
)