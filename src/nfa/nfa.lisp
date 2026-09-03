;; Структура nfa, nfa-ende и nfa-builder
(in-package :regex-library)

;; Структура для представления направленного ребра НКА.
;; label может принимать значения:
;; - fixnum (Class ID): переход по конкретному классу эквивалентности;
;; - :epsilon: безусловный эпсилон-переход;
;; - :anchor-start ('^'): эпсилон-переход при условии начала строки (после \r, \n или на первом (i = 0) символе);
;; - :anchor-end ('$'): эпсилон-переход при условии конца строки (перед \r, \n или при завершении прочтения строки).
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
  (start-state 0 :type fixnum)
  (accept-state 0 :type fixnum)
)