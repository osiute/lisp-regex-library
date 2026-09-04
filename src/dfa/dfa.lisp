;; Базовые типы, конструктор и битовые манипуляции с ключами.
(in-package :regex-library)

;;; ----------------------------------------------------------------------------
;;; Структуры данных
;;; ----------------------------------------------------------------------------

(defstruct dfa-state
  ;; Канонический отсортированный вектор состояний НКА
  (nfa-set #() :type (simple-array fixnum (*)))
  ;; Флаг принимающего состояния (t, если nfa-set содержит nfa-accept-state)
  (accept-p nil :type boolean)
  ;; Таблица переходов: packed-key (fixnum) → target-state-id (fixnum)
  (transitions (make-hash-table :test 'eql) :type hash-table)
)

(defstruct dfa
  (nfa nil :type (or null nfa))
  ;; Реестр уникальности: nfa-set (vector) → dfa-state
  (state-map (make-hash-table :test 'equalp) :type hash-table)
  ;; Вектор сгенерированных состояний dfa-state (индекс массива = id состояния)
  (states (make-array 16 :adjustable t :fill-pointer 0) :type vector)
  ;; Вектор ID стартовых состояний для 64 масок контекста (-1 = не вычислено)
  (start-states (make-array 64 :element-type 'fixnum :initial-element -1)
                :type (simple-array fixnum (64)))
  ;; Максимально допустимое число состояний до полного сброса кэша
  (max-states 1000 :type fixnum)
  ;; Выделенные заранее буферы для compute-nfa-closure (избегаем аллокаций)
  (closure-queue #() :type vector)
  (closure-visited #() :type (simple-array bit (*)))
)

;;; ----------------------------------------------------------------------------
;;; Битовые операции и конструкторы
;;; ----------------------------------------------------------------------------

;; Упаковывает пары (class-id, context) в один fixnum.
;; Под маску контекста (0..63) отводятся 6 младших бит.
(declaim (inline pack-transition-key))
(defun pack-transition-key (class-id context)
  (declare (type fixnum class-id context))
  (logior (ash class-id 6) context)
)

;; Выделяет плоский буфер очереди и битовый вектор посещений под размер НКА
(defun allocate-closure-buffers (nfa-size)
  (values (make-array nfa-size :adjustable t :fill-pointer 0)
          (make-array nfa-size :element-type 'bit :initial-element 0)
  )
)

;; Создаёт и инициализирует объект ленивого ДКА
(defun make-lazy-dfa (nfa &key (max-states 1000))
  (let ((nfa-size (length (nfa-states nfa))))
    (multiple-value-bind (queue visited) (allocate-closure-buffers nfa-size)
      (make-dfa :nfa nfa
                :max-states max-states
                :closure-queue queue
                :closure-visited visited)
    )
  )
)