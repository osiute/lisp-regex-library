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
  ;; Таблица переходов: packed-key (fixnum) → target-state-id (fixnum).
  ;; target-state-id = -1 означает отсутствие перехода по данному ключу (мёртвое состояние).
  (transitions (make-hash-table :test 'eql) :type hash-table)
)

(defstruct dfa
  (nfa nil :type (or null nfa))
  ;; Реестр уникальности: nfa-set (vector) → dfa-state
  (state-map (make-hash-table :test 'equalp) :type hash-table)
  ;; Вектор сгенерированных состояний dfa-state (индекс массива = id состояния)
  (states (make-array 16 :adjustable t :fill-pointer 0) :type vector)
  ;; Вектор ID стартовых состояний для 64 масок контекста (-1 = не вычислено), 2 типов автомата (привязанный, непривязанный)
  (start-states (make-array 128 :element-type 'fixnum :initial-element -1)
                :type (simple-array fixnum (128)))
  ;; Максимально допустимое число состояний до полного сброса кэша
  (max-states 1000 :type fixnum)
  ;; Выделенные заранее буферы для compute-nfa-closure и compute-nfa-transitions (для избегания аллокаций)
  (nfa-buffer-queue (make-array 16 :element-type 'fixnum 
                                   :fill-pointer 0 
                                   :adjustable t)
                    :type (array fixnum (*)))
  (nfa-buffer-visited #*0 :type (simple-array bit (*)))
)

;;; ----------------------------------------------------------------------------
;;; Вспомогательные функции и конструкторы
;;; ----------------------------------------------------------------------------

;; Выделяет плоский буфер очереди и битовый вектор посещений под размер НКА
(defun allocate-nfa-buffers (nfa-size)
  (values (make-array nfa-size :element-type 'fixnum :adjustable t :fill-pointer 0)
          (make-array nfa-size :element-type 'bit :initial-element 0)
  )
)

;; Создаёт и инициализирует объект ленивого ДКА
(defun make-dfa-instance (nfa max-states)
  (let ((nfa-size (length (nfa-states nfa))))
    (multiple-value-bind (queue visited) (allocate-nfa-buffers nfa-size)
      (make-dfa :nfa nfa
                :max-states max-states
                :nfa-buffer-queue queue
                :nfa-buffer-visited visited)
    )
  )
)