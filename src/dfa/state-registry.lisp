;; Реестр уникальных состояний. Занимается идентификацией, проверкой и хранением уникальных dfa-state объектов.
(in-package :regex-library)

;; Проверяет, существует ли состояние с заданным nfa-set.
(declaim (inline dfa-state-exists-p))
(defun dfa-state-exists-p (dfa nfa-set)
  (nth-value 1 (gethash nfa-set (dfa-state-map dfa)))
)

;; Проверяет, входит ли принимающее состояние НКА в каноническое подмножество nfa-set
(defun nfa-set-accept-p (nfa-set accept-state-id)
  (declare (type (simple-array fixnum (*)) nfa-set)
           (type fixnum accept-state-id))
  (loop for x across nfa-set
        thereis (= x accept-state-id))
)

;; Создает объект dfa-state и определяет, является ли оно принимающим
(defun create-dfa-state (nfa-set nfa-accept-id)
  (let ((accept-p (nfa-set-accept-p nfa-set nfa-accept-id)))
    (make-dfa-state :nfa-set nfa-set
                    :accept-p accept-p)
  )
)

;; Добавляет новое состояние в вектор states и заносит его ID в state-map
(defun register-dfa-state! (dfa nfa-set state)
  (let ((new-id (length (dfa-states dfa))))
    (vector-push-extend state (dfa-states dfa))
    ;; Каноническое подмножество НКА → id только что добавленного состояния ДКА
    (setf (gethash nfa-set (dfa-state-map dfa)) new-id)

    new-id
  )
)

;; Возвращает ID существующего состояния, если оно есть.
;; Если состояния нет, выдаёт ошибку.
(defun get-dfa-state (dfa nfa-set)
  (multiple-value-bind (existing-id found-p)
      (gethash nfa-set (dfa-state-map dfa))
    (assert found-p () "dfa/state-registry/get-dfa-state: нет dfa-state для заданного nfa-set. nfa-set — ~S. Я СДЕЛАЛ ПЛОХУЮ ПРОГРАММУ! НУЖНО ИСПОЛЬЗОВАТЬ get-or-register-dfa-state! ВМЕСТО get-dfa-state."
                        nfa-set)
    existing-id
  )
)

;; Возвращает ID существующего состояния или создает и регистрирует новое
(defun get-or-register-dfa-state! (dfa nfa-set)
  (multiple-value-bind (existing-id found-p)
      (gethash nfa-set (dfa-state-map dfa))
    (if found-p
      existing-id
      (let* ((nfa (dfa-nfa dfa))
              (accept-id (nfa-accept-state nfa))
              (new-state (create-dfa-state nfa-set accept-id)))
        (register-dfa-state! dfa nfa-set new-state)
      )
    )
  )
)