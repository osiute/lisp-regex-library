;; Реестр уникальных состояний. Занимается идентификацией, проверкой и хранением уникальных dfa-state объектов.
(in-package :regex-library)

;; Проверяет, входит ли принимающее состояние НКА в каноническое подмножество nfa-set
(defun nfa-set-accept-p (nfa-set accept-state-id)
  (declare (type (simple-array fixnum (*)) nfa-set)
           (type fixnum accept-state-id))
  (not (null (find accept-state-id nfa-set :test #'=)))
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