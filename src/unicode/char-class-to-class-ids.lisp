(in-package :regex-library)

(defun char-class-to-class-ids (node eq-table)
  "Возвращает список уникальных Class ID для узла ast-char-class по объекту equivalence-table."
  (let* ((raw-intervals (ast-ranges-to-id-intervals (ast-char-class-ranges node) eq-table))
         (merged (merge-id-intervals raw-intervals))
         (final-intervals (if (ast-char-class-negated-p node)
                              (invert-id-intervals merged eq-table)
                              merged)))
    (expand-intervals-to-ids final-intervals)
  )
)

;; -----------------------------------------------------------------------------
;; 1. Преобразование диапазонов AST в отрезки Class ID и их сортировка/слияние
;; -----------------------------------------------------------------------------

;; Преобразует диапазоны AST (start . end) в список отрезков (start-id . end-id)
(defun ast-ranges-to-id-intervals (ranges eq-table)
  (let ((intervals nil))
    (dolist (pair ranges)
      (let ((s-id (char-to-class-id (first pair) eq-table))
            (e-id (char-to-class-id (rest pair) eq-table)))
        (push (cons s-id e-id) intervals)
      )
    )
    ;; Сортировка отрезков по левой границе по левой границе start-id
    (sort intervals #'< :key #'first))
)

;; Сливает перекрывающиеся и смежные отрезки Class ID
(defun merge-id-intervals (intervals)
  (unless intervals
    (return-from merge-id-intervals nil)
  )
  (let ((merged (list (copy-tree (first intervals)))))
    (dolist (curr (rest intervals))
      (let ((prev (first merged)))
        (if (<= (first curr) (1+ (rest prev)))
            ;; Отрезки пересекаются или соприкасаются — расширяем правую границу
            (setf (rest prev) (max (rest prev) (rest curr)))
            ;; Разрыв — добавляем новый отрезок
            (push (copy-tree curr) merged)
        )
      )
    )
    (nreverse merged))
)

;; -----------------------------------------------------------------------------
;; 2. Поиск инвертированных отрезков (дыр) и разворачивание в итоговый список
;; -----------------------------------------------------------------------------

;; Находит дополнения (дыры) для сплошных отрезков относительно [0 .. num-classes-1]
(defun invert-id-intervals (merged-intervals eq-table)
  (let ((total (equivalence-table-num-classes eq-table))
        (curr-start 0)
        (inverted nil))
    (dolist (iv merged-intervals)
      (when (< curr-start (first iv))
        (push (cons curr-start (1- (first iv))) inverted)
      )
      (setf curr-start (1+ (rest iv)))
    )
    (when (< curr-start total)
      (push (cons curr-start (1- total)) inverted)
    )
    (nreverse inverted))
)

;; Разворачивает список отрезков (start-id . end-id) в плоский список Class ID
(defun expand-intervals-to-ids (intervals)
  (let ((result nil))
    (dolist (iv intervals (nreverse result))
      (loop for id from (first iv) to (rest iv) do
        (push id result)
      )
    )
  )
)