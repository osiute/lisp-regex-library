(in-package :regex-library)

;; ----------------- Вспомогательные функции --------------------
(defun vector-strictly-sorted-p (vec)
  (loop for i from 0 below (1- (length vec))
        always (< (aref vec i) (aref vec (1+ i)))))

;; ----------------- Тесты --------------------
;; Массив из базовых границ Unicode (0 и #x110000)
(defun test-converter-empty (assert-true-fn)
  (let ((vec (get-sorted-endpoints (parse-regex "|"))))
    (funcall assert-true-fn (equalp vec #(0 #x110000)) "пустое AST: вектор #(0 1114112)")
    (funcall assert-true-fn (typep vec 'simple-array) "пустое AST: возвращен simple-array")))

(defun test-converter-complex-email (assert-true-fn)
  (let ((ast (parse-regex "a[0-9]+@b")))
    (let ((vec (get-sorted-endpoints ast)))
      ;; Точки: 0, 48, 58 ('0'-'9'), 64 ('@'), 65, 97, 98 ('a'), 98, 99 ('b'), #x110000
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "email: строгая отсортированность")
      (funcall assert-true-fn (equalp vec #(0 48 58 64 65 97 98 99 #x110000))
               "email: точное совпадение всех точек"))))

(defun test-converter-complex-alt-quantifier (assert-true-fn)
  (let ((ast (parse-regex "(a|b)*[0-9]")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "alt+star: строгая отсортированность")
      (funcall assert-true-fn (equalp vec #(0 48 58 97 98 99 #x110000))
               "alt+star: смежные точки для 'a' и 'b' (97, 98, 99)"))))

(defun test-converter-dot-and-literal (assert-true-fn)
  (let ((ast (parse-regex ".*a")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "dot+literal: отсортированность")
      ;; Должны присутствовать точки от dot (0, 10, 11, 13, 14) и от 'a' (97, 98)
      (funcall assert-true-fn (equalp vec #(0 10 11 13 14 97 98 #x110000))
               "dot+literal: корректный набор точек"))))

(defun test-converter-overlapping-classes (assert-true-fn)
  (let ((ast (parse-regex "[a-z]|[c-f]")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "overlapping: отсортированность")
      (funcall assert-true-fn (equalp vec #(0 97 99 103 123 #x110000))
               "overlapping: разбиение на подинтервалы [a-b], [c-f], [g-z]"))))

(deftest run-endpoints-converter-tests "unicode/endpoints-converter"
  (test-converter-empty #'assert-true)
  (test-converter-complex-email #'assert-true)
  (test-converter-complex-alt-quantifier #'assert-true)
  (test-converter-dot-and-literal #'assert-true)
  (test-converter-overlapping-classes #'assert-true))