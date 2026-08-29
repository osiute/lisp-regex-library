(in-package :regex-library)

;; ----------------- Вспомогательные функции --------------------
(defun vector-strictly-sorted-p (vec)
  (loop for i from 0 below (1- (length vec))
        always (< (aref vec i) (aref vec (1+ i)))))

(defun equal-elements-p (vec1 vec2)
  (and (= (length vec1) (length vec2))
       (loop for i from 0 below (length vec1)
             always (= (aref vec1 i) (aref vec2 i)))))

;; ----------------- Тесты --------------------
;; Массив из базовых границ Unicode (0 и (1+ +max-unicode+))
(defun test-converter-empty (assert-true-fn)
  (let ((vec (get-sorted-endpoints (parse-regex "|"))))
    (funcall assert-true-fn (equal-elements-p vec (vector 0 (1+ +max-unicode+))) "пустое AST: вектор (vector 0 1114112)")
    (funcall assert-true-fn (typep vec 'simple-array) "пустое AST: возвращен simple-array")))

(defun test-converter-complex-email (assert-true-fn)
  (let ((ast (parse-regex "a[0-9]+@b")))
    (let ((vec (get-sorted-endpoints ast)))
      ;; Точки: 0, 48, 58 ('0'-'9'), 64 ('@'), 65, 97, 98 ('a'), 98, 99 ('b'), (1+ +max-unicode+)
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "email: строгая отсортированность")
      (funcall assert-true-fn (equal-elements-p vec (vector 0 48 58 64 65 97 98 99 (1+ +max-unicode+)))
               "email: точное совпадение всех точек"))))

(defun test-converter-complex-alt-quantifier (assert-true-fn)
  (let ((ast (parse-regex "(a|b)*[0-9]")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "alt+star: строгая отсортированность")
      (funcall assert-true-fn (equal-elements-p vec (vector 0 48 58 97 98 99 (1+ +max-unicode+)))
               "alt+star: смежные точки для 'a' и 'b' (97, 98, 99)"))))

(defun test-converter-dot-and-literal (assert-true-fn)
  (let ((ast (parse-regex ".*a")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "dot+literal: отсортированность")
      ;; Должны присутствовать точки от dot (0, 10, 11, 13, 14) и от 'a' (97, 98)
      (funcall assert-true-fn (equal-elements-p vec (vector 0 10 11 13 14 97 98 (1+ +max-unicode+)))
               "dot+literal: корректный набор точек"))))

(defun test-converter-overlapping-classes (assert-true-fn)
  (let ((ast (parse-regex "[a-z]|[c-f]")))
    (let ((vec (get-sorted-endpoints ast)))
      (funcall assert-true-fn (vector-strictly-sorted-p vec) "overlapping: отсортированность")
      (funcall assert-true-fn (equal-elements-p vec (vector 0 97 99 103 123 (1+ +max-unicode+)))
               "overlapping: разбиение на подинтервалы [a-b], [c-f], [g-z]"))))

(deftest run-endpoints-converter-tests "unicode/endpoints-converter"
  (test-converter-empty #'assert-true)
  (test-converter-complex-email #'assert-true)
  (test-converter-complex-alt-quantifier #'assert-true)
  (test-converter-dot-and-literal #'assert-true)
  (test-converter-overlapping-classes #'assert-true))