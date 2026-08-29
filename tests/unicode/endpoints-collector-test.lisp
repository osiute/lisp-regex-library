(in-package :regex-library)

(defun endpoint-present-p (set code)
  (gethash code set))

(deftest run-endpoints-collector-tests "unicode/endpoints-collector"
  (test-endpoints-empty #'assert-true #'assert-equal)
  (test-endpoints-literal #'assert-true)
  (test-endpoints-dot #'assert-true)
  (test-endpoints-multi-range #'assert-true)
  (test-endpoints-unicode-boundaries #'assert-true)
  (test-endpoints-star #'assert-true)
  (test-endpoints-alternation #'assert-true)
  (test-endpoints-invalid-range #'assert-error))

(defun test-endpoints-empty (assert-true-fn assert-equal-fn)
  (let ((set (extract-endpoints-from-ast nil)))
    (funcall assert-true-fn (endpoint-present-p set 0) "пустое AST: база 0")
    (funcall assert-true-fn (endpoint-present-p set (1+ +max-unicode+)) "пустое AST: база (1+ +max-unicode+)")
    (funcall assert-equal-fn (hash-table-count set) 2 "пустое AST: ровно 2 точки")))

(defun test-endpoints-literal (assert-true-fn)
  (let ((ast (make-ast-literal :char #\a)))
    (let ((set (extract-endpoints-from-ast ast)))
      (funcall assert-true-fn (endpoint-present-p set 97) "литерал 'a': левая граница (97)")
      (funcall assert-true-fn (endpoint-present-p set 98) "литерал 'a': правая граница (98)"))))

;; '.', исключая '\n' и '\r'
(defun test-endpoints-dot (assert-true-fn)
  (let ((ast (make-ast-dot)))
    (let ((set (extract-endpoints-from-ast ast)))
      (funcall assert-true-fn (endpoint-present-p set 0) "dot: начало Unicode (0)")
      (funcall assert-true-fn (endpoint-present-p set 10) "dot: граница перед \\n (10)")
      (funcall assert-true-fn (endpoint-present-p set 11) "dot: граница после \\n (11)")
      (funcall assert-true-fn (endpoint-present-p set 13) "dot: граница перед \\r (13)")
      (funcall assert-true-fn (endpoint-present-p set 14) "dot: граница после \\r (14)"))))

;; Символьный класс с несколькими диапазонами [0-9a-z]
(defun test-endpoints-multi-range (assert-true-fn)
  (let ((ast (make-ast-char-class :ranges '((#\0 . #\9) (#\a . #\z)))))
    (let ((set (extract-endpoints-from-ast ast)))
      (funcall assert-true-fn (endpoint-present-p set 48) "класс: левая граница '0' (48)")
      (funcall assert-true-fn (endpoint-present-p set 58) "класс: правая граница '9' + 1 (58)")
      (funcall assert-true-fn (endpoint-present-p set 97) "класс: левая граница 'a' (97)")
      (funcall assert-true-fn (endpoint-present-p set 123) "класс: правая граница 'z' + 1 (123)"))))

;; Символы на границах диапазонов Unicode (U+0000 и U+10FFFF)
(defun test-endpoints-unicode-boundaries (assert-true-fn)
  (let ((char-min (code-char 0))
        (char-max (code-char +max-unicode+)))
    (let ((ast (make-ast-char-class :ranges `((,char-min . ,char-max)))))
      (let ((set (extract-endpoints-from-ast ast)))
        (funcall assert-true-fn (endpoint-present-p set 0) "unicode: мин граница (0)")
        (funcall assert-true-fn (endpoint-present-p set (1+ +max-unicode+)) "unicode: макс граница ((1+ +max-unicode+))")))))

(defun test-endpoints-star (assert-true-fn)
  (let ((ast (make-ast-star :child (make-ast-literal :char #\z))))
    (let ((set (extract-endpoints-from-ast ast)))
      (funcall assert-true-fn (endpoint-present-p set 122) "star: левая граница 'z' (122)")
      (funcall assert-true-fn (endpoint-present-p set 123) "star: правая граница 'z' (123)"))))

(defun test-endpoints-alternation (assert-true-fn)
  (let ((ast (make-ast-alt :left (make-ast-literal :char #\a)
                          :right (make-ast-literal :char #\b))))
    (let ((set (extract-endpoints-from-ast ast)))
      (funcall assert-true-fn (endpoint-present-p set 97) "alt: левая 'a' (97)")
      (funcall assert-true-fn (endpoint-present-p set 98) "alt: смежная граница 'a'/'b' (98)")
      (funcall assert-true-fn (endpoint-present-p set 99) "alt: правая 'b' (99)"))))

(defun test-endpoints-invalid-range (assert-error-fn)
  (funcall assert-error-fn
           (lambda () (add-endpoint-range (make-hash-table) 100 50))
           "ошибка: start-code (100) > end-code (50)"))