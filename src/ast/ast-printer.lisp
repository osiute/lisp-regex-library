;; Печать AST-дерева в формате псевдографики
(in-package :regex-library)

(defun print-ast-node (node &key (prefix "") (last-p t) (stream *standard-output*))
  (let ((connector (if last-p "└── " "├── "))
        (child-prefix (concatenate 'string prefix (if last-p "    " "│   "))))
    
    (format stream "~A~A" prefix connector)
    
    (typecase node
      (ast-literal
       (let ((code (ast-literal-code node)))
         (format stream "AST-LITERAL '~C' (code: ~A)~%" (code-char code) code)
        ))

      (ast-anchor
       (format stream "AST-ANCHOR (~A)~%" (ast-anchor-type node)))

      (ast-char-class
       (format stream "AST-CHAR-CLASS (negated: ~A, ranges: ~A)~%"
               (ast-char-class-negated-p node)
               (ast-char-class-ranges node)))

      (ast-star
       (format stream "AST-STAR~%")
       (print-ast-node (ast-star-child node) :prefix child-prefix :last-p t :stream stream))

      (ast-plus
       (format stream "AST-PLUS~%")
       (print-ast-node (ast-plus-child node) :prefix child-prefix :last-p t :stream stream))

      (ast-question
       (format stream "AST-QUESTION~%")
       (print-ast-node (ast-question-child node) :prefix child-prefix :last-p t :stream stream))

      (ast-range
       (format stream "AST-RANGE {~A,~A}~%"
               (or (ast-range-min node) "")
               (or (ast-range-max node) ""))
       (print-ast-node (ast-range-child node) :prefix child-prefix :last-p t :stream stream))

      (ast-concat
       (format stream "AST-CONCAT~%")
       (let* ((elems (ast-concat-elements node))
              (len (length elems)))
         (loop for child in elems
               for i from 1
               do (print-ast-node child 
                                  :prefix child-prefix 
                                  :last-p (= i len) 
                                  :stream stream)
          )
        ))

      (ast-alt
       (format stream "AST-ALT~%")
       (print-ast-node (ast-alt-left node) :prefix child-prefix :last-p nil :stream stream)
       (print-ast-node (ast-alt-right node) :prefix child-prefix :last-p t :stream stream))

      (t
       (format stream "UNKNOWN-NODE: ~A~%" node))
     )
   )
)

(defun print-ast (node &optional (stream *standard-output*))
  "Публичная функция для печати AST-дерева"
  (typecase node
    (null (format stream "NIL~%"))
    (t (print-ast-node node :prefix "" :last-p t :stream stream)))
  node)