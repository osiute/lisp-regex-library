;; nfa API
(in-package :regex-library)

(defun build-nfa-from-ast (ast-root eq-table)
  (let ((builder (make-nfa-builder)))
    (let ((frag (compile-ast-node builder ast-root eq-table)))
      (finalize-nfa builder 
                    (nfa-fragment-start frag) 
                    (nfa-fragment-accept frag)
      )
    )
  )
)