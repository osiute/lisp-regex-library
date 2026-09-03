;; nfa API
(in-package :regex-library)

(defun build-nfa-from-ast (ast-root eq-table)
  "Возвращает объект структуры nfa, построенный по AST-ROOT и EQ-TABLE, где:
  AST-ROOT — объект структуры ast-node;
  EQ-TABLE — объект структуры equivalence-table.
  "
  (let* ((builder (make-nfa-builder))
         (frag (compile-ast-node builder ast-root eq-table)))
    (finalize-nfa builder 
                  (nfa-fragment-start frag) 
                  (nfa-fragment-accept frag)
    )
  )
)

(defun compute-nfa-closure (nfa initial-states context &key queue visited)
  "Вычисляет эпсилон-замыкание для INITIAL-STATES с учётом битового контекста CONTEXT.
  Возвращает новый канонический (отсортированный и статический) вектор fixnum состояний.
  NFA — объект структуры nfa;
  INITIAL-STATES — вектор индексов состояний NFA, для которых нужно вычислить замыкание;
  CONTEXT — 'fixnum, представляющий собой битовую маску контекста, где:
    0 разряд (-------x) — контекст начала строки (абсолютное начало или после '\n', '\r'),
    1 разряд (------x-) — контекст абсолютного начала строки,
    2 разряд (-----x--) — контекст конца строки (абсолютный конец или перед '\n', '\r'),
    3 разряд (----x---) — контекст абсолютного конца строки или сразу перед последним '\n', '\r',
    4 разряд (---x----) — контекст абсолютного конца строки,
    5 разряд (--x-----) — контекст границы слова;
  QUEUE — динамический вектор индексов состояний NFA (чтобы не создавать каждый раз новый), по которому собираются состояния замыкания;
  VISITED — битовый вектор (длины состояний NFA), используемый для отметки уже добавленных в очередь состояний.
  Если QUEUE или VISITED не переданы, они создаются автоматически под размер NFA.
  "
  (let* ((size (length (nfa-states nfa)))
         (actual-queue (or queue (make-array size :fill-pointer 0 :adjustable t)))
         (actual-visited (or visited (make-array size :element-type 'bit :initial-element 0))))
    (epsilon-closure nfa initial-states context actual-queue actual-visited)
  )
)

(defun reverse-nfa (nfa-to-reverse)
  "Принимает NFA-TO-REVERSE (объект структуры nfa) и возвращает новый объект nfa с инвертированным направлением всех рёбер"
 (let ((reversed-nfa (make-raw-reversed-nfa nfa-to-reverse))
        (orig-states (nfa-states nfa-to-reverse)))
    (dotimes (src (length orig-states))
      (reverse-state-edges! reversed-nfa src (aref orig-states src))
    )
    reversed-nfa
  )
)