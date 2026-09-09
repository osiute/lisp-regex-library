;; dfa API
(in-package :regex-library)

(defun make-lazy-dfa (nfa &key (max-states 1000))
  "Создаёт и инициализует объект DFA по заданному объекту NFA. MAX-STATE — fixnum"
  (assert (>= max-states 2) (max-states)
          "Ошибка при создании ДКА: max-states должен быть не менее 2, получено: ~A" max-states)
  (make-dfa-instance nfa max-states)
)

(defun dfa-get-start-state (dfa context unanchored-p)
  "Вычисляет стартовое состояние DFA с учётом CONTEXT и ANCHORED-P.
  Возвращает id (индекс в DFA-STATES) стартового состояния.
  DFA — объект структуры dfa;
  CONTEXT — 'fixnum, представляющий собой битовую маску контекста, где:
    0 разряд (-------x) — контекст начала строки (абсолютное начало или после '\n', '\r'),
    1 разряд (------x-) — контекст абсолютного начала строки,
    2 разряд (-----x--) — контекст конца строки (абсолютный конец или перед '\n', '\r'),
    3 разряд (----x---) — контекст абсолютного конца строки или сразу перед последним '\n', '\r',
    4 разряд (---x----) — контекст абсолютного конца строки,
    5 разряд (--x-----) — контекст границы слова.
  UNANCHORED-P — 'boolean: если nil, то вычисляет стартовое состояние для привязанного ДКА, иначе — для непривязанного.
  "
  (get-dfa-start-state! dfa context :unanchored-p unanchored-p)
)

;; Вычисляет целевое состояние ДКА для перехода, возвращая его id.
(defun dfa-step-state (dfa cur-state-id class-id context)
  "Вычисляет целевое состояние перехода по CLASS-ID из CUR-STATE-ID для DFA с учётом CONTEXT.
  Возвращает id (индекс в DFA-STATES) целевого состояния.
  DFA — объект структуры dfa;
  CUR-STATE-ID — fixnum, индекс в DFA-STATES состояния, из которого совершается переход;
  CLASS-ID — fixnum, класс эквивалентных символов, по которому совершается переход (label ребра ДКА);
  CONTEXT — 'fixnum, представляющий собой битовую маску контекста, где:
    0 разряд (-------x) — контекст начала строки (абсолютное начало или после '\n', '\r'),
    1 разряд (------x-) — контекст абсолютного начала строки,
    2 разряд (-----x--) — контекст конца строки (абсолютный конец или перед '\n', '\r'),
    3 разряд (----x---) — контекст абсолютного конца строки или сразу перед последним '\n', '\r',
    4 разряд (---x----) — контекст абсолютного конца строки,
    5 разряд (--x-----) — контекст границы слова;
  "
  (dfa-step dfa cur-state-id class-id context)
)

(declaim (inline dfa-accept-state-p))
(defun dfa-accept-state-p (dfa state-id)
  "Определяет, является ли состояние с заданным STATE-ID принимающим.
  DFA — объект dfa;
  STATE-ID — fixnum.
  "
  (declare (type fixnum state-id))
  (dfa-state-accept-p (aref (dfa-states dfa) state-id))
)

(declaim (inline dfa-cache-count))
(defun dfa-cache-count (dfa)
  "Возвращает количество закэшированных состояний DFA (объекта dfa)."
  (length (dfa-states dfa))
)

(declaim (inline reset-dfa!))
(defun reset-dfa! (dfa)
  "Явный сброс кэша DFA (объекта dfa)"
  (flush-dfa-cache! dfa)
)