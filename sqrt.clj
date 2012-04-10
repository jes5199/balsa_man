(load-file "assembly.clj")
(load-file "helpers.clj")

(defn Sqrt []
  (concat
    (label :sqrt)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, I)

    (SET A, 0xFF)
    (SET I, 0x08)
    (label :sqrt_cycle)

    (IFE I, 0)
    (goto :sqrt_exit)
    (SUB I, 1)

    (SET B, X)
    (DIV B, A)
    (ADD B, A)
    (DIV B, 2)

    (SET A, B)

    (goto :sqrt_cycle)

    (label :sqrt_exit)
    (SET X, A)

    (SET I, POP)
    (SET B, POP)
    (SET A, POP)
    (return)
  )
)

(defn main []
  (concat
    (label :main)
    (SET X, 10000)
    (JSR :sqrt)
    (label :crash) (goto :crash)
  )
)

(defn PROGRAM []
  (concat
    (goto :main)
    (Sqrt)

    (main)
  )
)

(assembler (PROGRAM) )
