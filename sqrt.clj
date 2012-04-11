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

(defn Sqrt32 [] ; SQRT( Y * 0x10000 + X )
  (concat
    (label :sqrt32)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, C)
    (SET PUSH, I)
    (SET PUSH, J)

    (SET J, 0x0000)
    (SET A, 0xFFFF)
    (SET I, 0x30)
    (label :sqrt32_cycle)

    (IFE I, 0)
    (goto :sqrt32_exit)
    (SUB I, 1)

    (SET C, Y) (SET B, X)
    (div32by32   C, B, J, A)
    (add32plus32 C, B, J, A)
    (div32by16   C, B, 2)

    (SET J, C)
    (SET A, B)

    (goto :sqrt32_cycle)

    (label :sqrt32_exit)
    (SET X, A)
    (SET Y, 0)

    (SET J, POP)
    (SET I, POP)
    (SET C, POP)
    (SET B, POP)
    (SET A, POP)
    (return)
  )
)

(defn main []
  (concat
    (label :main)
    (SET X, 0xFFFF)
    (SET Y, 0xFFFF)
    (JSR :sqrt32)
    (label :crash) (goto :crash)
  )
)

(defn PROGRAM []
  (concat
    (goto :main)
    (Sqrt32)

    (main)
  )
)

(assembler (PROGRAM) )
