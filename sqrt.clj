(load-file "assembly.clj")
(load-file "helpers.clj")
(load-file "math.clj")
(load-file "malloc.clj")
(load-file "display.clj")

(defn Sqrt []
  (concat
    (label :sqrt)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, I)

    (SET A, 0x80)
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

(defn Sqrt32 [] ; SQRT( X[1] * 0x10000 + X[0] )
  (concat
    (label :sqrt32)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, I)

    (SET B, X) ; [B] is the number to sqrt

    (SET X, 2)
    (JSR :malloc)
    (SET A, X)
    (SET (ram A 1), 0x0000)
    (SET (ram A 0), 0x8000) ; [A] is the estimated sqrt

    (SET I, 0x10)
    (label :sqrt32_cycle)

    (IFE I, 0)
    (goto :sqrt32_exit)
    (SUB I, 1)

    (SET X, B)
    (SET Y, A)
    (JSR :div32by32)
    (add32plus32 (ram X 1) (ram X) (ram A 1) (ram A) )
    (div32by16   (ram X 1) (ram X) 2)

    (SET (ram A),   (ram X)  )
    (SET (ram A 1), (ram X 1))
    (JSR :free)

    (goto :sqrt32_cycle)

    (label :sqrt32_exit)
    (SET X, A)
    (SET Y, 0)

    (SET I, POP)
    (SET B, POP)
    (SET A, POP)
    (return)
  )
)

(defn main []
  (concat
    (label :main)

    (SET X, 2)
    (JSR :malloc)
    (SET (ram X 0) 0x0000)
    (SET (ram X 1) 0xFFFE)
    (SET C, X)

    (JSR :sqrt32)
    (SET A, X)

    (SET X, C)
    (JSR :free)

    (SET I, (ram A))
    (SET J, (ram A 1))

    (SET X, 0x4000)
    (JSR :sqrt)

    (SET C, X)
    (JSR :print_decimal)
    (JSR :crlf)
    (JSR :print_decimal)

    (label :crash) (goto :crash)
  )
)

(defn SQRT-EXAMPLE-PROGRAM []
  (concat
    (goto :main)

    (Div32by32)
    (Sqrt)
    (Sqrt32)
    (Malloc (label-address :heap_start))
    (Free (label-address :heap_start))
    (PrintAscii)
    (PrintDecimal)
    (CrLf)

    (main)

    (FreeMemory :heap_start)
  )
)
