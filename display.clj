(def screen-start 0x8000)
(def screen-size 0x400)
(def cursor-location 0x7FFF)

(defn PrintAscii []
  (concat
    (label :print_ascii)
    (SET Y, (ram cursor-location))
    (ADD Y, screen-start)
    (SET (ram Y), X)

    (ADD (ram cursor-location), 1)
    (MOD (ram cursor-location), screen-size)
    (return)
  )
)

(defn CrLf []
  (concat
    (label :crlf)
    (DIV (ram cursor-location), 32)
    (MUL (ram cursor-location), 32)
    (ADD (ram cursor-location), 32)
    (MOD (ram cursor-location), screen-size)
    (return)
  )
)

(defn PrintDecimal []
  (concat
    (label :print_decimal)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET A, X)
    (SET B, 10000)

    (label :print_decimal_shrink_b)
    (IFG A, B)
    (goto :print_decimal_digit)
    (DIV B, 10)
    (goto :print_decimal_shrink_b)

    (label :print_decimal_digit)
    (SET C, A)
    (DIV C, B)
    (MOD C, 10)
    (ADD C, 48)

    (SET X, C)
    (JSR :print_ascii)

    (DIV B, 10)
    (IFN B, 0)
    (goto :print_decimal_digit)

    (SET B, POP)
    (SET A, POP)
    (return)
  )
)
