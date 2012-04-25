(defn add32plus16 [high low plus]
  (concat
    (ADD low, plus)
    (ADD high, EX)
  )
)

(defn add32plus32 [high low plushigh pluslow]
  (concat
    (ADD low, pluslow)
    (ADD high, EX)
    (ADD high, plushigh)
  )
)


(defn div32by16 [high low divisor]
  (concat
    (DIV low, divisor)
    (DIV high, divisor)
    (ADD low, EX)
  )
)

(defn rightshift32 [high low amount]
  (concat
    (SHR low, amount)
    (SHR high, amount)
    (ADD low, EX)
  )
)

(defn Div32by32 []
  (concat
    (label :div32by32)
    (SET PUSH, A)
    (SET PUSH, B)

    (SET A, X)
    (SET B, Y)

    (SET X, 2)
    (JSR :malloc)
    (SET Y, X)
    (SET (ram Y  ), (ram B  ))
    (SET (ram Y 1), (ram B 1))

    (SET X, 2)
    (JSR :malloc)
    (SET (ram X  ), (ram A  ))
    (SET (ram X 1), (ram A 1))

    (label :div32by32_next)

    (IFE (ram Y 1) 0) (goto :div32by32_exit)
    (rightshift32 (ram X 1) (ram X) 1)
    (rightshift32 (ram Y 1) (ram Y) 1)
    (goto :div32by32_next)

    (label :div32by32_exit)
    (div32by16 (ram X 1) (ram X) (ram Y))

    (SET PUSH, X)
    (SET X, Y)
    (JSR :free)
    (SET X, POP)

    (SET B, POP)
    (SET A, POP)
    (return)
  )
)
