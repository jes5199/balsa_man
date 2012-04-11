(defn goto [addr]
  (SET PC, addr)
)

(defn return []
  (goto POP)
)

(defn add32plus16 [high low plus]
  (concat
    (ADD low, plus)
    (ADD high, O)
  )
)

(defn add32plus32 [high low plushigh pluslow]
  (concat
    (ADD low, pluslow)
    (ADD high, O)
    (ADD high, plushigh)
  )
)


(defn div32by16 [high low divisor]
  (concat
    (DIV low, divisor)
    (DIV high, divisor)
    (ADD low, O)
  )
)

(defn rightshift32 [high low amount]
  (concat
    (SHR low, amount)
    (SHR high, amount)
    (ADD low, O)
  )
)

(defn div32by32 [high low divisorhigh divisorlow]
  ; This may be complicated enough to become a function instead of a macro.
  (let [
      exit_label  (keyword (gensym "div32by32_exit"))
      start_label (keyword (gensym "div32by32_start"))
    ]
    (concat
      (label start_label)

      (IFE divisorhigh 0) (goto exit_label)
      (rightshift32 divisorhigh divisorlow 1)
      (rightshift32 high low 1)
      (goto start_label)

      (label exit_label)
      (div32by16 high low divisorlow)
    )
  )
)
