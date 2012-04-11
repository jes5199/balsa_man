(defn goto [addr]
  (SET PC, addr)
)

(defn return []
  (goto POP)
)

