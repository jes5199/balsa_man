(defn SET [dest src]
  [ [:SET dest src] ]
)

(defn JSR [dest]
  [ [:JSR dest] ]
)

(def PUSH :PUSH)
(def POP  :POP )
(def PC   :PC  ) ; program counter
(def SP   :SP  ) ; stack pointer
(def A    :A   )
(def B    :B   )
(def C    :C   )
(def X    :X   )
(def Y    :Y   )
(def Z    :X   )
(def I    :I   )
(def J    :J   )
(def EX   :EX  ) ; overflow

(defn register-plus [register literal] [:register-plus register literal])
(defn literal [value] [[:literal value]])
(defn ram [addr & plus]
  (if (= (count plus) 0)
    [:ram addr]
    (if (= (first plus) 0)
      [:ram addr]
      [:register-plus addr (first plus)]
    )
  )
)

(defn label [s] [[:label s]])
(defn label-address [s] [:label s])

(defn SET [a, b] [[:SET a, b]])
(defn ADD [a, b] [[:ADD a, b]])
(defn SUB [a, b] [[:SUB a, b]])
(defn MUL [a, b] [[:MUL a, b]])
(defn DIV [a, b] [[:DIV a, b]])
(defn MOD [a, b] [[:MOD a, b]])
(defn SHL [a, b] [[:SHL a, b]])
(defn SHR [a, b] [[:SHR a, b]])
(defn AND [a, b] [[:AND a, b]])
(defn BOR [a, b] [[:BOR a, b]])
(defn XOR [a, b] [[:XOR a, b]])
(defn IFE [a, b] [[:IFE a, b]])
(defn IFN [a, b] [[:IFN a, b]])
(defn IFG [a, b] [[:IFG a, b]])
(defn IFB [a, b] [[:IFB a, b]])

(def builtin-instructions #{
  :SET :ADD :SUB :MUL :DIV :MOD
  :SHL :SHR :AND :BOR :XOR
  :IFE :IFN :IFG :IFB
  :JSR
})

(defn word [name value]
  (concat
    (label name) (literal value)
  )
)

(defn hex [value]
  (cond
    (number? value) (str "0x" (format "%04x" value))
    (keyword? value) (name value)
    (and (sequential? value) (= (first value) :label)) (name (second value))
    true (str value)
  )
)

(defn asm-arg [arg]
  (cond
    (keyword? arg) (name arg)
    (integer? arg) (hex arg)
    (= :label (first arg)) (name (second arg))
    (= :ram   (first arg)) (str "[" (asm-arg (second arg)) "]")
    (= :register-plus (first arg)) (str "[" (asm-arg (nth arg 2)) "+" (asm-arg (second arg)) "]")
    true (str arg)
  )
)

(defn assembler-instruction [instruction]
  (cond
    (= :label (first instruction)) (str (keyword (second instruction)))
    (builtin-instructions (first instruction)) (str (name (first instruction)) " " (apply str (interpose ", " (map asm-arg (rest instruction) ))))
    (= :literal (first instruction)) (str "DAT " (hex (second instruction)))
    true (str instruction)
  )
)

(defn assembler [instructions]
  (doall (map (fn [instruction] (println (assembler-instruction instruction))) instructions))
)
