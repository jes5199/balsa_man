(load-file "assembly.clj")
(load-file "helpers.clj")

(defn mkHalf [sign exponent mantissa]
  ; 1 sign bit
  (reduce bit-or [
    (if sign 0x8000 0x0)
  ; 5 exponent bits , offset of 15
    (bit-and 0x7C00 (bit-shift-left (+ exponent 15 ) 10))
  ; 10 significant bits
    (bit-and 0x03ff (int mantissa))
  ])
)

(defn pow [x y]
  (. Math pow x y)
)

(defn abs [x]
  (. Math abs x)
)

(defn xor [a b]
  (if a (not b) b)
)

(defn halfZero [sign]
  (mkHalf sign 0 0)
)

(defn halfInfinity [sign]
  (mkHalf sign 16 0)
)

(defn halfNaN []
  (mkHalf false 16 1)
)

(defn mkHalfSubnormal [sign mantissa]
  (mkHalf sign -15 mantissa )
)

(defn halfValue [f]
  (if (= halfValue 0)
    (halfZero false)
    (let [exponent (. Math getExponent (float f))]
      (if (< exponent -14)
        (mkHalfSubnormal (< f 0) (* (abs f) (pow 2 24)) )
        (mkHalf (< f 0) exponent (* (abs f) (pow 2 (- 10 exponent))))
      )
    )
  )
)

;(println (mkHalf false 1 0))
;(doall (map #(println (halfValue %)) [1,1.0009765625,-2,65504, 6.10352e-5, (pow 2 -14), 5.96046e-8 ]))
; (println (halfValue 6.10352e-5))
; (println (halfValue 6.09756e-5))
; (println (halfValue (pow 2 -24)))
; (println (halfValue (- (pow 2 -24))))

;'(println (halfInfinity false))

(defn halfExponent [x]
  (- (bit-shift-right (bit-and 0x7C00 x) 10 ) 15)
)

(defn halfMantissa [x]
  (let [impliedOne (if (== -15 (halfExponent x)) 0 0x0400) ]
    (bit-or impliedOne (bit-and 0x03ff (int x)))
  )
)

(defn halfSign [x]
  (> (bit-and 0x8000 x) 0)
)

(defn halfIsInfinite [x]
  (and (= (halfExponent x) 16)
       (= (halfMantissa x)  0)
  )
)

(defn halfIsNaN [x]
  (and (= (halfExponent x) 16)
       (not= (halfMantissa x) 0)
  )
)

(defn halfIsZero [x]
  (and (= (halfExponent x) -15)
       (= (halfMantissa x)   0)
  )
)

(defn halfIsSubnormal [x]
  (and (= (halfExponent x)    -15)
       (not= (halfMantissa x)   0)
  )
)

(defn mkHalfNormalizing [sign exponent mantissa]
  ;TODO: rounding
  ;(println [sign exponent mantissa])
  (if (> mantissa 0x7FF )
    ; overflows mantissa
    (do
      ;(println :overflow_mantissa)
      (recur sign (+ exponent 1) (bit-shift-right mantissa 1) )
    )

    (if (= 0 (bit-and 0x0400 mantissa))
      ; underflows mantissa
      ( if ( == exponent -14 )
        (mkHalfSubnormal sign mantissa)
        ( if ( < exponent -14 )
          ;shift towards being an underflow
          (do
            ;(println :shift_toward_underflow)
            (recur sign (+ exponent 1) (bit-shift-right mantissa 1) )
          )

          ;shift towards mantissa being in the 1s column
          (do
            ;(println :shift_toward_normal)
            (recur sign (- exponent 1) (bit-shift-left  mantissa 1) )
          )
        )
      )

      ;mantissa is just right
      ( if (> exponent 15)
        ; but we overflowed our exponents
        (halfInfinity sign)
        (if (< exponent -14)
          ; buuut our exponent puts us into underflow-land
          (mkHalfSubnormal sign (bit-shift-right mantissa (- -14 exponent)) )
          ; finally, the happy path.
          (mkHalf sign exponent mantissa)
        )
      )
    )
  )
)

(defn halfEffectiveExponent [x]
  (max (halfExponent x) -14)
)

; for reference, let's write this in clj and port it to dcpu later
(defn multiplyHalfs [a b]
  ;(println [ (halfExponent a) (halfMantissa a) :* (halfExponent b) (halfMantissa b) ] )
  (cond
    (or  (halfIsNaN a)      (halfIsNaN b))      (halfNaN)
    (and (halfIsInfinite a) (halfIsZero b))     (halfNaN)
    (and (halfIsZero a)     (halfIsInfinite b)) (halfNaN)
    (or  (halfIsInfinite a) (halfIsInfinite b)) (halfInfinity (xor (halfSign a) (halfSign b)))
    (or  (halfIsZero a) (halfIsZero b)) (halfZero (xor (halfSign a) (halfSign b)))

    true (mkHalfNormalizing
          (xor (halfSign a) (halfSign b))
          (+ (halfEffectiveExponent a) (halfEffectiveExponent b) -10)
          (* (halfMantissa a) (halfMantissa b))
         )
  )
)

(defn addHalfs [a b]
  ;(println [ (halfExponent a) (halfMantissa a) :+ (halfExponent b) (halfMantissa b) ] )
  ; it's something like, shift the lesser one left
  ; then add mantissas
  ;
  ; TODO: negatives
  (let [
      signA (halfSign a)
      signB (halfSign b)
      expA (halfEffectiveExponent a)
      expB (halfEffectiveExponent b)
    ]
    (if (< expA expB)
        (recur b a)
        ( let
          [bitsA (halfMantissa a)
           bitsB (bit-shift-right (halfMantissa b) (- expA expB))
          ]
          (mkHalfNormalizing signA expA (+ bitsA bitsB))
        )
    )
  )
)

(defn minus [sign]
  (if sign
    "-"
    ""
  )
)

(defn binary [x]
  (format "%010d" (Long/parseLong (Integer/toString x 2)))
)

(defn prettyHalf [x]
  (condp apply [x]
    halfIsNaN (str "NaN:" (minus (halfSign x)) (halfMantissa x))
    halfIsZero (str (minus (halfSign x)) "0")
    halfIsSubnormal (str (minus (halfSign x)) "0." (binary (halfMantissa x)) "b*2^-14" )

    (str (minus (halfSign x)) "1." (binary (bit-and 0x3FF (halfMantissa x))) "b*2^" (halfExponent x) )
  )
)

; (println (halfValue 2.0))
; (println (halfValue (* 2 (pow 2 -24))))
; (println (multiplyHalfs (halfValue 2.0) (halfValue (pow 2 -24))))
; (println (multiplyHalfs (halfValue 4.0) (halfValue (pow 2 -24))))
; (println (halfValue 4.0))
; (println (multiplyHalfs (halfValue 2.0) (halfValue 2.0)))
; (println (multiplyHalfs (halfValue 1.0) (halfValue 4.0)))
; (println (multiplyHalfs (halfValue 0.5) (halfValue 8.0)))

; (println (prettyHalf (halfValue (pow 2 -14))))
; (println (prettyHalf (halfValue (pow 2 -15))))
; (println (prettyHalf (halfValue 2.0)))
; (println (prettyHalf (multiplyHalfs (halfValue 2.0) (halfValue (pow 2 -15)))))
; (println (prettyHalf (multiplyHalfs (halfValue 2.0) (halfValue 2.0))))

(println (prettyHalf (addHalfs (halfValue 5.0) (halfValue 6.0))))

