(load-file "assembly.clj")

; this is gonna be a really abstract partial interpreter
; of DCPU16 assembler
; it cheats in some fundamental ways:
;   code, memory, and stack are separate
;   instructions and pointers are symbolic, not addresses
;
; this should be useful for debugging subroutines, not for simulating a real DCPU

; TODO: clock

(defn mkFCPU []
  {
    :registers {}
    :labels {}
    :programs {}
    :ram {}
    :stack []
  }
)

(defn fcpuSet [fcpu dest src]
  (println [:SET dest src])
  (condp apply [dest]
    registers (update-in fcpu [:registers] assoc dest src)
    fcpu
  )
)

(defn fcpuGet [fcpu src]
  (println [:GET src])
  (condp apply [src]
    integer?  [fcpu, src]
    registers [fcpu, (get-in fcpu [:registers src])]
              [fcpu, (get-in fcpu src)]
  )
)


(defn fcpuAdd [fcpu b a]
  (println [:ADD b a])
  (let [
      [fcpu1 v1] (fcpuGet fcpu  a)
      [fcpu2 v2] (fcpuGet fcpu1 b)
      answer (+ v1 v2)
      fcpu3 (fcpuSet fcpu2 b (mod answer 0x10000))
      fcpu4 (fcpuSet fcpu3 :EX (unchecked-divide answer 0x10000))
    ]
    fcpu4
  )
)

(defn symbolicJmp [amount symbolicPointer]
  (cond
    (integer? symbolicPointer) (+ symbolicPointer amount)
    (= :programs (first symbolicPointer)) (update-in symbolicPointer [2] (partial + amount) )
  )
)

(defn fcpuDecPC [fcpu]
  (update-in fcpu [:registers :PC] (partial symbolicJmp -1) )
)

(defn fcpuIncPC [fcpu]
  (update-in fcpu [:registers :PC] (partial symbolicJmp 1) )
)

(defn fcpuExecuteInstruction [fcpu instruction]
  (println [:executing instruction])
  (let [applyRest (fn [f] (apply (partial f fcpu) (rest instruction)))]
    (condp = (first instruction)
      :SET (applyRest fcpuSet)
      :ADD (applyRest fcpuAdd)
      (fcpuDecPC fcpu)
    )
  )
)


(defn fcpuRun [fcpu]
  (let
    [
      [fcpu2 instruction] (apply fcpuGet (fcpuGet fcpu :PC))
      fcpu3 (fcpuIncPC fcpu)
      fcpu4 (fcpuExecuteInstruction fcpu3 instruction )
    ]
    (if (not= fcpu fcpu4)
      (recur fcpu4)
      fcpu4
    )
  )
)

(defn fcpuLoadProgram [fcpu programName program]
  (update-in fcpu [:programs] assoc programName program )
)

(defn fcpuRunProgram [fcpu programName]
  (fcpuRun (fcpuSet fcpu :PC, [:programs programName 0]))
)


(def addTestProgram
  (vec
    (concat
      (SET A, 1)
      (ADD A, 2)
    )
  )
)

(def addCarryTestProgram
  (vec
    (concat
      (SET A, 0xFFFF)
      (ADD A, 0x1)
    )
  )
)


(defn addTest []
  (let [r (fcpuGet (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :addTest addTestProgram ) :addTest ) :A )]
    (do
      (println r)
      (assert (= (second r) 3))
    )
  )
)

(defn addCarryTest []
  (let [
      state (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :addCarryTest addCarryTestProgram ) :addCarryTest )
      [_ a]  (fcpuGet state :A)
      [_ ex] (fcpuGet state :EX)
    ]
    (do
      (println state)
      (assert (= a  0))
      (assert (= ex 1))
    )
  )
)

;(addTest)
(addCarryTest)

