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

(defn fcpuGet [fcpu src]
  ;(println [:GET src])
  (condp apply [src]
    integer?  [fcpu, src]
    registers [fcpu, (get-in fcpu [:registers src])]
              [fcpu, (get-in fcpu src)]
  )
)

(defn fcpuSet [fcpu dest src]
  (condp apply [dest]
    registers (update-in fcpu [:registers] assoc dest src)
    fcpu
  )
)


(defn fcpuAdd[fcpu b a]
  ;(println [:ADD b a])
  (let [
      [fcpu v1] (fcpuGet fcpu a)
      [fcpu v2] (fcpuGet fcpu b)
      answer (+ v2 v1)
      fcpu (fcpuSet fcpu b   (mod answer 0x10000))
      fcpu (fcpuSet fcpu :EX (bit-shift-right answer 16))
    ]
    fcpu
  )
)

(defn fcpuSub [fcpu b a]
  ;(println [:SUB b a])
  (let [
      [fcpu v1] (fcpuGet fcpu a)
      [fcpu v2] (fcpuGet fcpu b)
      answer (- v2 v1)
      fcpu (fcpuSet fcpu b   (mod answer 0x10000))
      fcpu (fcpuSet fcpu :EX (mod (bit-shift-right answer 16) 0x10000))
    ]
    fcpu
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

(def instructionFunctions
  {
    :SET fcpuSet
    :ADD fcpuAdd
    :SUB fcpuSub
  }
)

(defn fcpuExecuteInstruction [fcpu instruction]
  ;(println [:executing instruction])
  (let [applyInstructionFunction (fn [f] (apply (partial f fcpu) (rest instruction)))]
    (condp apply [(first instruction)]
      instructionFunctions :>> applyInstructionFunction
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

(def subTestProgram
  (vec
    (concat
      (SET A, 0x3)
      (SUB A, 0x2)
    )
  )
)

(def subBorrowTestProgram
  (vec
    (concat
      (SET A, 0x1)
      (SUB A, 0xFFFF)
    )
  )
)

(defn addTest []
  (let [
      state (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :addTest addTestProgram ) :addTest )
      [_ a]  (fcpuGet state :A)
      [_ ex] (fcpuGet state :EX)
    ]
    (do
      ;(println state)
      (assert (= a  3))
      (assert (= ex 0))
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
      ;(println state)
      (assert (= a  0))
      (assert (= ex 1))
    )
  )
)

(defn subTest []
  (let [
      state (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :subTest subTestProgram ) :subTest )
      [_ a]  (fcpuGet state :A)
      [_ ex] (fcpuGet state :EX)
    ]
    (do
      ;(println state)
      (assert (= a  1))
      (assert (= ex 0))
    )
  )
)

(defn subBorrowTest []
  (let [
      state (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :subBorrowTest subBorrowTestProgram ) :subBorrowTest )
      [_ a]  (fcpuGet state :A)
      [_ ex] (fcpuGet state :EX)
    ]
    (do
      ;(println state)
      (assert (= a  2))
      (assert (= ex 0xFFFF))
    )
  )
)

(addTest)
(addCarryTest)
(subTest)
(subBorrowTest)

