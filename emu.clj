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
    #{:A :PC} (update-in fcpu [:registers] assoc dest src)
    fcpu
  )
)

(defn fcpuGet [fcpu src]
  (println [:GET src])
  (condp apply [src]
    integer?  [fcpu, src]
    #{:A :PC} [fcpu, (get-in fcpu [:registers src])]
              [fcpu, (get-in fcpu src)]
  )
)


(defn fcpuAdd [fcpu b a]
  (println [:ADD b a])
  (let [
      [fcpu1 v1] (fcpuGet fcpu  a)
      [fcpu2 v2] (fcpuGet fcpu1 b)
      answer (+ v1 v2)
      fcpu3 (fcpuSet fcpu b answer)
    ]
    fcpu3
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


(defn addTest []
  (let [r (fcpuGet (fcpuRunProgram (fcpuLoadProgram (mkFCPU) :addTest addTestProgram ) :addTest ) :A )]
    (do
      (println r)
      (assert (= (second r) 3))
    )
  )
)

(addTest)
