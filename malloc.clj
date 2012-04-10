(load-file "assembly.clj")
(load-file "helpers.clj")

(def Nil 0xFFFF)
(def null 0x0)
(def used-memory-marker 0xCAFE)

(defn Initialize [heap-start heap-size]
  ; this is the programatic way to set up your free memory list
  ; it's cheaper to just put the data structure pre-initialized into the binary
  (concat
    (SET X, heap-start)

; FIXME: can't do assembly-time pointer math yet.
;   (SET (ram X), (+ heap-start 1)) ; pointer to free list
    (SET (ram X) heap-start)
    (ADD (ram X) 1)

    (SET (ram X 1) Nil)
    (SET (ram X 2) (- heap-size 1))
  )
)

(def malloc-header-size 2)

(defn Malloc [heap-start]  ; X is size -> X is address
  (concat
    (label :malloc)
    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, C)

    (ADD X, malloc-header-size) ; add room for the header
    ; K&R's malloc rounds to the nearest header size
    ; I don't have allignment concerns, so I'd prefer not to.

    ; start at the beginning of the free list
    (SET B, heap-start)

    (label :malloc_try_frame)
    (SET A, (ram B))

    (IFE A, Nil) (goto :malloc_error)

    ;can't do it if X > frame size
    (IFG X, (ram A 1))
    (goto :malloc_try_next_frame)

    ; if we can't usefully fit another header and the data in this space,
    ; then we'll return the whole block, possibly with up to
    ; header-size words of wasted padding
    (SET C, (ram A 1))
    (SUB C, malloc-header-size)

    (IFG C, X) (goto :malloc_fits_within)

    (label :malloc_exact_or_very_close)
    ; unlink the block
    ; set the previous pointer to our next pointer
    (SET (ram B) (ram A))
    ; set our pointer to debugging symbol
    (SET (ram A), 0xCAFE)
    (goto :malloc_done)

    (label :malloc_fits_within)
    (SUB (ram A 1), X) ; shrink the size

    (ADD A, (ram A 1)) ; move to our new header's location
    ; set our new pointer to debugging symbol
    (SET (ram A), 0xCAFE)
    ; set our new size to requested size
    (SET (ram A 1) X)
    (goto :malloc_done)

    (label :malloc_try_next_frame)
    (SET B, A)
    (goto :malloc_try_frame)

    (label :malloc_error)
    (SET X, null)
    (goto :end_malloc)

    (label :malloc_done)
    (SET X, A)
    (ADD X, malloc-header-size)

    (label :end_malloc)
    (SET C, POP)
    (SET B, POP)
    (SET A, POP)
    (return)
  )
)

(defn Free [heap-start] ; X is address -> void
  (concat
    (label :free)
    (SUB X, malloc-header-size)
    (IFN (ram X) 0xCAFE) (return)

    (SET PUSH, A)
    (SET PUSH, B)
    (SET PUSH, C)

    (SET B, heap-start)

    (label :free_try_this)
    (SET A, (ram B))
    ; if X is greater than A, keep going
    (IFG A, X)
    (goto :free_put_it_here)
    (SET B, (ram B))
    (goto :free_try_this)
    (label :free_put_it_here)

    (label :free_does_b_eat_x)
    ; does the right side of B touch X?
    ; if( B + B[1] == X )
    (SET C, B)
    (ADD C, (ram B, 1))
    (IFN C, X) (goto :free_b_doesnt_eat_x)

    (label :free_b_eats_x)
    ; B[1] += X[1]
    (ADD (ram B,1) (ram X,1))
    (SET (ram X) 0xDEAD)

    (label :free_does_b_eat_a_too)
    ; Now, does B also eat A?
    (ADD C, (ram A, 1))
    (IFN C, A) (goto :free_done)

    ; B[1] += A[1]
    ; B[0] = A[0]
    (label :free_b_eats_a)
    (ADD (ram B 1), (ram A 1))
    (SET (ram B), (ram A))
    (SET (ram A), 0xDEAD)
    (goto :free_done)

    (label :free_b_doesnt_eat_x)
    (label :free_does_x_eat_a)
    ; does the right side of X touch A?
    ; if( x + x[1] == A )
    (SET C, X)
    (ADD C, (ram X, 1))
    (IFN C, A) (goto :free_x_doesnt_eat_a)

    (label :free_x_eats_a)
    ; just have X swallow up A
    (ADD (ram X, 1) (ram A, 1))
    (SET (ram X), (ram A))
    (SET (ram B), X)
    (SET (ram C), 0xDEAD)
    (goto :free_done)

    (label :free_x_doesnt_eat_a)
    ; link X's block in front of A
    (SET (ram X), A)
    ; b[0] = X
    (SET (ram B), X)
    (goto :free_done)

    (label :free_done)
    (SET C, POP)
    (SET B, POP)
    (SET A, POP)
    (return)
  )
)

(defn main []
  (concat
    (label :main)
    (SET X, 31)
    (JSR :malloc)
    (JSR :free)
    (label :crash) (goto :crash)
  )
)

(defn PROGRAM []
  (concat
    (goto :main)
    (Malloc (label-address :heap_start))
    (Free (label-address :heap_start))

    (main)

    ; TODO: offset here to 0x2000 or something like that.

    ; A degenerate free-space marker, also the head of our free list
    (word :heap_start (label-address :ram_free_struct))
    (literal 0)
    ; A legitimate free-space marker
    (word :ram_free_struct Nil)
    (word :ram_free_size 0x7000)
  )
)

(assembler (PROGRAM) )
