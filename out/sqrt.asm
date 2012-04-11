SET PC, main
:div32by32
SET PUSH, A
SET PUSH, B
SET A, X
SET B, Y
SET X, 0x0002
JSR malloc
SET Y, X
SET [Y], [B]
SET [0x0001+Y], [0x0001+B]
SET X, 0x0002
JSR malloc
SET [X], [A]
SET [0x0001+X], [0x0001+A]
:div32by32_next
IFE [0x0001+Y], 0x0000
SET PC, div32by32_exit
SHR [X], 0x0001
SHR [0x0001+X], 0x0001
ADD [X], O
SHR [Y], 0x0001
SHR [0x0001+Y], 0x0001
ADD [Y], O
SET PC, div32by32_next
:div32by32_exit
DIV [X], [Y]
DIV [0x0001+X], [Y]
ADD [X], O
SET PUSH, X
SET X, Y
JSR free
SET X, POP
SET B, POP
SET A, POP
SET PC, POP
:sqrt
SET PUSH, A
SET PUSH, B
SET PUSH, I
SET A, 0x0080
SET I, 0x0008
:sqrt_cycle
IFE I, 0x0000
SET PC, sqrt_exit
SUB I, 0x0001
SET B, X
DIV B, A
ADD B, A
DIV B, 0x0002
SET A, B
SET PC, sqrt_cycle
:sqrt_exit
SET X, A
SET I, POP
SET B, POP
SET A, POP
SET PC, POP
:sqrt32
SET PUSH, A
SET PUSH, B
SET PUSH, I
SET B, X
SET X, 0x0002
JSR malloc
SET A, X
SET [0x0001+A], 0x0000
SET [A], 0x8000
SET I, 0x0010
:sqrt32_cycle
IFE I, 0x0000
SET PC, sqrt32_exit
SUB I, 0x0001
SET X, B
SET Y, A
JSR div32by32
ADD [X], [A]
ADD [0x0001+X], O
ADD [0x0001+X], [0x0001+A]
DIV [X], 0x0002
DIV [0x0001+X], 0x0002
ADD [X], O
SET [A], [X]
SET [0x0001+A], [0x0001+X]
JSR free
SET PC, sqrt32_cycle
:sqrt32_exit
SET X, A
SET Y, 0x0000
SET I, POP
SET B, POP
SET A, POP
SET PC, POP
:malloc
SET PUSH, A
SET PUSH, B
SET PUSH, C
ADD X, 0x0002
SET B, heap_start
:malloc_try_frame
SET A, [B]
IFE A, 0xffff
SET PC, malloc_error
IFG X, [0x0001+A]
SET PC, malloc_try_next_frame
SET C, [0x0001+A]
SUB C, 0x0002
IFG C, X
SET PC, malloc_fits_within
:malloc_exact_or_very_close
SET [B], [A]
SET [A], 0xcafe
SET PC, malloc_done
:malloc_fits_within
SUB [0x0001+A], X
ADD A, [0x0001+A]
SET [A], 0xcafe
SET [0x0001+A], X
SET PC, malloc_done
:malloc_try_next_frame
SET B, A
SET PC, malloc_try_frame
:malloc_error
SET X, 0x0000
SET PC, end_malloc
:malloc_done
SET X, A
ADD X, 0x0002
:end_malloc
SET C, POP
SET B, POP
SET A, POP
SET PC, POP
:free
SUB X, 0x0002
IFN [X], 0xcafe
SET PC, POP
SET PUSH, A
SET PUSH, B
SET PUSH, C
SET B, heap_start
:free_try_this
SET A, [B]
IFG A, X
SET PC, free_put_it_here
SET B, [B]
SET PC, free_try_this
:free_put_it_here
:free_does_b_eat_x
SET C, B
ADD C, [0x0001+B]
IFN C, X
SET PC, free_b_doesnt_eat_x
:free_b_eats_x
ADD [0x0001+B], [0x0001+X]
SET [X], 0xdead
:free_does_b_eat_a_too
ADD C, [0x0001+A]
IFN C, A
SET PC, free_done
:free_b_eats_a
ADD [0x0001+B], [0x0001+A]
SET [B], [A]
SET [A], 0xdead
SET PC, free_done
:free_b_doesnt_eat_x
:free_does_x_eat_a
SET C, X
ADD C, [0x0001+X]
IFN C, A
SET PC, free_x_doesnt_eat_a
:free_x_eats_a
ADD [0x0001+X], [0x0001+A]
SET [X], [A]
SET [B], X
SET [C], 0xdead
SET PC, free_done
:free_x_doesnt_eat_a
SET [X], A
SET [B], X
SET PC, free_done
:free_done
SET C, POP
SET B, POP
SET A, POP
SET PC, POP
:main
SET X, 0x0002
JSR malloc
SET [X], 0x0000
SET [0x0001+X], 0xfffe
SET C, X
JSR sqrt32
SET A, X
SET X, C
JSR free
SET I, [A]
SET J, [0x0001+A]
SET X, 0x4000
JSR sqrt
:crash
SET PC, crash
:heap_start
DAT ram_free_struct
DAT 0x0000
:ram_free_struct
DAT 0xffff
:ram_free_size
DAT 0x7000
