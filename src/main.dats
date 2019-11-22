#include "./HATS/includes.hats"
staload "./DATS/lib.dats"
staload _ = "./DATS/lib.dats"

datatype dt = 
| First of ()
| Second of ()

implement main(argc, argv) = 0 where {

    vtypedef T = Arg(dt)
    (* val a1 = B(First(), "hello") *)
    (* val a2 = B(Second(), "test2") *)
    val a1 = A(@{ idx=First(), name="hello" })
    val a2 = A(@{ idx=Second(), name="test2" })
    val a3 = A(@{ idx=First(), name="goodbye" })
    val args = new_args("Test")
    val () = parse(args, argc, argv)
    val () = add_arg(args, a1)
    val () = add_arg(args, a2)
    val () = add_arg(args, a3)
    val () = free_args(args)
}
