#include "./HATS/includes.hats"
staload "./DATS/lib.dats"
staload _ = "./DATS/lib.dats"
staload "./DATS/arg.dats"

datatype dt = 
| First of ()
| Second of ()

fn hello(s: string): void = println!("HELLO")

implement main(argc, argv) = 0 where {

    vtypedef T = Arg(dt)
    val x = new_arg(Int(), "blah", "")
    val () = add_action(x, lam (s: string) =<cloptr1> hello(s))
    val a1 = new_arg(Bool(), "test", "just a test")
    val () = set_short(a1, "t")
    val () = add_action(a1, lam (s: string) =<cloptr1> println!("test func!: ", s))
    val a2 = new_arg(Float(), "test2", "another test")
    val () = add_action(a2, lam (s: string) =<cloptr1> println!("test2 func!: ", s))
    val a3 = new_arg(String(), "goodbye", "bye")
    val args = new_args("Test")
    val () = set_author(args, "Randy Valis <randy.valis@gmail.com>")
    val () = set_about(args, "This program does nothing")
    val () = add_arg<dt>(args, x)
    val () = add_arg<dt>(args, a2)
    val () = add_arg<dt>(args, a3)
    val () = add_arg<dt>(args, a1)
    val () = parse<dt>(args, argc, argv)
    val y = get_value(args, "test")
    val () = case y of
    | ~Some_vt y => (println!("VALUE: ", y))
    (* | ~Some_vt y => (println!("VALUE: ", y);free(y)) *)
    | ~None_vt() => println!("VALUE: NOTHING")
    val () = free_args<dt>(args)
}
