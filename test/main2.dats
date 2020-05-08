#include "ats-args.hats"

staload $ARG 
staload $ARGS

fn get_args(): Args = args where {
    val a1 = new_arg("test", "just a test")
    val () = a1.set_short("t")
    val () = make_required(a1)
    val () = set_needs_value(a1)
    val a2 = new_arg("test2", "just a test")
    val () = a2.set_short("y")
    val () = make_required(a2)
    val a3 = new_arg("test3", "just a test")
    val () = set_position(a3, Pos(1))
    val a4 = new_arg("test4", "just a test")
    val () = set_position(a4, Pos(2))
    val args = new_args("Main2")
    // val () = set_author(args, "Randy Valis <randy.valis@gmail.com>")
    // val () = set_about(args, "Another test")
    // val () = set_version(args, "v0.0.1")
    val () = add_arg(args, a1)
    val () = add_arg(args, a2)
    val () = add_arg(args, a3)
    val () = add_arg(args, a4)
}

vtypedef cli = @{
    test = List_vt(int),
    test2 = List_vt(int)//Option_vt(int)
}

implement get_parsed<cli>(args) = let
val test_ls = get_values<int>(args, "t")
val cli = @{
    test = test_ls,
    test2 = get_values<int>(args, "y")
}
in
cli
end

fn{} addem(ls: List_vt(int)): void = () where {
    fun{} loop(ls: List_vt(int), res: int): int =
        case+ ls of
        | ~list_vt_cons(x, xs) => loop(xs, res + x)
        | ~list_vt_nil() => res
    val () = println!("Sum: ", loop(ls, 0))
}

implement main(argc, argv) = 0 where {
    val args = get_args()
    val res = parse(args, argc, argv)
    val () = case+ res of
    | ~Ok(_) => () where {
        val c = get_parsed<cli>(args)
        val test = c.test
        val test2 = c.test2
        val v = get_values<strptr>(args, "test3")
        val () = println!("test3: ", v)
        val () = list_vt_freelin(v)
        val v = get_values<strptr>(args, "test4")
        val () = println!("test4: ", v)
        val () = list_vt_freelin(v)
        val () = addem(test)
        // val () = println!("test: ", test)
        // val () = print!("test2: ")
        // val () = fprint_option_vt(stdout_ref, test2)
        // val () = println!()
        // val () = case+ test2 of | ~Some_vt(_) => () | ~None_vt() => ()
        val () = println!("test2: ", test2)
        val () = list_vt_free(test2)
    }
    | ~Error(err) => handle_error(args, err)
    val () = free_args(args)
}
