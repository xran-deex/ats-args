#include "ats-args.hats"

staload $ARG 
staload $ARGS

datatype Command =
| Get of ()
| Do of ()

fn get_args(): Args = args where {
    val args = new_args("Main3")
    val sc = $SC.new_subcommand("get", "get something")
    val sc2 = $SC.new_subcommand("do", "do something")
    val a1 = new_arg("a1", "")
    val () = a1.set_short("a")
    val () = set_position(a1, 0)
    val a2 = new_arg("b1", "")
    val () = a2.set_short("b")
    val () = set_position(a2, 0)
    val () = $SC.add_arg(sc, a1)
    val () = $SC.add_arg(sc2, a2)
    val () = add_subcommand(args, sc)
    val () = add_subcommand(args, sc2)
    val a3 = new_arg("c1", "")
    val () = a3.set_short("c")
    val () = set_position(a3, 0)
    val () = add_arg(args, a3)
}

implement command_from_string<Command>(s) = res where {
    val () = assertloc(strptr_isnot_null(s))
    val res = (case $UNSAFE.strptr2string(s) of
    | "get" => Some_vt(Get)
    | "do" => Some_vt(Do)
    | _ => None_vt()): Option_vt(Command)
}

implement main(argc, argv) = 0 where {
    val args = get_args()
    val res = parse(args, argc, argv)
    val () = case+ res of
    | ~Ok(_) => () where {
        val opt = get_command<Command>(args)
        val () = case+ opt of
        | ~Some_vt(command) => {
            val () = case+ command of
            | Get() => {
                val () = println!("Get was passed")
                val-~Some_vt(x) = get_value<int>(args, "a1")
                val () = println!("a: ", x)
            }
            | Do() => {
                val () = println!("Do was passed")
                val-~Some_vt(x) = get_value<int>(args, "b1")
                val () = println!("b: ", x)
            }
        }
        | ~None_vt() => {
            val-~Some_vt(x) = get_value<int>(args, "c1")
            val () = println!("x: ", x)
            // val () = println!("Unknown command")
        }
    }
    | ~Error(err) => handle_error(args, err)
    val () = free_args(args)
}
