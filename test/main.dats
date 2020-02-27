#include "{$PATSHOMERELOC}/ats-args.hats"

datatype dt = 
| First of ()
| Second of ()

fn get_args(): Args = args where {
    val x = new_arg("blah", "ugh???")
    val a1 = new_arg("test", "just a test")
    val () = a1.set_short("t")
    val () = make_required(a1)
    val a2 = new_arg("test2", "another test")
    val a3 = new_arg("goodbye", "bye")
    val a4 = new_arg("a", "bye")
    val a5 = new_arg("b", "bye")
    val () = a4.set_short("a")
    val () = a5.set_short("b")
    val () = make_required(a4)
    val () = make_required(a5)
    val args = new_args("Test")
    val () = set_author(args, "Randy Valis <randy.valis@gmail.com>")
    val () = set_about(args, "This program does nothing")
    val () = set_version(args, "v0.0.1")
    val () = add_arg(args, x)
    val () = add_arg(args, a2)
    val () = add_arg(args, a3)
    val () = add_arg(args, a1)
    val () = add_arg(args, a4)
    val () = add_arg(args, a5)
}

implement main(argc, argv) = 0 where {
    val args = get_args()
    val res = parse(args, argc, argv)
    val () = case+ res of
    | ~Ok _ => () where {
      val y = get_value<bool>(args, "test")
      val () = case y of
      | ~Some_vt y => () where {
          val z = get_value<bool>(args, "test2")
          val () = case z of
          | ~Some_vt z => println!("VALUE: ", y && z)
          | ~None_vt() => println!("VALUE: z is not a bool")
      }
      | ~None_vt() => ()
      val z = get_value<int>(args, "goodbye")
      val () = case z of
      | ~Some_vt y => println!("GOODBYE: ", y)
      | ~None_vt() => ()
      val y = get_value<int>(args, "a")
      val () = case y of
      | ~Some_vt y => () where {
          val z = get_value<int>(args, "b")
          val () = case z of
          | ~Some_vt z => println!("VALUE: ", z + y)
          | ~None_vt() => println!("error")
      }
      | ~None_vt() => ()
    }
    | ~Error(msg) => () where {
        val () = case+ msg of
                 | ~PrintHelp() => ()
                 | ~Invalid() => println!("Invalid arguments")
                 | ~MissingRequired m => () where {
                        val () = println!("Missing required arguments: ", m)
                        val () = free(m)
                 }
    }
    val () = free_args(args)
}
