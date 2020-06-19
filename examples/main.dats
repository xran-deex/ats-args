#include "ats-args.hats"

staload $ARG 
staload $ARGS

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
    val () = make_required(a3)
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

vtypedef cli = @{
    a = int,
    b = int,
    test = Option_vt(strptr)
}

implement get_parsed<cli>(args) = let
val- ~Some_vt(a_opt) = get_value<int>(args, "a")
val- ~Some_vt(b_opt) = get_value<int>(args, "b")
val cli = @{
    a = a_opt,
    b = b_opt,
    test = get_value<strptr>(args, "goodbye")
}
in
cli
end

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
                 | ~PrintHelp() => println!(args)
                 | ~Invalid() => println!("Invalid arguments")
                 | ~MissingValues v => list_vt_freelin(v)
                 | ~MissingRequired m => () where {
                        val () = println!("\33[31mError:\33[0m Missing required arguments: \33[92m", m, "\33[0m")
                        val () = list_vt_freelin(m)
                 }
    }
    val c = get_parsed<cli>(args)
    val b = c.b
    val a = c.a
    val () = println!(a, ", ", b)
    val () = fprint_option_vt(stdout_ref, c.test)
    val () = println!()
    val () = case+ c.test of | ~Some_vt(s) => free(s) | ~None_vt() => ()
    val () = free_args(args)
}
