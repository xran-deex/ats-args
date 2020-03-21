#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

staload $ARG
staload $ARGS

implement {a} fprint_arg(out, x) = ()

assume Args_vtype = Args
implement{} new_args(prog_name) = args where {
  val args = ARGS(_)
  val ARGS(x) = args 
  val () = x.args_map := linmap_nil()
  val () = x.prog_name := prog_name
  val () = x.about := ""
  val () = x.author := ""
  val () = x.version := ""
  val () = x.captured_args := $HT.hashtbl_make_nil(i2sz 10)
  val () = x.has_all_required := true
  prval () = fold@(args)
}

implement{} set_author(args, author) = () where {
  val @ARGS(x) = args 
  val () = x.author := author
  prval() = fold@(args)
}

implement{} set_about(args, about) = () where {
  val @ARGS(x) = args 
  val () = x.about := about
  prval() = fold@(args)
}

implement{} set_version(args, version) = () where {
  val @ARGS(x) = args 
  val () = x.version := version
  prval() = fold@(args)
}

implement{} add_arg(args, arg) = () where {
  val @ARGS(x) = args 
  val @A(y) = arg
  val name = y.name
  prval() = fold@(arg)
  val () = linmap_insert_any(x.args_map, name, arg)
  prval () = fold@(args)
}

implement fprint_ref<strptr>(o, x) = print!(x)

vtypedef sa = @(Strptr1, Strptr1)

fn{} print_args(args: !Args): void = () where {
    implement(env)
    linmap_foreach$fwork<string, List_vt(strptr)><env>(k, it, e) = println!(k, "->", it)
    val @ARGS(x) = args
    val () = fold@(args)
}

implement linmap_freelin$clear<Arg>(x) =
case x of
| ~A(a) => () where {
  val () = case a.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
}

implement{} free_args(args) = () where {
  val ~ARGS(x) = args
  implement $HT.hashtbl_free$clear<strptr,List_vt(strptr)>(k, v) = () where {
    val () = strptr_free(k)
    val () = list_vt_freelin<strptr>(v)
  }
  val () = $HT.hashtbl_free<strptr,List_vt(strptr)>(x.captured_args)
  val () = linmap_freelin(x.args_map)
}

implement {a} get_value(args, key) = res where {
   val+ @ARGS(ar) = args
   val key1 = copy(key)
   val value = $HT.hashtbl_takeout_opt(ar.captured_args, key1)
   val res = (case+ value of
   | ~Some_vt(list) =>
      (case+ list of
      | ~list_vt_cons(l, ls) => let
        val () = assertloc(strptr_isnot_null l)
        val v = copy(l)
        val str_value = string_to_value<a>(v)
        val () = free(v)
        val list = list_vt_cons(l, ls)
        val-~None_vt() = $HT.hashtbl_insert_opt(ar.captured_args, key1, list)
        in
            str_value
        end
      | ~list_vt_nil() => (free(key1);None_vt()))
   | ~None_vt() => (free(key1);None_vt())): Option_vt(a)
   prval () = fold@(args)
}

implement{} print_help(args) = () where {
  val+ @ARGS(ar) = args
  val () = println!("=== ", ar.prog_name, " ===")
  val () = println!(ar.prog_name)
  val () = println!(ar.author)
  val () = println!(ar.about)
  val () = println!(ar.version)
  val () = println!()
  val () = println!("USAGE:")
  val () = println!("\t", ar.prog_name, " [FLAGS]")
  implement (env)
  linmap_foreach$fwork<string, Arg><env>(k, it, e) = () where {
    val+ @A(x) = it
    val () = print!("  ")
    val () = case x.short of
    | @Some_vt(s) => (print!("-", s, ", ");fold@(x.short))
    | @None_vt() => (fold@(x.short))
    val () = print!("--", x.name)
    val () = print!("\t", x.description)
    val () = case x.required of
    | true => println!("\t(required)")
    | false => println!()
    prval() = fold@(it)
  }
  val () = println!("FLAGS:")
  val () = linmap_foreach(ar.args_map)
  prval () = fold@(args)
}

// gets the list of all required arguments
fn{} get_all_required(args: !Args): List0_vt(string) = res where {
  val @ARGS(ar) = args
  var res: List0_vt(string) = list_vt_nil()
  val () = linmap_foreach_env<string, Arg><List0_vt(string)>(ar.args_map, res) where {
    implement linmap_foreach$fwork<string, Arg><List0_vt(string)>(k, arg, list) = () where {
      val+ @A(a) = arg
      val () = if(a.required) then () where {
        val () = list := list_vt_cons(a.name, list)
      }
      prval() = fold@(arg)
    }
  }
  prval() = fold@(args)
}

// returns an error result if the catured args don't contain all the required args
fn{} has_required(args: !Args): result_vt((), ArgError) = res where {
  val reqs = get_all_required(args)
  fun{} loop(reqs: !List0_vt(string), args: !Args, res: &result_vt((), ArgError)): void = () where {
      val () = case+ reqs of
               | list_vt_nil() => ()
               | list_vt_cons(x, xs) => () where {
                    val+ @ARGS(ar) = args
                    val cpy = copy(x)
                    val ref = $HT.hashtbl_search_ref(ar.captured_args, cpy)
                    val () = free(cpy)
                    val res2 = (case+ res of
                             | ~Ok _ => 
                                (case+ ref > 0 of
                                | true => Ok(())
                                | false => Error(MissingRequired(string0_copy x))): result_vt((), ArgError)
                             | ~Error err => res where {
                                  val res = (case+ err of
                                  | ~PrintHelp() => Error(PrintHelp())
                                  | ~Invalid() => Error(Invalid())
                                  | ~MissingRequired m => 
                                      (case+ ref > 0 of
                                      | true => Error(MissingRequired(m))
                                      | false => res where {
                                          val w = $UNSAFE.castvwtp0{strptr}(x)
                                          val comma = $UNSAFE.castvwtp0{strptr}(", ")
                                          val y = strptr_append(m, comma)
                                          val y' = strptr_append(y, w)
                                          val () = free(m)
                                          val () = free(y)
                                          prval () = $UNSAFE.cast2void(w)
                                          prval () = $UNSAFE.cast2void(comma)
                                          val res = Error(MissingRequired(y'))
                                      }): result_vt((), ArgError)): result_vt((), ArgError)
                             }): result_vt((), ArgError)
                    val () = res := res2
                    prval() = fold@(args)
                    val () = loop(xs, args, res)
                }
  }
  var res = Ok(())
  val () = loop(reqs, args, res)
  val () = free(reqs)
}

fn{} handle_parse_result(args: !Args, res: result_vt((), ArgError)): result_vt((), ArgError) =
case+ res of
| ~Ok(_) => has_required(args)
| ~Error(err) => Error(err)

implement{} parse(args, argc, argv) =
case- argc of
| 1 => Error(PrintHelp)
| _ when argc > 1 => handle_parse_result(args, res) where {
  val res = parse_args(args, argc, argv)
}

implement{} handle_error(args, err) =
case+ err of
| ~PrintHelp() => println!(args)
| ~Invalid() => println!("\033[32mInvalid\033[0m")
| ~MissingRequired m => () where {
  val () = println!("Missing required args: \033[31m", m, "\033[0m")
  val () = free(m)
}

implement string_to_value<int>(v) = let
  val tmp = g1string2int($UNSAFE.castvwtp1{string}(v))
in
  // if we get back a 0 and the string was not "0", then the string is not a number
  ifcase
  | tmp = 0 && v != "0" => None_vt()
  | _ => Some_vt(tmp)
end

implement string_to_value<bool>(v) = 
ifcase
| v = "true" => Some_vt(true)
| v = "True" => Some_vt(true) 
| v = "t" => Some_vt(true)
| v = "false" => Some_vt(false)
| v = "False" => Some_vt(false) 
| v = "f" => Some_vt(false)
| _ => None_vt()

implement string_to_value<strptr>(v) = let
  val s = copy v
in
  Some_vt(s)
end