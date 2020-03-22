#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

staload $ARG
staload $ARGS

vtypedef arg_result = result_vt((), ArgError)

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
  val () = x.captured_prog_name := None_vt()
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
  val-~Some_vt(_) = x.captured_prog_name
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

fun {a:vt@ype} build_list(ls: !List0_vt(strptr) >> _, init: List0_vt(a)): List_vt(a) = res where {
val res = (case+ ls of
| @list_vt_cons(x, xs) => res where {
  val () = assertloc(strptr_isnot_null x)
  val v = copy(x)
  val-~Some_vt(str_value) = string_to_value<a>(v)
  val () = free(v)
  val res = build_list<a>(xs, list_vt_cons{a}(str_value, init))
  prval() = fold@(ls)
}
| @list_vt_nil() => init where {
  prval() = fold@(ls)
}): List_vt(a)
}

implement {a} get_values(args, key) = res where {
   val+ @ARGS(ar) = args
   val key1 = copy(key)
   val value = $HT.hashtbl_takeout_opt(ar.captured_args, key1)
   val res = (case+ value of
   | ~Some_vt(list) => let
      val() = assertloc(list_vt_length(list) >= 0)
      val list_value = build_list<a>(list, list_vt_nil())
      val-~None_vt() = $HT.hashtbl_insert_opt(ar.captured_args, key1, list)
      in
          list_value
      end
   | ~None_vt() => (free(key1);list_vt_nil())): List_vt(a)
   prval () = fold@(args)
}

fn{} maybe_print(v: string): void = if v != "" then println!(v)

fn{} get_prog_name(ar: !args_struct): string = res where {
  val res = 
    case+ ar.captured_prog_name of
    | @Some_vt(n) => res where {
        val res = n
        prval() = fold@(ar.captured_prog_name)
    }
    | @None_vt() => ar.prog_name where {
        prval() = fold@(ar.captured_prog_name)
    }
}

implement{} print_help(args) = () where {
  val+ @ARGS(ar) = args
  val () = maybe_print(ar.prog_name)
  val () = maybe_print(ar.author)
  val () = maybe_print(ar.about)
  val () = maybe_print(ar.version)
  val () = println!()
  val () = println!("USAGE:")
  val () = println!("\t", get_prog_name ar, " [FLAGS]")
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
  val () = println!("  -h, --help\tThis help message")
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
                                | false => Error(MissingRequired(list_vt_cons(string0_copy x, list_vt_nil())))): result_vt((), ArgError)
                             | ~Error err => res where {
                                  val res = (case+ err of
                                  | ~PrintHelp() => Error(PrintHelp())
                                  | ~Invalid() => Error(Invalid())
                                  | ~MissingValues(v) => Error(MissingValues(v))
                                  | ~MissingRequired m =>
                                      (case+ ref > 0 of
                                      | true => Error(MissingRequired(m))
                                      | false => Error(MissingRequired(list_vt_cons(string0_copy x, m)))
                                      ): result_vt((), ArgError)): result_vt((), ArgError)
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

fn{} has_value(captured: !$HT.hashtbl(strptr, List_vt(strptr)), arg: string): bool = res where {
  val str = string0_copy arg
  val found = $HT.hashtbl_takeout_opt(captured, str)
  val res = case+ found of
  | ~Some_vt(ls) => f where {
      val f = list_vt_length(ls) > 0
      val () = assertloc(strptr_isnot_null str)
      val cpy = copy str
      val-~None_vt() = $HT.hashtbl_insert_opt(captured, cpy, ls)
  }
  | ~None_vt() => f where {
      val f = false
      val () = assertloc(strptr_isnot_null str)
  }
  val () = free(str)
}

fn{} validate_args_that_need_values(args: !Args): arg_result = res where {
  val @ARGS(ar) = args
  vtypedef env = @{ captured=$HT.hashtbl(strptr, List_vt(strptr)), res=arg_result }
  var envir: env = @{ captured=ar.captured_args, res=Ok(()) }
  val () = linmap_foreach_env<string, Arg><env>(ar.args_map, envir) where {
    implement linmap_foreach$fwork<string, Arg><env>(k, arg, envir) = () where {
      val+ @A(a) = arg
      val () = if a.needs_value && ~has_value(envir.captured, a.name) then () where {
        val r = (case+ envir.res of
        | ~Ok(_) => Error(MissingValues(list_vt_cons(copy a.name, list_vt_nil())))
        | ~Error(err) => (case+ err of
             | ~MissingValues(m) => Error(MissingValues(list_vt_cons(copy a.name, m)))
             | ~MissingRequired(ls) => r where {
                val () = list_vt_freelin(ls)
                val r = Ok(())
             }
             | ~PrintHelp() => Ok(())
             | ~Invalid() => Ok(())): arg_result
        ): arg_result
        val () = envir.res := r
      }
      prval() = fold@(arg)
    }
  }
  val () = ar.captured_args := envir.captured
  val res = envir.res
  prval() = fold@args
}

fn{} handle_parse_result(args: !Args, res: arg_result): arg_result =
case+ res of
| ~Ok(_) => r where {
  val req = has_required(args)
  val r = (case+ req of
  | ~Ok(_) => validate_args_that_need_values(args)
  | ~Error(err) => Error(err)): arg_result
}
| ~Error(err) => Error(err)

fn{} add_prog_name(args: !Args, name: string): void = () where {
  val+ @ARGS(ar) = args
  val-~None_vt() = ar.captured_prog_name
  val () = ar.captured_prog_name := Some_vt name
  prval() = fold@(args)
}

fn{} add_prog_name_and_print_help(args: !Args, name: string): arg_result = res where {
  val+ @ARGS(ar) = args
  val-~None_vt() = ar.captured_prog_name
  val () = ar.captured_prog_name := Some_vt name
  val res = Error(PrintHelp)
  prval() = fold@(args)
}

implement{} parse(args, argc, argv) =
case- argc of
| 1 => add_prog_name_and_print_help(args, argv[0])
| _ when argc > 1 => handle_parse_result(args, res) where {
  val () = add_prog_name(args, argv[0])
  val res = parse_args(args, argc, argv)
}

implement{} handle_error(args, err) =
case+ err of
| ~PrintHelp() => println!(args)
| ~Invalid() => println!("\033[32mInvalid\033[0m")
| ~MissingValues v => () where {
  val () = println!("Some arguments are missing required values: \033[31m", v, "\033[0m")
  val () = list_vt_freelin(v)
}
| ~MissingRequired m => () where {
  val () = println!("Missing required args: \033[31m", m, "\033[0m")
  val () = list_vt_freelin(m)
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