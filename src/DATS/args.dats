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
  val () = x.subcommand := None_vt()
  val () = x.command_map := linmap_nil()
  val () = linmap_insert_any(x.command_map, "", $SC.new_subcommand("", ""))
  val () = x.prog_name := prog_name
  val () = x.about := ""
  val () = x.author := ""
  val () = x.version := ""
  val () = x.captured_args := $HT.hashtbl_make_nil(i2sz 10)
  val () = x.has_all_required := true
  val () = x.captured_prog_name := None_vt()
  val () = x.captured_command := None_vt()
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
  val-~Some_vt(sc) = linmap_takeout_opt(x.command_map, "")
  val () = $SC.add_arg(sc, arg)
  val () = linmap_insert_any(x.command_map, "", sc)
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

implement{} free_args(args) = () where {
  val ~ARGS(x) = args
  implement $HT.hashtbl_free$clear<strptr,List_vt(strptr)>(k, v) = () where {
    val () = strptr_free(k)
    val () = list_vt_freelin<strptr>(v)
  }
  val () = $HT.hashtbl_free<strptr,List_vt(strptr)>(x.captured_args)
  val-~Some_vt(_) = x.captured_prog_name
  val () = linmap_freelin(x.command_map)
  val () = case+ x.subcommand of
  | ~Some_vt(sc) => $SC.free_subcommand(sc)
  | ~None_vt() => ()
  val () = case+ x.captured_command of | ~Some_vt(c) => free(c) | ~None_vt() => ()
}

fn{} get_long_name_from_short(args: !Args, short: string): string = long where {
  val+@ARGS(ars) = args
  typedef state = @{ key=string, short=string }
  var key: state = @{ key="", short=short }
  val-@Some_vt(sc) = ars.subcommand
  val+@$SC.SC(s) = sc
  val arg = linmap_foreach_env<string,Arg><state>(s.args_map, key) where {
    implement linmap_foreach$fwork<string,Arg><state>(k, v, e) = {
      val+@A(a) = v
      val () = case+ a.short of
      | @None_vt() => fold@(a.short)
      | @Some_vt(sh) => fold@(a.short) where {
        val () = if sh = e.short then {
          val () = e.key := a.name
        }
      }
      prval() = fold@(v)
    }
  }
  val long = key.key
  prval() = fold@(sc)
  prval() = fold@(ars.subcommand)
  prval() = fold@(args)
}

fn{} get_short_name_from_long(args: !Args, long: string): string = short where {
  val+@ARGS(ars) = args
  typedef state = @{ key=string, long=string }
  var key: state = @{ key="", long=long }
  val-@Some_vt(sc) = ars.subcommand
  val+@$SC.SC(s) = sc
  val arg = linmap_foreach_env<string,Arg><state>(s.args_map, key) where {
    implement linmap_foreach$fwork<string,Arg><state>(k, v, e) = {
      val+@A(a) = v
      val () = case+ a.short of
      | @None_vt() => fold@(a.short)
      | @Some_vt(sh) => fold@(a.short) where {
        val () = if a.name = e.long then {
          val () = e.key := sh
        }
      }
      prval() = fold@(v)
    }
  }
  val short = key.key
  prval() = fold@(sc)
  prval() = fold@(ars.subcommand)
  prval() = fold@(args)
}

implement {a} get_value(args, key) = res where {
   val+ @ARGS(ar) = args
   // always use the long name
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
   val opt = get_short_and_long(s.args_map, g1ofg0 key)
  prval() = fold@(sc)
  prval() = fold@(ar.subcommand)
   val key1 = (case+ opt of
   | ~Some_vt(p) => copy(p.0)
   | ~None_vt() => copy(key)
   ): strptr
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
        prval () = fold@(args)
        in
            str_value
        end
      | ~list_vt_nil() => (free(key1);fold@(args);None_vt()))
   | ~None_vt() => None_vt() where {
     val () = free(key1)
     prval () = fold@(args)
   }): Option_vt(a)
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
   // always use the long name
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
   val opt = get_short_and_long(s.args_map, g1ofg0 key)
  prval() = fold@(sc)
  prval() = fold@(ar.subcommand)
   val key1 = (case+ opt of
   | ~Some_vt(p) => copy(p.0)
   | ~None_vt() => copy(key)
   ): strptr
   val value = $HT.hashtbl_takeout_opt(ar.captured_args, key1)
   val res = (case+ value of
   | ~Some_vt(list) => let
      val() = assertloc(list_vt_length(list) >= 0)
      val list_value = build_list<a>(list, list_vt_nil())
      val-~None_vt() = $HT.hashtbl_insert_opt(ar.captured_args, key1, list)
      prval () = fold@(args)
      in
          list_value
      end
   | ~None_vt() => list_vt_nil() where {
      val () = free(key1)
      prval () = fold@(args)
    }): List_vt(a)
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

implement{} print_help(args) = {
  val+ @ARGS(ar) = args
  val () = maybe_print(ar.prog_name)
  val () = maybe_print(ar.author)
  val () = maybe_print(ar.about)
  val () = maybe_print(ar.version)
  val () = println!()
  val () = println!("USAGE:")
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
  val cmd = s.command
  prval () = fold@(sc)
  prval () = fold@(ar.subcommand)
  val show_cmd = (if cmd = "" then "[CMD]" else cmd): string
  val () = println!("\t", get_prog_name ar, " ", show_cmd, " [FLAGS]")
  implement
  linmap_foreach$fwork<string, Arg><string>(k, it, e) = {
    val+ @A(x) = it
    val () = print!(e, "  ")
    val () = case x.short of
    | @Some_vt(s) => (print!("-", s, ", ");fold@(x.short))
    | @None_vt() => (fold@(x.short))
    val () = print!("--", x.name)
    val () = print!("\t", x.description)
    val () = case x.required of
    | true => print!("\t(required)")
    | false => print!()
    val () = case x.needs_value of
    | true => println!("\t(requires value)")
    | false => println!()
    prval() = fold@(it)
  }
  val () = println!("FLAGS:")
  // print the default flags
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
  var spacing: string = ""
  val () = linmap_foreach_env<string,Arg><string>(s.args_map, spacing)
  val cmd = s.command
  prval () = fold@(sc)
  prval () = fold@(ar.subcommand)
  val () = println!("  -h, --help\tThis help message")
  // print each subcommand
  val () = if cmd = "" then {
    val ls = linmap_listize(ar.command_map)
    var e = linmap_nil()
    val () = println!()
    val () = println!("SUBCOMMANDS:")
    fun loop(ls: List_vt(@(string, $SC.SubCommand)), cmds: &map(string, $SC.SubCommand) >> _): void = {
      val () = case+ ls of
      | ~list_vt_cons(x, xs) => {
        val+@$SC.SC(sc) = x.1
        val () = println!("  ", x.0, "\t", sc.description)
        var spacing: string = "  "
        val () = linmap_foreach_env<string,Arg><string>(sc.args_map, spacing)
        prval () = fold@(x.1)
        val () = linmap_insert_any(cmds, x.0, x.1)
        val () = loop(xs, cmds)
      }
      | ~list_vt_nil() => ()
    }
    val () = loop(ls, e)
    val () = ar.command_map := e
  }
  prval () = fold@(args)
}

vtypedef arg_pair = @(string, Option_vt(string))

// gets the list of all required arguments
fn{} get_all_required(args: !Args): List0_vt(arg_pair) = res where {
  val @ARGS(ar) = args
  var res: List0_vt(arg_pair) = list_vt_nil()
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
  val () = linmap_foreach_env<string, Arg><List0_vt(arg_pair)>(s.args_map, res) where {
    implement linmap_foreach$fwork<string, Arg><List0_vt(arg_pair)>(k, arg, list) = () where {
      val+ @A(a) = arg
      val () = if(a.required) then () where {
        val () = case+ a.short of
        | @Some_vt(s) => fold@(a.short) where {
          val () = list := list_vt_cons(@(a.name, Some_vt(s)), list)
        }
        | @None_vt() => fold@(a.short) where {
          val () = list := list_vt_cons(@(a.name, None_vt()), list)
        }
      }
      prval() = fold@(arg)
    }
  }
  prval() = fold@(sc)
  prval() = fold@(ar.subcommand)
  prval() = fold@(args)
}

fn{} has_arg(captured: !$HT.hashtbl(strptr, List_vt(strptr)), arg: !(string,Option_vt(string))): bool = res where {
  val key = copy(arg.0)
  val ref = $HT.hashtbl_search_ref(captured, key)
  val () = free(key)
  val res = if ref > 0 then true else res where {
    val () = if debug() then {
      val () = $HT.hashtbl_foreach(captured) where {
        implement $HT.hashtbl_foreach$fwork<strptr,List_vt(strptr)><void>(k,v,e) = {
          val () = println!(k, ": ", v)
        }
      }
    }
    val res = case+ arg.1 of
    | @None_vt() => false where {
      prval() = fold@(arg.1)
    }
    | @Some_vt(s) => res where {
      val key = copy(s)
      val ref = $HT.hashtbl_search_ref(captured, key)
      val () = free(key)
      val res = ref > 0
      prval() = fold@(arg.1)
    }
  }
}

// returns an error result if the catured args don't contain all the required args
fn{} has_required(args: !Args): arg_result = res where {
  val reqs = get_all_required(args)
  val @ARGS(ar) = args
  vtypedef env = @{ captured=$HT.hashtbl(strptr, List_vt(strptr)), res=arg_result }
  var envir: env = @{ captured=ar.captured_args, res=Ok(()) }
  val () = list_vt_foreach_env<(string,Option_vt(string))><env>(reqs, envir) where {
    implement list_vt_foreach$fwork<(string,Option_vt(string))><env>(arg, envir) = () where {
      val found = has_arg(envir.captured, arg)
      val r = (case+ envir.res of
              | ~Ok _ => 
                (case+ found of
                | true => Ok(())
                | false => Error(MissingRequired(list_vt_cons(string0_copy arg.0, list_vt_nil())))
                )
              | ~Error err => res where {
                  val res = (case+ err of
                  | ~PrintHelp() => Error(PrintHelp())
                  | ~Invalid() => Error(Invalid())
                  | ~MissingValues(v) => Error(MissingValues(v))
                  | ~MissingRequired m =>
                      (case+ found of
                      | true => Error(MissingRequired(m))
                      | false => Error(MissingRequired(list_vt_cons(string0_copy arg.0, m)))
                      ): arg_result): arg_result
              }): arg_result
      val () = envir.res := r
    }
  }
  val () = list_vt_freelin(reqs) where {
    implement list_vt_freelin$clear<arg_pair>(req) = {
      val () = case+ req.1 of
      | ~Some_vt _ => ()
      | ~None_vt() => ()
      prval() = $UNSAFE.castview2void(req)
    }
  }
  val () = ar.captured_args := envir.captured
  val res = envir.res
  prval() = fold@args
}

fun{} has_value{b:bool}(captured: !$HT.hashtbl(strptr, List_vt(strptr)), arg: string, alt: !option_vt(string,b)): bool = res where {
  val str = string0_copy arg
  val found = $HT.hashtbl_takeout_opt(captured, str)
  val res = (case+ found of
  | ~Some_vt(ls) => f where {
      val f = list_vt_length(ls) > 0
      val () = assertloc(strptr_isnot_null str)
      val cpy = copy str
      val-~None_vt() = $HT.hashtbl_insert_opt(captured, cpy, ls)
  }
  | ~None_vt() => f where {
      // check the short name...
      val f = case+ alt of
      | @None_vt() => false where {
          prval() = fold@(alt)
      }
      | @Some_vt(s) => res where {
        val none = None_vt()
        val res = has_value(captured, s, none)
        val-~None_vt() = none
        prval() = fold@(alt)
      }
      val () = assertloc(strptr_isnot_null str)
  }): bool
  val () = free(str)
}

fn{} validate_args_that_need_values(args: !Args): arg_result = res where {
  val @ARGS(ar) = args
  vtypedef env = @{ captured=$HT.hashtbl(strptr, List_vt(strptr)), res=arg_result }
  var envir: env = @{ captured=ar.captured_args, res=Ok(()) }
  val-@Some_vt(sc) = ar.subcommand
  val+@$SC.SC(s) = sc
  val () = linmap_foreach_env<string, Arg><env>(s.args_map, envir) where {
    implement linmap_foreach$fwork<string, Arg><env>(k, arg, envir) = () where {
      val+ @A(a) = arg
      val () = if a.needs_value && ~has_value(envir.captured, a.name, a.short) then () where {
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
  prval() = fold@(sc)
  prval() = fold@(ar.subcommand)
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
  val () = case+ ar.captured_prog_name of
  | ~Some_vt _ => ()
  | ~None_vt() => ()
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
| ~PrintHelp() => print!(args)
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

implement{} add_subcommand(args, subcommand) = {
  val+@ARGS(a) = args
  val+@$SC.SC(sc) = subcommand
  val key = sc.command
  prval() = fold@subcommand
  val () = linmap_insert_any(a.command_map, key, subcommand)
  prval() = fold@args
}

implement{a} get_command(args) = res where {
  val+@ARGS(a) = args
  val res = (case+ a.captured_command of
  | @None_vt() => None_vt() where {
    prval () = fold@(a.captured_command)
  }
  | @Some_vt(c) => res where {
    val res = command_from_string<a>(c)
    prval () = fold@(a.captured_command)
  }): Option_vt(a)
  prval() = fold@args
}

implement{} is_subcommand(args, cmd) = res where {
  val+@ARGS(a) = args
  val opt = linmap_takeout_opt(a.command_map, cmd)
  val res = case+ opt of
  | ~Some_vt(map) => true where {
    val-~None_vt() = linmap_insert_opt(a.command_map, cmd, map)
  }
  | ~None_vt() => false
  prval() = fold@args
}