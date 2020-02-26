#include "./../HATS/includes.hats"
staload "./../SATS/result.sats"
staload "./../SATS/lib.sats"
staload "./../SATS/arg.sats"
staload _ = "./../DATS/result.dats"
#define ATS_DYNLOADFLAG 0

extern fun{a:vt@ype} fprint_arg(out: FILEref, x: !a): void

implement {a} fprint_arg(out, x) = ()

overload fprint with fprint_arg

assume Args_vtype = Args
implement new_args(prog_name) = args where {
  val args = ARGS(_)
  val ARGS(x) = args 
  val () = x.args_map := linmap_nil()
  val () = x.prog_name := prog_name
  val () = x.about := ""
  val () = x.author := ""
  val () = x.version := ""
  val () = x.captured_args := hashtbl_make_nil<string, Strptr1>(i2sz 20)
  val () = x.has_all_required := true
  prval () = fold@(args)
}

implement set_author(args, author) = () where {
  val @ARGS(x) = args 
  val () = x.author := author
  prval() = fold@(args)
}

implement set_about(args, about) = () where {
  val @ARGS(x) = args 
  val () = x.about := about
  prval() = fold@(args)
}

implement set_version(args, version) = () where {
  val @ARGS(x) = args 
  val () = x.version := version
  prval() = fold@(args)
}

implement add_arg(args, arg) = () where {
  val @ARGS(x) = args 
  val @A(y) = arg
  val name = y.name
  prval() = fold@(arg)
  val () = linmap_insert_any(x.args_map, name, arg)
  prval () = fold@(args)
}

vtypedef sa = @(Strptr1, Strptr1)

fn print_args(args: !Args): void = () where {
    implement(env)
    hashtbl_foreach$fwork<string, Strptr1><env>(k, it, e) = println!(k, "->", it)
    val @ARGS(x) = args
    val () = hashtbl_foreach(x.captured_args)
    val () = fold@(args)
}

implement linmap_freelin$clear<Arg>(x) =
case x of
| ~A(a) => () where {
   val () = case a.short of
   | ~Some_vt(_) => ()
   | ~None_vt() => ()
}

implement free_args(args) = () where {
  val ~ARGS(x) = args
  implement list_vt_freelin$clear<sa>(x) = let
        val() = strptr_free(x.0)
    in
        strptr_free(x.1)
    end
  val ls = hashtbl_listize<string, Strptr1>(x.captured_args)
  val () = list_vt_freelin($UNSAFE.castvwtp0{List0_vt(sa)}(ls))
  val () = linmap_freelin(x.args_map)
}

implement {a} get_value(args, key) = res where {
   val+ @ARGS(ar) = args
   val ref = hashtbl_search_ref(ar.captured_args, key)
   val res = (if(ref > 0) then let
       val ref3 = $UNSAFE.cptr_get<Strptr1>(ref)
       val res2 = copy(ref3)
       prval () = $UNSAFE.cast2void(ref3)
       val str = $UNSAFE.castvwtp0{string}(res2)
       val x = string_to_value<a>(str)
       val res4 = $UNSAFE.castvwtp0{Strptr1}(str)
       val () = free(res4)
       in
          x
       end
       else
           None_vt()): Option_vt(a)
   prval () = fold@(args)
}

implement print_help(args) = () where {
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

datavtype dash_type =
| Single of ()
| Double of ()
| None of ()

fn has_double_dash{n:int}(arg: string(n)): dash_type = let
  val () = assertloc(string_length(arg) > 0)
  val dash1 = eq_char0_char0(string_get_at(arg, 0), '-')
  val dash2 = (if string_length(arg) > 1 then eq_char0_char0(string_get_at(arg, 1), '-') else false): bool
in
  case dash1 of
  | true =>
      (case dash2 of
      | true => Double()
      | false => Single())
  | false => None()
end

fn get_arg_name{n,m:int | n > 2; m > n }(ei: ssize_t(n), arg1: string(m), dashtype: dash_type): [n:int] strnptr(n) =
  (case dashtype of
  | ~Single() => res where {
      val start = i2sz(1)
      val len = (g1i2u ei) - 1
      val () = assertloc(string1_length(arg1) > 1)
      val res = string_make_substring(arg1, start, len)
  }
  | ~Double() => res where {
      val start = i2sz(2)
      val len = (g1i2u ei) - 2
      val () = assertloc(string1_length(arg1) > 2)
      val () = assertloc(string1_length(arg1) >= (start+len))
      val res = string_make_substring(arg1, start, len)
  }
  | ~None() => res where {
      val len = (g1i2u ei) - 2
      val res = string_make_substring(arg1, i2sz(0), len)
  }): [n:int] strnptr(n)

fn get_arg_value{n,m:int | n > 2; m > n }(ei: ssize_t(n), arg1: string(m), dashtype: dash_type): [n:int] strnptr(n) =
  (case dashtype of
  | ~Single() => res where {
      val start = i2sz(3)
      val len = (g1i2u ei) - 1
      val () = assertloc(string1_length(arg1) > 3)
      val () = assertloc(string1_length(arg1) >= len+start)
      val res = string_make_substring(arg1, start, len)
  }
  | ~Double() => res where {
      val start = i2sz(4)
      val len = (g1i2u ei) - 2
      val () = assertloc(string1_length(arg1) > 4)
      val () = assertloc(string1_length(arg1) >= (start+len))
      val res = string_make_substring(arg1, start, len)
  }
  | ~None() => res where {
      val len = (g1i2u ei) - 2
      val res = string_make_substring(arg1, i2sz(1), len)
  }): [n:int] strnptr(n)

fn process_arg(args: !Args, arg: string, prev: string): bool = res where {
  val+ @ARGS(ar) = args
  val arg1 = g1ofg0 arg
  val ei = string_index(arg1, '=')
  val prev1 = g1ofg0 prev
  val () = assertloc(string_length(prev1) > 0)
  val dash1 = eq_char0_char0(string_get_at(prev1, 0), '-')
  val dash2 = (if string_length(prev1) > 1 then eq_char0_char0(string_get_at(prev1, 1), '-') else false): bool
  val res = if ei >= 0 then res where {
      val () = assertloc(ei > 2)
      val len = (g1i2u ei) - 2
      val dashtype = has_double_dash(arg1)
      val name = get_arg_name(ei, arg1, dashtype)
      val dashtype = has_double_dash(arg1)
      val value = get_arg_value(ei, arg1, dashtype)
      val value0 = strnptr2strptr value
      val () = assertloc(strptr_isnot_null value0)
      val () = assertloc(strnptr_length(name) > 0)
      val name0 = strnptr2string name
      val () = hashtbl_insert_any(ar.captured_args, name0, value0)
      val res = true
  } else if (dash1 && dash2) then res where {
    val prev1 = g1ofg0 prev
    val strlen = string1_length(prev1)
    val () = assertloc(strlen > 2)
    val str = string_make_substring(prev1, i2sz 2, strlen - 2)
    val str0 = strnptr2string str
    val () = println!(str0)
    val () = hashtbl_insert_any(ar.captured_args, str0, string0_copy arg)
    val res = true
  } else if (dash1 && not dash2) then res where {
    val prev1 = g1ofg0 prev
    val strlen = string1_length(prev1)
    val () = assertloc(strlen > 1)
    val str = string_make_substring(prev1, i2sz 1, strlen - 1)
    val () = hashtbl_insert_any(ar.captured_args, strnptr2string str, string0_copy arg)
    val res = true
  } else true
  prval () = fold@(args)
}

fun do_parse{n:int | n > 1}{m:nat | m < n && m > 0} .<n-m>. (args: !Args, argc: int(n), argv: !argv(n), cur: int(m)): result_vt((), ArgError) = res where {
  val arg = argv[cur]
  val prev = argv[cur-1]
  val continue = (case arg of
           | "-h" => (print_help(args); Error(PrintHelp()))
           | "--help" => (print_help(args); Error(PrintHelp()))
           | _ => res where {
              val res = process_arg(args, arg, prev)
              val res = (if res then Ok(()) else Error(Invalid()) ): result_vt((), ArgError)
           }): result_vt((), ArgError)
  val res = (case+ continue of
            | ~Ok _ => (if cur < argc-1 then do_parse(args, argc, argv, cur+1) else Ok(())): result_vt((), ArgError)
            | ~Error err => Error(err)): result_vt((), ArgError)
}

fn get_all_required(args: !Args): List0_vt(string) = res where {
  val @ARGS(ar) = args
  implement
  linmap_foreach$fwork<string, Arg><List0_vt(string)>(k, it, e) = () where {
    val+ @A(x) = it
    val () = if(x.required) then () where {
      val () = e := list_vt_cons(x.name, e)
    }
    prval() = fold@(it)
  }
  var res: List0_vt(string) = list_vt_nil()
  val () = linmap_foreach_env<string, Arg><List0_vt(string)>(ar.args_map, res)
  prval() = fold@(args)
}

fn has_required(args: !Args): result_vt((), ArgError) = res where {
  val reqs = get_all_required(args)
  fun loop(reqs: !List0_vt(string), args: !Args, res: &result_vt((), ArgError)): void = () where {
      val () = case+ reqs of
               | list_vt_nil() => ()
               | list_vt_cons(x, xs) => () where {
                    val+ @ARGS(ar) = args
                    val ref = hashtbl_search_ref(ar.captured_args, x)
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
                    val () = res := res2//((if res2 then Ok(()) else Error(MissingRequired(x))): result_vt((), ArgError))
                    // val () = fprint_result(stdout_ref, res)
                    // val () = println!("res2: ", res2)
                    prval() = fold@(args)
                    val () = loop(xs, args, res)
                }
  }
  var res = Ok(())
  val () = loop(reqs, args, res)
  val () = free(reqs)
  // val res = (if res then Ok(res) else Error(MissingRequired("MISSING"))): result_vt(bool, ArgError)
}

implement parse(args, argc, argv) = res where {
  val () = println!("parsing...")
  val res = (case- argc of
           | 1 => (print_help(args); Error(PrintHelp))
           | _ when argc > 1 => res where {
              val res = do_parse(args, argc, argv, 1)
              val res = (case+ res of
                       | ~Ok(r) => has_required(args)
                       | ~Error(r) => Error(r)): result_vt((), ArgError)
           }): result_vt((), ArgError)
}

implement string_to_value<int>(v) = let
  val tmp = g0string2int(v)
in
  // if we get back a 0 and the string was not "0", then the string is not a number
  case+ v of
  | _ when tmp = 0 && v != "0" => None_vt()
  | _ => Some_vt(tmp)
end

implement string_to_value<bool>(v) = 
case+ v of
| "true" => Some_vt(true)
| "True" => Some_vt(true) 
| "t" => Some_vt(true)
| "false" => Some_vt(false)
| "False" => Some_vt(false) 
| "f" => Some_vt(false)
| _ => None_vt()
