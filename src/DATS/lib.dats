#include "./../HATS/includes.hats"
staload "./../DATS/arg.dats"
#define ATS_DYNLOADFLAG 0

absvtype Args_vtype
vtypedef Args = Args_vtype

vtypedef args_struct(a:t@ype) = 
    @{ 
        args_map=map(string, Arg(a)),
        prog_name=string,
        author=string,
        about=string,
        captured_args=hashtbl(string, Strptr1)
    }
datavtype Args(a:t@ype) =
| ARGS of args_struct(a)

extern fun{a:vt@ype} fprint_arg(out: FILEref, x: !a): void

implement {a} fprint_arg(out, x) = ()

overload fprint with fprint_arg

assume Args_vtype = Args
extern fn new_args{a:t@ype}(prog_name: string): Args(a)
implement new_args{a:t@ype}(prog_name) = args where {
  val args = ARGS(_)
  val ARGS(x) = args 
  val () = x.args_map := linmap_nil()
  val () = x.prog_name := prog_name
  val () = x.about := ""
  val () = x.author := ""
  val () = x.captured_args := hashtbl_make_nil<string, Strptr1>(i2sz 20)
  prval () = fold@(args)
}

extern fn {a:t@ype} set_author(args: !Args(a), author: string): void
implement {a} set_author(args, author) = () where {
  val @ARGS(x) = args 
  val () = x.author := author
  prval() = fold@(args)
}

extern fn {a:t@ype} set_about(args: !Args(a), about: string): void
implement {a} set_about(args, about) = () where {
  val @ARGS(x) = args 
  val () = x.about := about
  prval() = fold@(args)
}

extern fn {a:t@ype} add_arg(args: !Args(a) >> _, arg: Arg(a)): void
implement {a} add_arg(args, arg) = () where {
  val @ARGS(x) = args 
  val @A(y) = arg
  val name = y.name
  prval() = fold@(arg)
  val () = linmap_insert_any(x.args_map, name, arg)
  prval () = fold@(args)
}

vtypedef sa = @(Strptr1, Strptr1)

fn {a:t@ype} print_args(args: !Args(a)): void = () where {
    implement(env)
    hashtbl_foreach$fwork<string, Strptr1><env>(k, it, e) = println!(k, "->", it)
    val @ARGS(x) = args
    val () = hashtbl_foreach(x.captured_args)
    val () = fold@(args)
}

extern fn{a:t@ype} free_args(args: Args(a)): void
implement {a} free_args(args) = () where {
  val () = print_args<a>(args)
  val ~ARGS(x) = args
  implement list_vt_freelin$clear<sa>(x) = let
        val() = strptr_free(x.0)
    in
        strptr_free(x.1)
    end
  val ls = hashtbl_listize<string, Strptr1>(x.captured_args)
  val () = list_vt_freelin($UNSAFE.castvwtp0{List0_vt(sa)}(ls))
  implement linmap_freelin$clear<Arg(a)>(x) =
             case x of
             | ~A(a) => () where {
                 val () = case a.action of
                 | ~Some_vt(y) => cloptr_free($UNSAFE.castvwtp0(y))
                 | ~None_vt() => ()
                 val () = case a.short of
                 | ~Some_vt(_) => ()
                 | ~None_vt() => ()
             }
  val () = linmap_freelin(x.args_map)
}

extern fn {a:t@ype} get_value(args: !Args(a), key: string): Option_vt(int)

implement {a} get_value(args, key) = res where {
   val+ @ARGS(ar) = args
   val ref = hashtbl_search_ref(ar.captured_args, key)
   val res = (if(ref > 0) then let
       val ref3 = $UNSAFE.cptr_get<Strptr1>(ref)
       val res2 = copy(ref3)
       prval () = $UNSAFE.cast2void(ref3)
       (* val res4 = strptr2string res2 *)
       val str = $UNSAFE.castvwtp0{string}(res2)
       val i = g0string2int(str)
       val res4 = $UNSAFE.castvwtp0{Strptr1}(str)
       val () = free(res4)
       in
         Some_vt(i)
       end
       else
           None_vt()): Option_vt(int)
   prval () = fold@(args)
}

extern fn {a:t@ype} print_help(args: !Args(a)): void

implement {a} print_help(args) = () where {
  val+ @ARGS(ar) = args
  val () = println!("===", ar.prog_name, "===")
  val () = println!(ar.prog_name)
  val () = println!(ar.author)
  val () = println!(ar.about)
  val () = println!()
  val () = println!("USAGE:")
  val () = println!("\t", ar.prog_name, " [FLAGS]")
  implement (env)
  linmap_foreach$fwork<string, Arg(a)><env>(k, it, e) = () where {
    val+ @A(x) = it
    val () = print!("  ")
    val () = case x.short of
    | @Some_vt(s) => (print!("-", s, ", ");fold@(x.short))
    | @None_vt() => (fold@(x.short))
    val () = print!("--", x.name)
    val () = println!("\t", x.description)
    prval() = fold@(it)
  }
  val () = println!("FLAGS:")
  val () = linmap_foreach(ar.args_map)
  prval () = fold@(args)
}

fn {a:t@ype} process_arg(args: !Args(a), arg: string, prev: string): void = () where {
  val+ @ARGS(ar) = args
  val arg1 = g1ofg0 arg
  val ei = string_index(arg1, '=')
  val prev1 = g1ofg0 prev
  val () = assertloc(string_length(prev1) > 1)
  val dash1 = eq_char0_char0(string_get_at(prev1, 0), '-')
  val dash2 = eq_char0_char0(string_get_at(prev1, 1), '-')
  val () = if ei >= 0 then () where {
      val len = g1i2u ei
      val start = i2sz(0)
      val str = string_make_substring(arg1, start, len)
      val strlen = string1_length(arg1)
      val len2 = len+1
      val en = strlen - len2
      val () = println!(str)
      val str2 = string_make_substring(arg1, len2, en)
      val str3 = strnptr2strptr str2
      val () = assertloc(strptr_isnot_null str3)
      val () = hashtbl_insert_any(ar.captured_args, strnptr2string str, str3)
  } else if (dash1 && dash2) then () where {
    val prev1 = g1ofg0 prev
    val strlen = string1_length(prev1)
    val () = assertloc(strlen > 2)
    val str = string_make_substring(prev1, i2sz 2, strlen - 2)
    val str0 = strnptr2string str
    val () = hashtbl_insert_any(ar.captured_args, str0, string0_copy arg)
    val some_arg = linmap_takeout_opt(ar.args_map, str0)
    val () = case some_arg of
    | ~Some_vt(arg1) => () where {
        val+ @A(w) = arg1
        val () = case w.action of
                 | @Some_vt(f) => (f(arg); fold@(w.action))
                 | @None_vt() => fold@(w.action)
        prval () = fold@(arg1)
        val () = linmap_insert_any(ar.args_map, str0, arg1)
    }
    | ~None_vt() => ()
  } else if (dash1 && not dash2) then () where {
    val prev1 = g1ofg0 prev
    val strlen = string1_length(prev1)
    val () = assertloc(strlen > 1)
    val str = string_make_substring(prev1, i2sz 1, strlen - 1)
    val () = hashtbl_insert_any(ar.captured_args, strnptr2string str, string0_copy arg)
  }
  prval () = fold@(args)
}

fun {a:t@ype} do_parse{n:int | n > 1}{m:nat | m < n && m > 0} .<n-m>. (args: !Args(a), argc: int(n), argv: !argv(n), cur: int(m)): void = () where {
  val arg = argv[cur]
  val prev = argv[cur-1]
  val continue = case arg of
           | "-h" => (print_help(args); false)
           | "--help" => (print_help(args); false)
           | _ => (process_arg(args, arg, prev); true)
  val () = if cur < argc-1 then if continue then do_parse(args, argc, argv, cur+1)
}

extern fn {a:t@ype} parse{n:int | n > 0}(args: !Args(a), argc: int(n), argv: !argv(n)): void

implement {a} parse(args, argc, argv) = () where {
  val () = println!("parsing...")
  val () = case- argc of
           | 1 => print_help(args)
           | _ when argc > 1 => do_parse(args, argc, argv, 1)
}
