#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0
implement hello(w) = string_append("Hello ", w)

typedef arg_struct(a:t@ype) = @{ idx=a, name=string }
datavtype Arg(a:t@ype) =
| A of arg_struct(a) 
| B of (a, string)

absvtype Args_vtype
vtypedef Args = Args_vtype

vtypedef args_struct(a:t@ype) = 
    @{ 
        args=List0_vt(Arg(a)), 
        prog_name=string,
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
  val () = x.args := list_vt_nil{Arg(a)}()
  val () = x.prog_name := prog_name
  val () = x.captured_args := hashtbl_make_nil<string, Strptr1>(i2sz 20)
  prval () = fold@(args)
}

extern fn add_arg{a:t@ype}(args: !Args(a) >> _, arg: Arg(a)): void
implement add_arg{a:t@ype}(args, arg) = () where {
  val @ARGS(x) = args 
  val () = x.args := list_vt_cons{Arg(a)}(arg, x.args)
  prval () = fold@(args)
}

vtypedef sa = @(Strptr1, Strptr1)

fn print_args{a:t@ype}(args: !Args(a)): void = () where {
    implement(env)
    hashtbl_foreach$fwork<string, Strptr1><env>(k, it, e) = println!(k, "->", it)
    val @ARGS(x) = args
    val () = hashtbl_foreach(x.captured_args)
    val () = fold@(args)
}

extern fn free_args{a:t@ype}(args: Args(a)): void
implement free_args{a:t@ype}(args) = () where {
  val () = print_args(args)
  val ~ARGS(x) = args
  implement list_vt_freelin$clear<Arg(a)>(x) = () where {
    val () = case x of
             | ~A(_) => ()
             | ~B(_, _) => ()
  }
  implement list_vt_freelin$clear<sa>(x) = let
        val() = strptr_free(x.0)
    in
        strptr_free(x.1)
    end
  val ls = hashtbl_listize<string, Strptr1>(x.captured_args)
  val () = list_vt_freelin($UNSAFE.castvwtp0{List0_vt(sa)}(ls))
  val () = list_vt_freelin(x.args)
}

extern fn {a:t@ype}print_help(args: !Args(a)): void

implement {a} print_help(args) = () where {
  val+ @ARGS(ar) = args
  val () = println!("===", ar.prog_name, "===")
  prval () = fold@(args)
}

typedef parser_state = @{
  pos= int
}

fn add_arg{a:t@ype}(args: !Args(a), arg: string): void = () where {
  val+ @ARGS(ar) = args
  val arg1 = g1ofg0 arg
  val ei = string_index(arg1, '=')
  val () = if ei >= 0 then () where {
      val len = g1i2u ei
      val start = i2sz(0)
      val str = string_make_substring(arg1, start, len)
      val strlen = string1_length(arg1)
      val len2 = len+1
      val en = strlen - len2
      val () = println!(str)
      val str2 = string_make_substring(arg1, len2, en)
      (* val () = println!(str) *)
      val str3 = strnptr2strptr str2
      val () = assertloc(strptr_isnot_null str3)
      val () = hashtbl_insert_any(ar.captured_args, strnptr2string str, str3)
      (* val () = free(str) *)
  }
  prval () = fold@(args)
}

fun do_parse{a:t@ype}{n:int | n > 1}{m:nat | m < n} .<n-m>. (args: !Args(a), argc: int(n), argv: !argv(n), cur: int(m)): void = () where {
  val state: parser_state = @{ pos = 0 }
  val arg = argv[cur]
  val continue = case arg of
           | "-h" => (print_help(args); false)
           | "--help" => (print_help(args); false)
           | _ => (println!(arg); add_arg(args, arg); true)
  val () = if cur < argc-1 then if continue then do_parse(args, argc, argv, cur+1) else () else ()
}

extern fn parse{a:t@ype}{n:int | n > 0}(args: !Args(a), argc: int(n), argv: !argv(n)): void

implement parse(args, argc, argv) = () where {
  val () = println!("parsing...")
  val () = case- argc of
           | 1 => print_help(args)
           | _ when argc > 1 => do_parse(args, argc, argv, 1)
}
