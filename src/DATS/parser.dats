#include "./../HATS/includes.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload _ = "./../DATS/args.dats"
#define ATS_DYNLOADFLAG 0

staload $ARG
staload $ARGS

vtypedef argnext = @{
    arg = strptr,
    capturing = Option_vt(strptr),
    captured = $HT.hashtbl(strptr, List_vt(strptr)),
    help_found = bool
}

vtypedef state = @{
    args = map(string, Arg),
    capturing = Option_vt(strptr),
    captured = $HT.hashtbl(strptr, List_vt(strptr)),
    print_help_request = bool
}

implement list_vt_freelin$clear<strptr>(x) = free(x)
implement fprint_ref<strptr>(o, n) = print!(n)

fn{} has_dash{n:nat}(str: string(n)): bool = res where {
    val strlen = string1_length(str)
    val head = if strlen > 0 then string_get_at(str, 0) else ' '
    val res = case+ strlen of
              | n when n > 0 => eq_char0_char0(head, '-')
              | _ => false
}

fn{} free_capturing(opt: Option_vt(strptr)): void = 
case+ opt of
| ~Some_vt(s) => free(s)
| ~None_vt() => ()

fn {a:vt@ype} print_opt(out: FILEref, opt: !Option_vt(a)): void = 
case+ opt of
| @Some_vt(a) => () where {
    val () = print!("Some_vt(")
    val () = fprint_ref<a>(out, a)
    val () = print!(")")
    prval() = fold@(opt)
}
| @None_vt() => (print!("None_vt");fold@(opt))

fn{} print_opt_strptr(opt: &Option_vt(strptr)): void = print_opt<strptr>(stdout_ref, opt)

overload print with print_opt_strptr

implement linmap_foreach$fwork<string,List_vt(strptr)><void>(key,itm,env) = () where {
    val () = print!(key, ": ", itm)
}

fn{} get_list2(opt: Option_vt(List_vt(strptr))): List0_vt(strptr) =
case+ opt of
| ~Some_vt(ls) => res where {
    val () = assertloc(list_vt_length(ls) >= 0)
    val res = ls
}
| ~None_vt() => list_vt_nil()

implement linmap_foreach$fwork<string,Arg><argnext>(key,itm,env) = () where {
    val @A(a) = itm
    val () = assertloc(strptr_isnot_null env.arg)
    val str = $UNSAFE.strptr2string(env.arg)
    val arg1 = g1ofg0 str
    val strcpy = strptr0_copy(env.arg)
    val dashtype = get_dash_type(arg1)
    val arg = get_arg_name(arg1, dashtype)
    val () = assertloc(strptr_isnot_null arg)
    val arg_str = $UNSAFE.strptr2string(arg)
    val () = if arg_str = "h" || arg_str = "help" then env.help_found := true
    val () = if arg_str = a.name then () where {
        val () = free_capturing(env.capturing)
        val key1 = copy(arg)
        val () = env.capturing := Some_vt(copy(arg))
        val opt = $HT.hashtbl_takeout_opt<strptr,List_vt(strptr)>(env.captured, key1)
        val () = case+ opt of
        | ~Some_vt(ls) => () where {
            val-~None_vt() = $HT.hashtbl_insert_opt(env.captured, key1, ls)
        }
        | ~None_vt() => () where {
            val-~None_vt() = $HT.hashtbl_insert_opt(env.captured, key1, list_vt_nil())
        }
        val () = free(arg)
    } else free(arg)
    val () = free(strcpy)
    prval() = fold@(itm)
}

implement $LM.linmap_freelin$clear<strptr,List_vt(strptr)>(k,v) = () where {
    val () = strptr_free(k)
    val () = list_vt_freelin(v)
}

fn{} get_list(opt: Option_vt(List_vt(strptr)), arg: strptr): List1_vt(strptr) =
case+ opt of
| ~Some_vt(ls) => res where {
    val () = assertloc(list_vt_length(ls) >= 0)
    val res = list_vt_cons(arg, ls)
}
| ~None_vt() => list_vt_cons(arg, list_vt_nil())

fn{} process_arg{n:nat}(args: &state, arg: string(n)): bool = res where {
    val hasDash = has_dash(arg)
    var env: argnext = @{ arg=string0_copy(arg), capturing=args.capturing, captured=args.captured, help_found=false }
    val () = linmap_foreach_env<string,Arg><argnext>(args.args, env)
    val () = case+ env.capturing of
    | @Some_vt(a) when ~has_dash(arg) => () where {
        val () = assertloc(strptr_isnot_null(a))
        val key1 = copy(a)
        val cpy = string0_copy(arg)
        val opt = $HT.hashtbl_takeout_opt<strptr,List_vt(strptr)>(env.captured, key1)
        val () = fold@(env.capturing)
        val () = case+ opt of
        | ~Some_vt(ls) => () where {
            val () = assertloc(list_vt_length(ls) >= 0)
            val-~None_vt() = $HT.hashtbl_insert_opt(env.captured, key1, list_vt_cons(cpy, ls))
        }
        | ~None_vt() => () where {
            val-~None_vt() = $HT.hashtbl_insert_opt(env.captured, key1, list_vt_cons(cpy, list_vt_nil()))
        }
    }
    | @Some_vt(_) => fold@(env.capturing)
    | @None_vt() => fold@(env.capturing)
    val () = args.captured := env.captured
    val () = args.capturing := env.capturing
    val () = args.print_help_request := env.help_found
    val () = free(env.arg)
    val res = true
}

implement{} gatherLoop(acc, argc, argv, cur) = () where {
    val arg = g1ofg0 argv[cur]
    val () = acc := list_vt_cons(arg, acc)
    val () = if cur < argc-1 then gatherLoop(acc, argc, argv, cur+1) else ()
}

implement{} gatherArgsIntoList(argc, argv) = res where {
    var r = list_vt_nil()
    val () = gatherLoop(r, argc, argv, 1)
    val res = list_vt_reverse(r)
}

vtypedef envi = @{
    args=Args
}

implement list_vt_foreach$fwork<[n:nat] string(n)><state>(itm, env) = () where {
    val _ = process_arg(env, itm)
}

implement{} parse_args(args, argc, argv) = res where {
    val+@ARGS(ar) = args
    val arg_list = gatherArgsIntoList(argc, argv)
    var st: state = @{ args = ar.args_map, capturing = None_vt(), captured=ar.captured_args, print_help_request=false }
    val () = list_vt_foreach_env<[n:nat] string(n)><state>(arg_list, st)
    val () = list_vt_free(arg_list)
    val () = free_capturing(st.capturing)
    val () = ar.captured_args := st.captured
    val () = ar.args_map := st.args
    prval() = fold@args
    val res = (if st.print_help_request then Error(PrintHelp) else Ok(())): result_vt((), ArgError)
}