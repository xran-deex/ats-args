#include "./../HATS/includes.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload _ = "./../DATS/args.dats"
#define ATS_DYNLOADFLAG 0

staload $ARG
staload $ARGS

vtypedef arg_result = result_vt((), ArgError)

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
    print_help_request = bool,
    pos = int,
    captured_command = Option_vt(strptr)
}

implement list_vt_freelin$clear<strptr>(x) = free(x)
implement fprint_ref<strptr>(o, n) = print!(n)

fn has_dash(str: string): bool = res where {
    val str = g1ofg0 str
    val strlen = string1_length(str)
    val head = if strlen > 0 then string_get_at(str, 0) else ' '
    val res = case+ strlen of
              | n when n > 0 => eq_char0_char0(head, '-')
              | _ => false
}

fn free_capturing(opt: Option_vt(strptr)): void = 
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

fn print_opt_strptr(opt: &Option_vt(strptr)): void = print_opt<strptr>(stdout_ref, opt)

overload print with print_opt_strptr

implement linmap_foreach$fwork<string,List_vt(strptr)><void>(key,itm,env) = () where {
    val () = print!(key, ": ", itm)
}

fn get_list2(opt: Option_vt(List_vt(strptr))): List0_vt(strptr) =
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
    val arg_str_short = (case+ a.short of
    | @Some_vt(s) => res where {
        val res = s
        prval () = fold@(a.short) 
    }
    | @None_vt() => "" where {
        prval () = fold@(a.short) 
    }): string
    val () = if arg_str = "h" || arg_str = "help" then env.help_found := true
    val () = if arg_str = a.name || arg_str = arg_str_short then () where {
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
    } else () where {
        val () = free(arg)
    }
    val () = free(strcpy)
    prval() = fold@(itm)
}

implement $LM.linmap_freelin$clear<strptr,List_vt(strptr)>(k,v) = () where {
    val () = strptr_free(k)
    val () = list_vt_freelin(v)
}

fn get_list(opt: Option_vt(List_vt(strptr)), arg: strptr): List1_vt(strptr) =
case+ opt of
| ~Some_vt(ls) => res where {
    val () = assertloc(list_vt_length(ls) >= 0)
    val res = list_vt_cons(arg, ls)
}
| ~None_vt() => list_vt_cons(arg, list_vt_nil())

fn handle_captured_position(captured: !$HT.hashtbl(strptr, List_vt(strptr)), opt: Option_vt(string), arg: string): void = {
    val () = case+ opt of
    | ~None_vt() => ()
    | ~Some_vt(s) when ~has_dash(arg) => {
        val key1 = copy(s)
        val cpy = string0_copy(arg)
        val opt = $HT.hashtbl_takeout_opt<strptr,List_vt(strptr)>(captured, key1)
        val () = case+ opt of
        | ~Some_vt(ls) => () where {
            val () = assertloc(list_vt_length(ls) >= 0)
            val-~None_vt() = $HT.hashtbl_insert_opt(captured, key1, list_vt_cons(cpy, ls))
        }
        | ~None_vt() => () where {
            val-~None_vt() = $HT.hashtbl_insert_opt(captured, key1, list_vt_cons(cpy, list_vt_nil()))
        }
    }
    | ~Some_vt _ => ()
}

fn process_arg(args: &state, arg: string): void = () where {
    val hasDash = has_dash(arg)
    val opt = get_arg_for_position(args.args, args.pos, args.captured_command)
    val () = handle_captured_position(args.captured, opt, arg)
    val () = args.pos := args.pos + 1
    var env: argnext = @{ arg=string0_copy(arg), capturing=args.capturing, captured=args.captured, help_found=false }
    val () = linmap_foreach_env<string,Arg><argnext>(args.args, env)
    val () = args.captured := env.captured
    val () = args.capturing := env.capturing
    val () = args.print_help_request := env.help_found
    val dashtype = get_dash_type(arg)
    val arg1 = get_arg_name(arg, dashtype)
    val clear = (case+ args.capturing of
    | @Some_vt(a) when ~hasDash => false where {
        val () = assertloc(strptr_isnot_null(a))
        val () = assertloc(strptr_isnot_null(arg1))
        val opt = get_short_and_long(args.args, $UNSAFE.strptr2string(a))
        val key1 = (case+ opt of
        | ~Some_vt(p) => copy(p.0)
        | ~None_vt() => copy(a)
        ): strptr
        prval () = fold@(args.capturing)
        val cpy = string0_copy(arg)
        val opt = $HT.hashtbl_takeout_opt<strptr,List_vt(strptr)>(args.captured, key1)
        val () = case+ opt of
        | ~Some_vt(ls) => () where {
            val () = assertloc(list_vt_length(ls) >= 0)
            val-~None_vt() = $HT.hashtbl_insert_opt(args.captured, key1, list_vt_cons(cpy, ls))
        }
        | ~None_vt() => () where {
            val-~None_vt() = $HT.hashtbl_insert_opt(args.captured, key1, list_vt_cons(cpy, list_vt_nil()))
        }
    }
    | @Some_vt(a) when hasDash => res where {
        val () = assertloc(strptr_isnot_null(a))
        val () = assertloc(strptr_isnot_null(arg1))
        val opt = get_short_and_long(args.args, $UNSAFE.strptr2string(arg1))
        val res = (case+ opt of
        | ~Some_vt(p) => res where {
            val res = if p.0 != $UNSAFE.strptr2string(a) && p.1 != $UNSAFE.strptr2string(a) then true else false
        }
        | ~None_vt() => false
        ): bool
        prval() = fold@(args.capturing)
    }
    | @Some_vt(_) => false where {
        prval() = fold@(args.capturing)
    }
    | @None_vt() => false where {
        prval() = fold@(args.capturing)
    }): bool
    val () = free(arg1)
    val () = if clear then {
        val () = free_capturing(args.capturing)
        val () = args.capturing := None_vt()
    }
    val () = free(env.arg)
}

fn help_or_ok(printHelp: bool): arg_result =
case+ printHelp of
| true => Error(PrintHelp)
| false => Ok(())

vtypedef envi = @{
    args=Args
}

implement list_vt_foreach$fwork<string><state>(itm, env) = process_arg(env, itm)

implement parse_args(args, argc, argv) = res where {
    val+@ARGS(ar) = args
    val ~list_vt_cons(prog, arg_list) = listize_argc_argv(argc, argv)
    var key: string
    var print_help_request: bool = false
    // subcommand handling - TODO - clean this up
    var subc = (if list_vt_length(arg_list) > 0 then map where {
        val () = key := arg_list[0]
        val () = if key = "-h" || key = "--help" then print_help_request := true
        val () = if has_dash(key) then key := ""
        val map = (case+ linmap_takeout_opt(ar.command_map, key) of
        | ~Some_vt(map) => map where {
            val () = case+ ar.captured_command of
            | ~Some_vt s => free(s)
            | ~None_vt() => ()
            val () = ar.captured_command := Some_vt(copy(key))
        }
        | ~None_vt() => map where {
            val-~Some_vt(map) = linmap_takeout_opt(ar.command_map, "")
        }): $SC.SubCommand
    } else map where {
        val () = key := ""
        val-~Some_vt(map) = linmap_takeout_opt(ar.command_map, key)
    }): $SC.SubCommand
    val () = case+ ar.subcommand of
    | ~Some_vt sc => $SC.free_subcommand(sc)
    | ~None_vt() => ()
    val () = ar.subcommand := Some_vt(subc)
    val-@Some_vt(sc) = ar.subcommand
    val+@$SC.SC(s) = sc
    var st: state = @{ args = s.args_map, capturing = None_vt(), captured=ar.captured_args, print_help_request=false, pos = 1, captured_command=ar.captured_command }
    val () = list_vt_foreach_env<string><state>(arg_list, st)
    val () = list_vt_free(arg_list)
    val () = free_capturing(st.capturing)
    val () = ar.captured_args := st.captured
    val () = s.args_map := st.args
    val () = ar.captured_command := st.captured_command
    prval () = fold@sc
    prval () = fold@(ar.subcommand)
    prval() = fold@args
    val res = help_or_ok(print_help_request || st.print_help_request)
}