#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/SATS/linmap_list.sats"
#include "hashtable-vt/hashtable_vt.hats"
staload "ats-result/src/SATS/result.sats"
staload "./../SATS/arg.sats"

absvtype Args_vtype
vtypedef Args = Args_vtype

vtypedef args_struct =
    @{ 
        args_map=map(string, Arg),
        prog_name=string,
        author=string,
        about=string,
        version=string,
        captured_args=$HT.hashtbl(strptr, List_vt(strptr)),
        has_all_required=bool
    }
datavtype Args =
| ARGS of args_struct

datavtype ArgError =
| PrintHelp of ()
| Invalid of ()
| MissingRequired of List1_vt(strptr)
| MissingValues of List1_vt(strptr)

fn{} new_args(prog_name: string): Args

fn{} set_author(args: !Args, author: string): void

fn{} set_about(args: !Args, about: string): void

fn{} set_version(args: !Args, version: string): void

fn{} add_arg{a:type}(args: !Args >> _, arg: Arg): void

fn{} free_args(args: Args): void

fn {a:vt@ype} get_value(args: !Args, key: string): Option_vt(a)

fn {a:vt@ype} string_to_value{l:addr | l > null}(value: !strptr(l)): Option_vt(a)

fn{} print_help(args: !Args): void

fn{} fprint_args(out: FILEref, args: !Args): void

fn{} parse{n:int | n > 0}(args: !Args, argc: int(n), argv: !argv(n)): result_vt((), ArgError)

fn{a:vt@ype} fprint_arg(out: FILEref, x: !a): void

overload fprint with fprint_arg
overload print with print_help

fn {a:vt@ype} get_parsed(args: !Args): a 

fn{} handle_error(args: !Args, err: ArgError): void