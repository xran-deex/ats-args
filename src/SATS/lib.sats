#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/SATS/hashtbl_chain.sats"
staload "libats/SATS/linmap_list.sats"
staload "./../SATS/result.sats"
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
        captured_args=hashtbl(string, Strptr1),
        has_all_required=bool
    }
datavtype Args =
| ARGS of args_struct

datavtype ArgError =
| PrintHelp of ()
| Invalid of ()
| MissingRequired of strptr

fn new_args(prog_name: string): Args

fn set_author(args: !Args, author: string): void

fn set_about(args: !Args, about: string): void

fn set_version(args: !Args, version: string): void

fn add_arg(args: !Args >> _, arg: Arg): void

fn free_args(args: Args): void

fn {a:t@ype} get_value(args: !Args, key: string): Option_vt(a)

fn {a:t@ype} string_to_value(value: string): Option_vt(a)

fn print_help(args: !Args): void

fn fprint_args(out: FILEref, args: !Args): void

fn parse{n:int | n > 0}(args: !Args, argc: int(n), argv: !argv(n)): result_vt((), ArgError)

fun{a:vt@ype} fprint_arg(out: FILEref, x: !a): void

overload fprint with fprint_arg
overload print with print_help