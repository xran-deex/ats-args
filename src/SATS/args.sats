#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/SATS/linmap_list.sats"
#include "hashtable-vt/hashtable_vt.hats"
staload "ats-result/src/SATS/result.sats"
staload "./../SATS/arg.sats"
staload "./../SATS/subcommand.sats"

absvtype Args_vtype
vtypedef Args = Args_vtype

vtypedef args_struct =
    @{ 
        subcommand=Option_vt(SubCommand),
        command_map=map(string, SubCommand),
        prog_name=string,
        author=string,
        about=string,
        version=string,
        captured_args=$HT.hashtbl(strptr, List_vt(strptr)),
        captured_command=Option_vt(strptr),
        has_all_required=bool,
        captured_prog_name=Option_vt(string)
    }
datavtype Args =
| ARGS of args_struct

datavtype ArgError =
| PrintHelp of ()
| Invalid of ()
| MissingRequired of List1_vt(strptr)
| MissingValues of List1_vt(strptr)

fn new_args(prog_name: string): Args

fn set_author(args: !Args, author: string): void

fn set_about(args: !Args, about: string): void

fn set_version(args: !Args, version: string): void

fn add_arg(args: !Args, arg: Arg): void

fn free_args(args: Args): void

fn {a:vt@ype} get_value(args: !Args, key: string): Option_vt(a)

fn {a:vt@ype} get_values(args: !Args, key: string): List_vt(a)

fn {a:vt@ype} string_to_value{l:addr | l > null}(value: !strptr(l)): Option_vt(a)

fn print_help(args: !Args): void

fn fprint_args(out: FILEref, args: !Args): void

fn parse{n:int | n > 0}(args: !Args, argc: int(n), argv: !argv(n)): result_vt((), ArgError)

fn{a:vt@ype} fprint_arg(out: FILEref, x: !a): void

overload fprint with fprint_arg
overload print with print_help

fn {a:vt@ype} get_parsed(args: !Args): a 

fn handle_error(args: !Args, err: ArgError): void

fn free_error(err: ArgError): void

fn add_subcommand(args: !Args, sc: SubCommand): void

fn{a:type} get_command(args: !Args): Option_vt(a)

fn{a:t@ype} command_from_string(cmd: !strptr): Option_vt(a)

fn is_subcommand(args: !Args, cmd: string): bool