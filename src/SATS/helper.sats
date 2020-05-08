#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/SATS/linmap_list.sats"
staload "./../SATS/arg.sats"
staload "./../SATS/args.sats"

datavtype dash_type =
| Single of ()
| Double of ()
| None of ()

fn{} debug(): bool

fn{} get_dash_type{n:int}(arg: string(n)): dash_type 

fn{} get_arg_name{m:nat }(arg1: string(m), dashtype: dash_type): strptr

typedef pair = @(string,string)

fn{} get_short_and_long{n:int}(maps: !map(string, Arg), arg: string(n)): Option_vt(pair)

fn{} get_arg_for_position(args: !map(string, Arg), pos: int): Option_vt(string)