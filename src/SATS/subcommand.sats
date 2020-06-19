#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/SATS/linmap_list.sats"
#include "hashtable-vt/hashtable_vt.hats"
staload "ats-result/src/SATS/result.sats"
staload "./../SATS/arg.sats"

vtypedef subcommand_struct =
    @{
        args_map=map(string, Arg),
        command=string
    }

datavtype SubCommand =
| SC of subcommand_struct

fn free_subcommand(sc: SubCommand):<!wrt> void

fn get_command(sc: !SubCommand): string

fn new_subcommand(name: string): SubCommand

fn add_arg(sc: !SubCommand, arg: Arg): void