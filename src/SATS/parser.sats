#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "ats-result/src/SATS/result.sats"
staload "./../SATS/arg.sats"
staload "./../SATS/args.sats"

fun{} parse_args{n:int | n > 1} (args: !Args, argc: int(n), argv: !argv(n)): result_vt((), ArgError)

fun{} do_parse{n:int | n > 1}{m:nat | m < n && m > 0} (args: !Args, argc: int(n), argv: !argv(n), cur: int(m)): result_vt((), ArgError)

fun{} gatherArgsIntoList{n:int | n > 1}(argc: int(n), argv: !argv(n)): List0_vt([m:nat] string(m))

fun{} gatherLoop{n:int | n > 1}{i:nat | i < n && i > 0}(acc: &List0_vt([m:nat] string(m)) >> _, argc: int(n), argv: !argv(n), cur: int(i)): void