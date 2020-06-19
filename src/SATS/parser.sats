#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "ats-result/src/SATS/result.sats"
staload "./../SATS/arg.sats"
staload "./../SATS/args.sats"

fun{} parse_args{n:int | n > 1} (args: !Args, argc: int(n), argv: !argv(n)): result_vt((), ArgError)

fun{} do_parse{n:int | n > 1}{m:nat | m < n && m > 0} (args: !Args, argc: int(n), argv: !argv(n), cur: int(m)): result_vt((), ArgError)

// fun{} gatherArgsIntoList{x:int | x > 1}(argc: int(x), argv: !argv(x)): list_vt([m:nat] string(m), x-1)