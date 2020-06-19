#include "./../HATS/includes.hats"
staload UN = "prelude/SATS/unsafe.sats"
staload _ = "../src/DATS/arg.dats"
staload "../src/SATS/subcommand.sats"
#define ATS_DYNLOADFLAG 0

implement free_subcommand(sc) = {
    val+~SC(s) = sc
    val () = linmap_freelin(s.args_map)
}

implement linmap_freelin$clear<SubCommand>(x) = free_subcommand(x)

implement new_subcommand(name) = sc where {
    val sc = SC(_)
    val SC(s) = sc
    val () = s.args_map := linmap_nil()
    val () = s.command := name
    prval () = fold@sc
}

implement add_arg(sc, arg) = {
    val+@SC(s) = sc
    val+@$ARG.A(a) = arg
    val name = a.name
    prval() = fold@arg
    val () = linmap_insert_any(s.args_map, name, arg)
    prval () = fold@sc
}