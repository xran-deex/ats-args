#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

datavtype dash_type =
| Single of ()
| Double of ()
| None of ()

fn{} get_dash_type{n:int}(arg: string(n)): dash_type 

fn{} get_arg_name{m:nat }(arg1: string(m), dashtype: dash_type): strptr