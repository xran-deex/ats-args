#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0
staload $ARG 

implement{} new_arg(name, desc) = arg where {
  val y = @{ name=name, description=desc, short=None_vt(), required=false, needs_value=false, position=None_vt() }
  val arg = A(y)
}

implement{} make_required(arg) = () where {
  val @A(ar) = arg
  val () = ar.required := true
  prval() = fold@(arg)
}

implement{} set_short(arg, short) = () where {
  val @A(ar) = arg
  val () = case ar.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
  val () = ar.short := Some_vt(short)
  prval () = fold@(arg)
}

implement{} set_position(arg, pos) = () where {
  val @A(ar) = arg
  val () = case ar.position of
  | ~None_vt() => ()
  | ~Some_vt _ => ()
  val () = ar.position := Some_vt(pos)
  prval () = fold@(arg)
}

implement{} set_needs_value(arg) = () where {
  val @A(ar) = arg
  val () = ar.needs_value := true
  prval () = fold@(arg)
}

implement{} free_arg(arg) =
case arg of
| ~A(a) => () where {
  val () = case a.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
  val () = case a.position of
  | ~None_vt() => ()
  | ~Some_vt(_) => ()
}

implement linmap_freelin$clear<Arg>(x) = free_arg(x)