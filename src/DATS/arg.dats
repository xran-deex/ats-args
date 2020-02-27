#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

implement new_arg(name, desc) = arg where {
  val y = @{ name=name, description=desc, short=None_vt(), required=false }
  val arg = A(y)
}

implement make_required(arg) = () where {
  val @A(ar) = arg
  val () = ar.required := true
  prval() = fold@(arg)
}

implement set_short(arg, short) = () where {
  val @A(ar) = arg
  val () = case ar.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
  val () = ar.short := Some_vt(short)
  prval () = fold@(arg)
}
