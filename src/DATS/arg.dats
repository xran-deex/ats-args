#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

vtypedef arg_struct = @{ 
    name=string, 
    description=string, 
    short=Option_vt(string),
    required=bool
}

datavtype Arg =
| A of arg_struct

extern fn new_arg(name: string, desc: string): Arg

implement new_arg(name, desc) = arg where {
  val y = @{ name=name, description=desc, short=None_vt(), required=false }
  val arg = A(y)
}

extern fn make_required(arg: !Arg): void

implement make_required(arg) = () where {
  val @A(ar) = arg
  val () = ar.required := true
  prval() = fold@(arg)
}

extern fn set_short(arg: !Arg, short: string): void

implement set_short(arg, short) = () where {
  val @A(ar) = arg
  val () = case ar.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
  val () = ar.short := Some_vt(short)
  prval () = fold@(arg)
}

symintr .set_short
overload .set_short with set_short
