#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

datatype DType =
| Int of int
| Float of float
| String of string
| Bool of bool

vtypedef func = string -<cloptr1> void
vtypedef arg_struct(a:t@ype) = @{ 
    (* idx=a, *) 
    arg_type=DType,
    name=string, 
    description=string, 
    action=Option_vt(func), 
    short=Option_vt(string) 
}

datavtype Arg(a:t@ype) =
| A of arg_struct(a) 

extern fn {a:t@ype} new_arg(t: DType, name: string, desc: string): Arg(a)

implement {a} new_arg(t, name, desc) = arg where {
  val y = @{ arg_type=t, name=name, description=desc, action=None_vt(), short=None_vt() }
  val arg = A(y)
}

extern fn {a:t@ype} add_action(arg: !Arg(a), action: func): void

implement {a} add_action(arg, action) = () where {
  val @A(ar) = arg
  val () = case ar.action of
  | ~Some_vt(f) => cloptr_free($UNSAFE.castvwtp0(f))
  | ~None_vt() => ()
  val () = ar.action := Some_vt(action)
  prval () = fold@(arg)
}

extern fn {a:t@ype} set_short(arg: !Arg(a), short: string): void

implement {a} set_short(arg, short) = () where {
  val @A(ar) = arg
  val () = case ar.short of
  | ~Some_vt(_) => ()
  | ~None_vt() => ()
  val () = ar.short := Some_vt(short)
  prval () = fold@(arg)
}
