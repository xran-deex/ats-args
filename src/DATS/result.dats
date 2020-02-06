#include "./../HATS/includes.hats"
#define ATS_DYNLOADFLAG 0

dataviewtype result_vt(a:vt@ype, b:vt@ype) =
| Ok of a
| Error of b

extern fn {a,b:vt@ype} is_ok(r: !result_vt(a, b)): bool

implement {a,b} is_ok(r) = res where {
    val res = case+ r of
              | @Ok _ => (fold@r; true)
              | @Error _ => (fold@r; false)
}

extern fn {a,b:vt@ype}
fprint_result(out: FILEref, r: !result_vt(a,b)): void

implement {a,b}
fprint_result(out, r) = 
case+ r of
| Ok _ => println!("Ok")
| Error _ => println!("Error")

overload fprint with fprint_result