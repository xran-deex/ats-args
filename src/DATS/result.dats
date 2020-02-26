#include "./../HATS/includes.hats"
staload "./../SATS/result.sats" 
#define ATS_DYNLOADFLAG 0

implement {a,b} is_ok(r) = res where {
    val res = case+ r of
              | @Ok _ => (fold@r; true)
              | @Error _ => (fold@r; false)
}

implement {a,b}
fprint_result(out, r) = 
case+ r of
| Ok _ => println!("Ok")
| Error _ => println!("Error")

overload fprint with fprint_result