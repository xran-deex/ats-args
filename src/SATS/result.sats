
dataviewtype result_vt(a:vt@ype, b:vt@ype) =
| Ok of a
| Error of b

fn {a,b:vt@ype} is_ok(r: !result_vt(a, b)): bool

fn {a,b:vt@ype}
fprint_result(out: FILEref, r: !result_vt(a,b)): void
