#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "libats/libc/SATS/stdlib.sats"
staload "./../SATS/helper.sats"
staload "libats/SATS/linmap_list.sats"
staload "./../SATS/arg.sats"
#define ATS_DYNLOADFLAG 0

implement{} debug() = res where {
    val DEBUG = getenv_gc("DEBUG")
    val res = if isneqz(DEBUG) then res where {
      val res = DEBUG = "1" || DEBUG = "true"
    } else false
    val () = free(DEBUG)
}

implement{} get_dash_type(arg) = let
  val () = assertloc(string_length(arg) > 0)
  val dash1 = eq_char0_char0(string_get_at(arg, 0), '-')
  val dash2 = (if string_length(arg) > 1 then eq_char0_char0(string_get_at(arg, 1), '-') else false): bool
in
  case dash1 of
  | true =>
      (case dash2 of
      | true => Double()
      | false => Single())
  | false => None()
end

implement{} get_arg_name(arg1, dashtype) =
  (case dashtype of
  | ~Single() => res where {
      val start = i2sz(1)
      val len = string1_length(arg1)
      val () = assertloc(len > 1)
      val res = string_make_substring(arg1, start, len - 1)
      val res = strnptr2strptr res
  }
  | ~Double() => res where {
      val start = i2sz(2)
      val len = string1_length(arg1)
      val () = assertloc(len > 2)
      val () = assertloc(string1_length(arg1) >= (start+len-2))
      val res = string_make_substring(arg1, start, len - 2)
      val res = strnptr2strptr res
  }
  | ~None() => res where {
      val res = string0_copy(arg1)
  }): strptr

implement{} get_short_and_long(maps, arg) = env where {
    var env: Option_vt(pair) = None_vt()
    val () = linmap_foreach_env<string,Arg><Option_vt(pair)>(maps, env) where {
        implement linmap_foreach$fwork<string,Arg><Option_vt(pair)>(k,v,e) = {
            val+@A(ar) = v
            val short = (case+ ar.short of
            | @Some_vt(s) => res where {
                val res = s
                prval() = fold@(ar.short)
            }
            | @None_vt() => "" where {
                prval() = fold@(ar.short)
            }): string
            val () = if ar.name = arg || short = arg then {
                val-~None_vt() = e
                val () = e := Some_vt(@(ar.name, short))
            }
            prval() = fold@(v)
        }
    }
}
