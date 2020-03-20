#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload "./../SATS/helper.sats"
#define ATS_DYNLOADFLAG 0

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
      // val len = len - 2
      // val res = string_make_substring(arg1, i2sz(0), len)
      // val res = strnptr2strptr res
      val res = string0_copy(arg1)
  }): strptr