#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "ats-unit-testing/ats-unit-testing.hats"
#include "./../ats-args.hats"

staload $ARGS
staload $ARG

implement gequal_ref_ref<strptr>(x, y) = compare_strptr_strptr(x,y) = 0
implement tostring<double>(x) = copy(tostring_double(x))
implement tostring<string>(x) = copy(x)

assume argv_int_vtype(n) = arrayptr(string, n)

fn test1(c: &Context): void = () where {

    val args = $ARGS.new_args("")

    val ls = (arrayptr)$arrpsz{string} ("prog", "--aaaaa")
    val res = $ARGS.parse(args, 2, ls)
    val () = free(ls)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, true, "Result should be ok")
    | ~Error(err) => $ARGS.handle_error(args, err)

    val () = $ARGS.free_args(args)
}

fn test2(c: &Context): void = () where {

    val args = $ARGS.new_args("")

    val ls = (arrayptr)$arrpsz{string} ("prog", "--aaaaa")
    val res = $ARGS.parse(args, 2, ls)
    val @$ARGS.ARGS(a) = args
    val-@Some_vt(n) = a.captured_prog_name
    val () = assert_equals1<string>(c, "prog", n)
    val () = free(ls)
    prval() = fold@(a.captured_prog_name)
    prval() = fold@(args)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, true, "Result should be ok")
    | ~Error(err) => $ARGS.handle_error(args, err)

    val () = $ARGS.free_args(args)
}

fn test3(c: &Context): void = () where {

    val args = $ARGS.new_args("")
    val arg = $ARG.new_arg("x", "")
    val () = $ARG.make_required(arg)
    val () = $ARGS.add_arg(args, arg)
    val ls = (arrayptr)$arrpsz{string} ("prog", "-a")
    val res = $ARGS.parse(args, 2, ls)
    val () = free(ls)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, false, "Result should not be ok")
    | ~Error(err) => case- err of
                     | ~$ARGS.MissingRequired(v) => () where {
                         val () = assert_true_msg(c, true, "Missing required should be called")
                         val () = list_vt_freelin(v)
                     }

    val () = $ARGS.free_args(args)
}

fn test4(c: &Context): void = () where {

    val args = new_args("")
    val arg = new_arg("test", "")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val () = add_arg(args, arg)
    val ls = (arrayptr)$arrpsz{string} ("prog", "--test")
    val res = parse(args, 2, ls)
    val () = free(ls)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, false, "Result should not be ok")
    | ~Error(err) => case- err of
                     | ~MissingValues(v) => () where {
                         val () = assert_true_msg(c, true, "Missing value should be called")
                         val () = list_vt_freelin(v)
                     }
    val () = free_args(args)
}

fn test5(c: &Context): void = () where {

    val args = new_args("")
    val arg = new_arg("test", "")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val () = add_arg(args, arg)
    val ls = (arrayptr)$arrpsz{string} ("prog", "--test", "2")
    val res = parse(args, 3, ls)
    val () = free(ls)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, true, "Result should be ok")
    | ~Error(err) => case- err of
                     | ~MissingValues(v) => () where {
                         val () = assert_true_msg(c, false, "Missing value should not be called")
                         val () = list_vt_freelin(v)
                     }
    val-~Some_vt num = get_value<int>(args, "test")
    val () = assert_equals1<int>(c, 2, num)
    val () = free_args(args)
}

fn test6(c: &Context): void = () where {

    val args = new_args("")
    val arg = new_arg("test", "")
    val () = arg.set_short("t")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val () = add_arg(args, arg)
    val ls = (arrayptr)$arrpsz{string} ("prog", "-t", "2")
    val res = parse(args, 3, ls)
    val () = free(ls)

    val () = case+ res of
    | ~Ok(_) => assert_true_msg(c, true, "Result should be ok")
    | ~Error(err) => case- err of
                     | ~MissingValues(v) => () where {
                         val () = assert_true_msg(c, false, "Missing value should not be called")
                         val () = list_vt_freelin(v)
                     }
    val-~Some_vt num = get_value<int>(args, "test")
    val () = assert_equals1<int>(c, 2, num)
    val-~Some_vt num = get_value<int>(args, "t")
    val () = assert_equals1<int>(c, 2, num)
    val () = free_args(args)
}

fn test7(c: &Context): void = () where {

    val args = new_args("")
    val arg = new_arg("test", "")
    val () = arg.set_short("t")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val () = add_arg(args, arg)
    val ls = (arrayptr)$arrpsz{string} ("prog", "-t", "2", "3")
    val-~Ok(_) = parse(args, 4, ls)
    val () = free(ls)

    val nums = get_values<int>(args, "test")
    val () = assert_equals1<int>(c, 2, list_vt_length(nums))
    val () = free(nums)
    val () = free_args(args)
}

fn test8(c: &Context): void = () where {
    val args = new_args("")
    val arg = new_arg("test", "")
    val () = arg.set_short("t")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val sc = $SC.new_subcommand("cmd", "a subcommand")
    val () = $SC.add_arg(sc, arg)
    val () = add_subcommand(args, sc)
    val ls = (arrayptr)$arrpsz{string} ("prog", "cmd", "-t", "1", "2")
    val-~Ok(_) = parse(args, 5, ls)
    val () = free(ls)

    val nums = get_values<int>(args, "test")
    val () = assert_equals1<int>(c, 2, list_vt_length(nums))
    vtypedef state = @{c=Context, i=int}
    var st = @{ c=c, i=1 }: state
    val () = list_vt_foreach_env<int><state>(nums, st) where {
        implement list_vt_foreach$fwork<int><state>(n, e) = {
            val () = assert_equals1<int>(e.c, e.i, n)
            val () = e.i := e.i + 1
        }
    }
    val () = c := st.c
    val () = free(nums)
    val () = free_args(args)
}

fn test9(c: &Context): void = () where {
    val args = new_args("")
    val arg = new_arg("test", "")
    val arg2 = new_arg("test2", "")
    val () = arg.set_short("t")
    val () = make_required(arg)
    val () = set_needs_value(arg)
    val sc = $SC.new_subcommand("cmd", "a subcommand")
    val () = $SC.add_arg(sc, arg)
    val sc2 = $SC.new_subcommand("other", "another subcommand")
    val () = $SC.add_arg(sc2, arg2)
    val () = add_subcommand(args, sc)
    val () = add_subcommand(args, sc2)
    val ls = (arrayptr)$arrpsz{string} ("prog", "cmd", "-t", "2")
    val-~Ok(_) = parse(args, 4, ls)
    val () = free(ls)

    val-~Some_vt(num) = get_value<int>(args, "test")
    val () = assert_equals1<int>(c, 2, num)

    val ls = (arrayptr)$arrpsz{string} ("prog", "other", "--test2", "2")
    val-~Ok(_) = parse(args, 4, ls)
    val () = free(ls)

    val-~Some_vt(num) = get_value<int>(args, "test2")
    val () = assert_equals1<int>(c, 2, num)
    val () = free_args(args)
}

fn test10(c: &Context): void = () where {
    val args = new_args("")
    val arg = new_arg("test", "")
    val sc = $SC.new_subcommand("cmd", "a subcommand")
    val () = $SC.add_arg(sc, arg)
    val () = add_subcommand(args, sc)
    val ls = (arrayptr)$arrpsz{string} ("prog", "--help")
    val-~Error(err) = parse(args, 2, ls)
    val () = free(ls)

    val () = free_error(err)

    val () = assert_true(c, true)

    val () = free_args(args)
}

implement main(argc, argv) = 0 where {
    val r = create_runner()
    val s = create_suite("ats-args tests")

    val () = add_test(s, "test1", test1)
    val () = add_test(s, "test2", test2)
    val () = add_test(s, "test3", test3)
    val () = add_test(s, "test4", test4)
    val () = add_test(s, "test5", test5)
    val () = add_test(s, "test6 - get_value by long or short works", test6)
    val () = add_test(s, "test7 - get_values returns a list", test7)
    val () = add_test(s, "test8 - subcommand", test8)
    val () = add_test(s, "test9 - multiple subcommands", test9)
    val () = add_test(s, "test10 - subcommand only prints help", test10)

    val () = add_suite(r, s)
    val () = run_tests(r)
    val () = free_runner(r)
}