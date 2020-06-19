
// datatype ArgType =
// | String of ()
// | Int of ()
// | Float of ()
// | Char of ()
datavtype Position =
| NoPos of ()
| Pos of int

vtypedef arg_struct = @{ 
    name=string, 
    description=string, 
    short=Option_vt(string),
    required=bool,
    needs_value=bool,
    position=Option_vt(int)
    // arg_type=ArgType
}

datavtype Arg =
| A of arg_struct

fn{} new_arg(name: string, desc: string): Arg

fn{} free_arg(arg: Arg):<!wrt> void

fn{} make_required(arg: !Arg): void

fn{} set_short(arg: !Arg, short: string): void

fn{} set_needs_value(arg: !Arg): void

fn{} set_position{n:nat}(arg: !Arg, pos: int(n)): void

symintr .set_short .set_position
overload .set_short with set_short
overload .set_position with set_position