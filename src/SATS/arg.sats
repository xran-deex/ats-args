
// datatype ArgType =
// | String of ()
// | Int of ()
// | Float of ()
// | Char of ()

vtypedef arg_struct = @{ 
    name=string, 
    description=string, 
    short=Option_vt(string),
    required=bool
    // arg_type=ArgType
}

datavtype Arg =
| A of arg_struct

fn{} new_arg(name: string, desc: string): Arg

// fn new_arg_with_type(name: string, desc: string, type: ArgType): Arg

fn{} make_required(arg: !Arg): void

fn{} set_short(arg: !Arg, short: string): void

symintr .set_short
overload .set_short with set_short