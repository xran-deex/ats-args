
vtypedef arg_struct = @{ 
    name=string, 
    description=string, 
    short=Option_vt(string),
    required=bool
}

datavtype Arg =
| A of arg_struct

fn new_arg(name: string, desc: string): Arg

fn make_required(arg: !Arg): void

fn set_short(arg: !Arg, short: string): void

symintr .set_short
overload .set_short with set_short