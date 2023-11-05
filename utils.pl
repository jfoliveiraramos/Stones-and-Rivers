validate_option([Option, L-U]) :- 
    between(L, U, Option).

validate(0, _, _) :- !.
validate(Input, Validator, Arguments) :- call(Validator, [Input | Arguments]).

raw_input(Input, Validator, Arguments) :- raw_input(Input, Validator, Arguments, 'option').

raw_input(Input, Validator, Arguments, _) :-
    read(Input),
    skip_line,
    validate(Input, Validator, Arguments),
    !.

raw_input(Input, Validator, Arguments, OptionType) :-
    repeat,
    format('Invalid! Provide valid ~s.\n', [OptionType]),
    read(Input),
    skip_line,
    validate(Input, Validator, Arguments),
    !.

is_cancel(0) :- !.

read_input(Input, Validator, Arguments) :- 
    raw_input(Input, Validator, Arguments),
    \+ is_cancel(Input).

read_input(Input, Validator, Arguments, OptionType) :- 
    raw_input(Input, Validator, Arguments, OptionType),
    \+ is_cancel(Input).

get_enter :- get_char('\n'), !.
get_enter :- get_char(_), get_enter.

wait_for_input :-
    write('Press ENTER to continue'),
    get_enter.