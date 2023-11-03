validate_option([Option, L-U]) :- 
    between(L, U, Option).

validate(0, _, _) :- !.
validate(Input, Validator, Arguments) :- call(Validator, [Input | Arguments]).

raw_input(Input, Validator, Arguments) :- raw_input(Input, Validator, Arguments, 'option').

raw_input(Input, Validator, Arguments, _) :-
    read(Input), 
    validate(Input, Validator, Arguments),
    !.

raw_input(Input, Validator, Arguments, OptionType) :-
    repeat,
    format('Invalid! Provide valid ~s.\n', [OptionType]),
    read(Input),
    validate(Input, Validator, Arguments),
    !.

is_cancel(0) :- !.

read_input(Input, Validator, Arguments) :- 
    raw_input(Input, Validator, Arguments),
    \+ is_cancel(Input).

read_input(Input, Validator, Arguments, OptionType) :- 
    raw_input(Input, Validator, Arguments, OptionType),
    \+ is_cancel(Input).


