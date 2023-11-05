% validate_option(+ArgumentsList)
% First argument is the option to validate.
% Second argument is a range of valid values which the provided option must meet.
validate_option([Option, L-U]) :- 
    between(L, U, Option).

% validate(+Input, +Validator, +Arguments)
% Validates the provided input using the provided validator and arguments.
validate(0, _, _) :- !.
validate(Input, Validator, Arguments) :- call(Validator, [Input | Arguments]).

% raw_input(-Input, +Validator, +Arguments)
% Reads the input from the user and validates it using the provided validator and arguments.
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

% is_cancel(+Input)
% Checks if the provided input is the cancel option.
is_cancel(0) :- !.

% read_input(-Input, +Validator, +Arguments)
% Reads the input from the user and validates it using the provided validator and arguments.
% If the input is the cancel option, it fails, which will be used to cancel the current action.
read_input(Input, Validator, Arguments) :- 
    raw_input(Input, Validator, Arguments),
    \+ is_cancel(Input).
read_input(Input, Validator, Arguments, OptionType) :- 
    raw_input(Input, Validator, Arguments, OptionType),
    \+ is_cancel(Input).

% get_enter/0
% Reads characters until it finds a newline character.
get_enter :- get_char('\n'), !.
get_enter :- get_char(_), get_enter.

% wait_for_input/0
% Waits for the user to press enter.
wait_for_input :-
    write('Press ENTER to continue'),
    get_enter.