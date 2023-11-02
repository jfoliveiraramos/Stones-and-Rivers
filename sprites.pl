% EmptySlot Slot
piece_sprite(emptySlot, top,       '     ') :- !.
piece_sprite(emptySlot, mid,       '     ') :- !.
piece_sprite(emptySlot, bottom,    '     ') :- !.

% Square Target
piece_sprite(squareScr, top,       '  +  ') :- !.
piece_sprite(squareScr, mid,       '+ a +') :- !.
piece_sprite(squareScr, bottom,    '  +  ') :- !.

% Circle Target
piece_sprite(circleScr, top,       '  +  ') :- !.
piece_sprite(circleScr, mid,       '+ b +') :- !.
piece_sprite(circleScr, bottom,    '  +  ') :- !.

% Square Scored
piece_sprite(squareOcp, top,       ' x x ') :- !.
piece_sprite(squareOcp, mid,       'x A x') :- !.
piece_sprite(squareOcp, bottom,    ' x x ') :- !.

% Circle Scored
piece_sprite(circleOcp, top,       ' x x ') :- !.
piece_sprite(circleOcp, mid,       'x B x') :- !.
piece_sprite(circleOcp, bottom,    ' x x ') :- !.

% Square Stone
piece_sprite(squareStn, top,       ' A A ') :- !.
piece_sprite(squareStn, mid,       'A   A') :- !.
piece_sprite(squareStn, bottom,    ' A A ') :- !.

% Square Vertical River
piece_sprite(squareVrt, top,       '  A  ') :- !.
piece_sprite(squareVrt, mid,       '  A  ') :- !.
piece_sprite(squareVrt, bottom,    '  A  ') :- !.

% Square Horizontal River
piece_sprite(squareHrz, top,       '     ') :- !.
piece_sprite(squareHrz, mid,       'A A A') :- !.
piece_sprite(squareHrz, bottom,    '     ') :- !.

% Circle Stone
piece_sprite(circleStn, top,       ' B B ') :- !.
piece_sprite(circleStn, mid,       'B   B') :- !.
piece_sprite(circleStn, bottom,    ' B B ') :- !.

% Circle Vertical River
piece_sprite(circleVrt, top,       '  B  ') :- !.
piece_sprite(circleVrt, mid,       '  B  ') :- !.
piece_sprite(circleVrt, bottom,    '  B  ') :- !.

% Circle Horizontal River
piece_sprite(circleHrz, top,       '     ') :- !.
piece_sprite(circleHrz, mid,       'B B B') :- !.
piece_sprite(circleHrz, bottom,    '     ') :- !.
