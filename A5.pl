:- set_prolog_flag(double_quotes, chars).

% Delimiters base.
delimiter --> ",".
delimiter --> "\s".
delimiter --> "\t".
delimiter --> "\n".

% To handle more than one delimiter between expressions.
delimiter_array --> delimiter.
delimiter_array --> delimiter, delimiter_array.

% Special symbols base.
special --> "+".
special --> "-".
special --> ">".
special --> "<".
special --> "=".
special --> "*".
special --> "_".

% Digits base, cell of every number.
digit --> "0".
digit --> "1".
digit --> "2".
digit --> "3".
digit --> "4".
digit --> "5".
digit --> "6".
digit --> "7".
digit --> "8".
digit --> "9".

% Number, contains digits.
number --> digit.
number --> digit, number.

% Any printable character.
char -->[A], {char_type(A, print)}.

% String without '"'.
substring --> char.
substring --> char, substring.

% String, contains any character.
string --> "\"", substring, "\"".

% id should start from letter.
letter --> [A], {char_type(A, alpha)}.

% Id, two cases like in condition.
id --> letter, letter_start.
id --> special, special_start.

% Contains only letters, numbers, special symbols.
letter_start --> "".
letter_start --> letter, letter_start.
letter_start --> number, letter_start.
letter_start --> special, letter_start.

% Contains only numbers, special symbols.
special_start --> "".
special_start --> number, special_start.
special_start --> special, special_start.

% Starts from ":".
keyword --> ":", id.

% Like in condition.
atom --> number.
atom --> string.
atom --> id.
atom --> keyword.

% Every atom is s_expression.
s_expression --> atom.

% Like in condition, nonull set of expressions in ().
s_expression --> "(", s_expression, expr_array, ")".

% Like in condition, nonull set of expressions in [].
s_expression --> "[", s_expression, expr_array, "]".

% To handle more than one expression in brackets.
expr_array --> "".
expr_array --> delimiter_array, s_expression, expr_array.

% Empty set is allowed.
s_expression --> "{", "", "}".

% We have even number of elements in set.
s_expression --> "{", s_expression, even_expr, "}".

% Only two elements in set.
even_expr --> delimiter_array, s_expression.

% More than two elements: 4, 6, 8...
even_expr --> delimiter_array, s_expression, even.

% To have only even number of expressions.
even --> s_expression, delimiter_array, s_expression.
even --> delimiter_array, even_or, delimiter_array, even.
even --> delimiter_array, even, delimiter_array, even_or.

even_or --> "".
even_or --> s_expression, delimiter_array, s_expression.

