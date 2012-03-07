
Nonterminals
	chunk explist semi exp assign bin_op un_op var un_op_el bin_op_el const uminus.
Terminals
	NAME NUMBER 'true' 'false'
	%% Arithmetic
	'+' '-' '*' '/' '%' '**'
	%% Boolean
	'and' 'or' '&&' '||' 'not' '!'
	%% Comparison
	'<' '>' '<=' '>=' '!=' '=='
	%% Code block
	'{' '}' 'begin' 'end'

	';' '='.


Rootsymbol chunk .

Left 100 'or' '||'.
Left 200 'and' '&&'.
Left 300 '<' '>' '<=' '>=' '!=' '=='.
Left 400 '+' '-'.
Left 500 '*' '/' '%'.
Unary 600 'not' '!' uminus.
Right 700 '**'.


chunk -> explist : '$1'.

semi -> '$empty'.
semi -> ';'.

explist -> '$empty' : [].
explist -> explist semi exp : '$1' ++ ['$3'].

exp -> assign : '$1'.
exp -> bin_op : '$1'.
exp -> un_op : '$1'.
exp -> const : '$1'.
exp -> var : '$1'.

assign -> var '=' exp : {assign, line('$2'), '$1', '$3'}.

var -> NAME : {var, line('$1'), '$1'}.

un_op -> un_op_el exp : {op, line('$1'), cat('$1'), '$2'}.
un_op -> uminus : '$1'.
uminus -> '-' exp : {op, line('$1'), cat('$1'), '$2'}.

un_op_el -> 'not' : $1.
un_op_el -> '!' : $1.

bin_op -> exp bin_op_el exp : {op, line('$2'), cat('$2'), '$1', '$3'}.
bin_op_el -> '+' : '$1'.
bin_op_el -> '-' : '$1'.
bin_op_el -> '*' : '$1'.
bin_op_el -> '/' : '$1'.
bin_op_el -> '%' : '$1'.
bin_op_el -> '<' : '$1'.
bin_op_el -> '>' : '$1'.
bin_op_el -> '<=' : '$1'.
bin_op_el -> '>=' : '$1'.
bin_op_el -> '!=' : '$1'.
bin_op_el -> '==' : '$1'.
bin_op_el -> 'and' : '$1'.
bin_op_el -> 'or' : '$1'.
bin_op_el -> '&&' : '$1'.
bin_op_el -> '||' : '$1'.

const -> NUMBER : {const, line('$1'), '$1'}.
const -> 'true' : {const, line('$1'), '$1'}.
const -> 'false' : {const, line('$1'), '$1'}.

Erlang code.

-export([chunk/1]).

line(T) -> out(T), element(2, T).
cat(T) -> element(1, T).

out(T) ->
	io:format("~narg for line func: ~p~n", [T]).

chunk(Ts) ->
	case F = parse(Ts) of
		{error, {Line, erldsl_parse, MsgList}} ->
			io:format("~nerldsl parse error in line ~p: ~p~n", [Line, lists:flatten(MsgList)]),
			F;
		{ok, Body} ->
			Body
		end.
