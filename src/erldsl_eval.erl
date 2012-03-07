-module(erldsl_eval).

-export([new_context/0, eval/2]).

-record(var, {name, type}).
%% -record(context, {name, vars}).


new_context() ->
	dict:new().


eval(Context, [Exp | Exps]) ->
	{Context1, Result} = eval_exp(Context, Exp),
	case Exps of
		[] -> {Context1, Result};
		_ -> eval(Context1, Exps)
	end;

eval(Context, Exp) ->
	eval_exp(Context, Exp).

eval_exp(Context, {assign, _Line, {var, _Line2, Name}, Exp}) ->
	{Context1, Result} = eval_exp(Context, Exp),
	assign_variable(Context1, Name, Result);

eval_exp(Context, {const, _Line, {_Type, _Line2, Value}}) ->
	{Context, Value};

eval_exp(Context, {var, Line, {'NAME', _Line2, Name}}) ->
	case dict:is_key(Name, Context) of
		true ->
			{Context, dict:fetch(Name, Context)};
		false ->
			exit({unknown_variable, Name, Line})
	end;
eval_exp(Context, {op, _Line, Op, LeftExp, RightExp}) ->
	{Context1, RightResult} = eval_exp(Context, RightExp),
	{Context2, LeftResult} = eval_exp(Context1, LeftExp),
	{Context2, do_op(Op, LeftResult, RightResult)};

eval_exp(Context, {op, _Line, Op, RightExp}) ->
	{Context1, RightResult} = eval_exp(Context, RightExp),
	{Context1, do_op(Op, RightResult)};

eval_exp(_Context, {Any, Line, _Oth}) ->
	exit({unknown_statment, Any, Line}).

do_op('-', Result) ->
	-Result.

do_op('-', Left, Right) ->
	Left - Right;
do_op('+', Left, Right) ->
	Left + Right;
do_op('*', Left, Right) ->
	Left * Right;
do_op('/', Left, Right) ->
	Left / Right.

assign_variable(Context, {'NAME', _Line, Name}, Result) ->
	Context1 = dict:store(Name, Result, Context),
	{Context1, Result}.
