-module(erldsl_eval).

-export([new_context/0, eval/2]).

-record(var, {name, type}).
%% -record(context, {name, vars}).


new_context() ->
	dict:new().


eval(Context, [Exp | Exps]) ->
	{Context1, _} = eval_exp(Context, Exp),
	eval(Context1, Exps);

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

eval_exp(_Context, {Any, Line, _Oth}) ->
	exit({unknown_statment, Any, Line}).


assign_variable(Context, {'NAME', _Line, Name}, Result) ->
	Context1 = dict:store(Name, Result, Context),
	{Context1, Result}.
