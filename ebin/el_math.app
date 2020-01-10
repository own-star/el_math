{application, 'el_math', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['el_math','el_math_app','el_math_sup','el_math_worker']},
	{registered, [el_math_sup]},
	{applications, [kernel,stdlib]},
	{mod, {el_math_app, []}},
	{env, []}
]}.