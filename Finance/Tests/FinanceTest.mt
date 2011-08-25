(* Mathematica Test File *)

Test[
	MkOption[long, call, 120]
	,
	{"position" -> "long", "type" -> "call", "strike" -> 120}
	,
	TestID->"FinanceTest-MkOption"
]

Test[
	OptionPayoff[MkOption[long, call, 100], S]
	,
	Max[0, -100 + S],
	TestID->"FinanceTest-OptionPayoff1"
]

Test[
	OptionPayoff[MkOption[long, put, 100], S]
	,
	Max[0, 100 - S],
	TestID->"FinanceTest-OptionPayof2"
]

Test[
	OptionPayoff[MkOption[short, call, 100], S]
	,
	- Max[0, -100 + S],
	TestID->"FinanceTest-OptionPayoff1"
]

Test[
	OptionPayoff[MkOption[short, put, 100], S]
	,
	- Max[0, 100 - S]
	,
	TestID->"FinanceTest-OptionPayof2"
]

Test[
	PortfolioPayoff[{MkOption[short, put, 100], MkOption[long, call, 100]}, S]
	,
	-Max[0, 100 - S] + Max[0, -100 + S]
	,
	TestID->"FinanceTest-PortfolioPayoff"
]

		