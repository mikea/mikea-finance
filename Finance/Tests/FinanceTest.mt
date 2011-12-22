(* Mathematica Test File *)

(* Test[Definition[Payoff],	{},	TestID->"FinanceTest-Foo" ] *)

(* Test bonds *)
Test[
	MkBond[price->120],
	bond[price->120],
	TestID->"FinanceTest-MkBond"
]

Test[
	Payoff[MkBond[price->120]],
	120,
	TestID->"FinanceTest-BondPayoff"
]


(* Test options *)
Test[
	MkOption[type->call, strike->120],
	option[type -> call, strike -> 120],
	TestID->"FinanceTest-MkOption"
]

(* Test portfolio position. *)
	
Test[
	MkPortfolioPosition[asset->MkBond[price->120], type->long, quantity->3],
	portfolioPosition[asset -> bond[price -> 120], type -> long, quantity -> 3],
	TestID->"FinanceTest-MkPosition"
]

Test[
	Payoff[MkPortfolioPosition[asset->MkBond[price->120], type->short, quantity->3]],
	-360,
	TestID->"FinanceTest-PositionPayoff"
]

(*
Test[
	MkSecurity[Position->long, Quantity->2, Asset->{}]
	,
	{"position" -> "long", "quantity" -> 2, "asset" -> {}}
	,
	TestID->"FinanceTest-MkSecurity"
]

Test[
	MkSecurity[Quantity->2, Asset->{}]
	,
	Indeterminate
	,
	Message[Message::name, "Position not specified"]
	,
	TestID->"FinanceTest-MkSecurity-NoPosition"
]

Test[
	MkSecurity[Position -> "long", Quantity->2]
	,
	Indeterminate
	,
	Message[Message::name, "Asset not specified"]
	,
	TestID->"FinanceTest-MkSecurity-NoAsset"
]

Test[
	MkSecurity[Position -> "unknown", Quantity->2, Asset->{}]
	,
	Indeterminate
	,
	Message[Message::name, "Wrong position. Should be long or short"]
	,
	TestID->"FinanceTest-MkSecurity-WrongPosition"
]

Test[
	MkOption[Position->long, Type->call, Strike->120]
	,
	{"position" -> "long", "quantity" -> 1., "asset" -> {"class" -> "option", "type" -> "call", "strike" -> 120}}
	,
	TestID->"FinanceTest-MkOption"
]

Test[
	Payoff[MkOption[Position->long, Type->call, Strike->100], S]
	,
	1. * Max[0, -100 + S],
	TestID->"FinanceTest-OptionPayoff1"
]

Test[
	Payoff[MkOption[Position->long, Type->put, Strike->100], S]
	,
	1. * Max[0, 100 - S],
	TestID->"FinanceTest-OptionPayof2"
]

Test[
	Payoff[MkOption[Position->short, Type->call, Strike->100], S]
	,
	-1. * Max[0, -100 + S],
	TestID->"FinanceTest-OptionPayoff1"
]

Test[
	Payoff[MkOption[Position->short, Type->put, Strike->100], S]
	,
	-1. * Max[0, 100 - S]
	,
	TestID->"FinanceTest-OptionPayof2"
]

Test[
	PortfolioPayoff[{MkOption[Position->short, Type->put, Strike->100], MkOption[Position->long, Type->call, Strike->100]}, S]
	,
	-1. * Max[0, 100 - S] + 1. * Max[0, -100 + S]
	,
	TestID->"FinanceTest-PortfolioPayoff"
]

Test[
	MkBond[Position->long, Price->120]
	,
	{"position" -> "long", "quantity" -> 1., "asset" -> {"class" -> "bond", "price" -> 120}}
	,
	TestID->"FinanceTest-MkBond"
]

Test[
	Payoff[MkBond[Position->long, Price->120], S]
	,
	120.
	,
	TestID->"FinanceTest-BondPayoff"
]

Test[
	BinomialPrice[{MkOption[Position->long, Type->call, Strike->100]}, 100, 4, 0.1, 1/3, 0.2]
	,
	6.13447246797594
	,
	TestID->"FinanceTest-BinomialPrice"
]

*)				