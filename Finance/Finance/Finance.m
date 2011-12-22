BeginPackage["Finance`", { "OOP`"}]

MkOption::usage = "";
option::usage ="";

MkBond::usage = "";
bond::usage ="";

MkPortfolioPosition::usage = "";
portfolioPosition::usage ="";

Payoff::usage = "";


PortfolioPayoff::usage =
"PortfolioPayoff[portfolio, assetprice]
	Calculates portfolio payoff.";

MkSecurity::usage = "MkSecurity"

BinomialPrice::usage = "BinomialPrice[S_, steps_, r_, expiry_, s_, portfolio_]";

Begin["`Private`"];

(****************** Securities ******************)


(* Bonds *)

MkClass["Bond", {"price"}];
Payoff[aBond_bond, ___] := Price[aBond];

(* Options *)
MkClass["Option", {"strike", "underlying", "type"}];

Payoff[anOption_option, prices__] := Module[{
		price = Underlying[anOption] /. prices
	}, 
	Switch[ToString[Type[anOption]],
		"call", Max[0, price - Strike[anOption]],
		"put", Max[0, Strike[anOption] - price]]
];

(* Portfolio position *)

MkClass["PortfolioPosition", {"asset", "quantity", "type"}];

Payoff[pos_portfolioPosition, prices___] := 
	Payoff[Asset[pos]] * Quantity[pos] *
	Switch[ToString[Type[pos]],
		"long", 1,
		"short", -1,
		_, Indeterminate];

(*
CSecurity := MkClass["security"]

Options[MkSecurity] = {Position->"", Quantity->1.0, Asset->""}
MkSecurity[OptionsPattern[]] :=
	Module[{position=ToString[OptionValue[Position]], asset=OptionValue[Asset], quantity = OptionValue[Quantity]},
	    Which[
	        position === "", Message["Position not specified"]; Indeterminate,
	        position =!= "long" && position =!= "short", Message["Wrong position. Should be long or short"]; Indeterminate,
	        asset === "", Message["Asset not specified"]; Indeterminate,
	        True, NewInstance[CSecurity, {
		        "position" -> position,
		        "quantity" -> quantity,
		        "asset" -> asset
		    }]]];

COption := MkClass["option", Parent->CSecurity]

MkOption[opts:OptionsPattern[Options[MkSecurity]~Join~{Type->"call", Strike->""}]] :=
	Module[{type=ToString[OptionValue[Type]], strike=OptionValue[Strike]},
		Which[
			(* TODO: error checking. *)
			True, MkSecurity[FilterRules[{opts},Options[MkSecurity]] ~Join~ {Asset->{"class" -> "option", "type"->type, "strike"->strike}}]
		]];

MkBond[opts:OptionsPattern[Options[MkSecurity]~Join~{Price->""}]] :=
	Module[{price=OptionValue[Price]},
		Which[
			(* TODO: error checking. *)
			True, MkSecurity[FilterRules[{opts},Options[MkSecurity]] ~Join~ {Asset->{"class" -> "bond", "price"->price}}]
		]];

GetValue[option_, key_] := Replace[key, FilterRules[option,{key}][[1]]];

OptionPayoff[option_, price_] :=
	Switch[GetValue[option, "type"],
		"call", Max[0, price - GetValue[option, "strike"]],
		"put", Max[0, GetValue[option, "strike"] - price]];

BondPayoff[bond_, price_] := GetValue[bond, "price"];

Payoff[security_, price_]:=
	(* position *) Switch[GetValue[security, "position"],	"long", 1,"short", -1] *
	(* quantity *) GetValue[security, "quantity"] *
	Switch[GetValue[GetValue[security, "asset"], "class"],
		"option", OptionPayoff[GetValue[security, "asset"], price],
		"bond", BondPayoff[GetValue[security, "asset"], price]
		];

PortfolioPayoff[portfolio_, price_]:=Fold[Plus, 0, Payoff[#, price]& /@portfolio ];
*)

(* Binomial Price *)

(* 
U[m_, s_, dt_] := 1/2 (E^(-m*dt) + E^((m + s^2) dt)) + 1/2 Sqrt[(E^(-m*dt) + E^((m + s^2) dt))^2 - 4];
V[m_, s_, dt_] := 1/U[m, s, dt];
P[m_, s_, dt_] := Module[{u = U[m, s, dt], v = V[m, s, dt]}, (E^(m*dt) - v)/(u - v)];

  
AssetPrices[S_, steps_, u_, v_] := Table[S * (u^(i - 1)) * (v^(steps - i + 1)), {i, steps + 1}];
 
OptionPrices[S_, steps_, u_, v_, payoffFunction_] := payoffFunction /@ AssetPrices[S, steps, u, v];   

ReducePrices[optionPrices_, p_, r_, dt_] := Table[
	((1 - p) *optionPrices[[i]] + p*optionPrices[[i + 1]])*E^(-r*dt), 
  	{i, Length[optionPrices] - 1}]

BinomialPrice[portfolio_, S_, steps_, r_, expiry_, s_] := 
 Module[{dt, u, v, p, payoffFunction},
  dt = expiry/steps;
  u = U[r, s, dt];
  v = V[r, s, dt];
  p = P[r, s, dt];
  payoffFunction = PortfolioPayoff[portfolio, #]&; 
  First[NestWhile[
    ReducePrices[#, p, r, dt] &,
    OptionPrices[S, steps, u, v, payoffFunction],
    True,
    steps + 1 ]]
  ];

*)

End[]

EndPackage[]

