(* Mathematica Package *)

(* Created by the Wolfram Workbench Aug 25, 2011 *)

BeginPackage["Finance`"]
(* Exported symbols added here with SymbolName::usage *) 
MkOption::usage = 
"MkOption[Position->long|short, Type->call|put, Strike->strike_price, Quantity->quantity]
	Creates new option object.";

MkBond::usage =
"MkBond[Position->long|short, Price->price, Quantity->quantity]
	Creates new bond object.";

Payoff::usage = 
"Payoff[security, assetprice]
	Calculates security payoff.";

PortfolioPayoff::usage = 
"PortfolioPayoff[portfolio, assetprice]
	Calculates portfolio payoff.";

MkSecurity::usage = "MkSecurity"

Begin["`Private`"];
(* Implementation of the package *)

Options[MkSecurity] = {Position->"", Quantity->1.0, Asset->""}
MkSecurity[OptionsPattern[]] :=
	Module[{position=ToString[OptionValue[Position]], asset=OptionValue[Asset], quantity = OptionValue[Quantity]},
	    Which[
	        position === "", Message["Position not specified"]; Indeterminate,
	        position =!= "long" && position =!= "short", Message["Wrong position. Should be long or short"]; Indeterminate,
	        asset === "", Message["Asset not specified"]; Indeterminate,
	        True, {
		        "position" -> position,
		        "quantity" -> quantity,
		        "asset" -> asset
		    }]];

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

End[]

EndPackage[]

