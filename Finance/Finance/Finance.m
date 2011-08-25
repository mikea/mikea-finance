(* Mathematica Package *)

(* Created by the Wolfram Workbench Aug 25, 2011 *)

BeginPackage["Finance`"]
(* Exported symbols added here with SymbolName::usage *) 
MkOption::usage = 
"MkOption[position, type, strike]
	Creates new option object.";

OptionPayoff::usage = 
"OptionPayoff[option, assetprice]
	Calculates option payoff.";

PortfolioPayoff::usage = 
"PortfolioPayoff[portfolio, assetprice]
	Calculates portfolio payoff.";

Begin["`Private`"];
(* Implementation of the package *)

MkOption[aposition_, atype_, astrike_] := 
	{"position"->ToString[aposition],"type"->ToString[atype],"strike"->astrike};

GetValue[option_, key_] := Replace[key, FilterRules[option,{key}][[1]]];

OptionPosition[option_] := Switch[GetValue[option, "position"],
	"long", 1,
	"short", -1];

OptionPayoff[option_, price_]:= 
	OptionPosition[option] * Switch[GetValue[option, "type"],
		"call", Max[0, price - GetValue[option, "strike"]],
		"put", Max[0, GetValue[option, "strike"] - price]];

PortfolioPayoff[portfolio_, price_]:=Fold[Plus, 0, OptionPayoff[#, price]& /@portfolio ];

End[]

EndPackage[]

