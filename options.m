(* ::Package:: *)

BeginPackage["FinancialOptions`"];
Begin["`Private`"];

MkOption[aposition_, atype_, astrike_] := {position->aposition,type->atype,strike->astrike};

GetValue[option_, key_] := Replace[key, FilterRules[option,{key}][[1]]];

OptionPosition[option_] := Switch[GetValue[option, position],
	long, 1,
	short, -1];

OptionPayoff[option_, price_]:= OptionPosition[option] * Switch[GetValue[option, type],
	call, Max[0, price - GetValue[option, strike]],
	put, Max[0, GetValue[option, strike] - price]];

PortfolioPayoff[portfolio_, price_]:=Fold[Plus, 0, OptionPayoff[#, price]& /@portfolio ];

End[];
EndPackage[];
