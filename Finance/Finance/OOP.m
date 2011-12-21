(* Mathematica Package *)

BeginPackage["OOP`"]

MkClass::usage =
"MkClass[name, Parent->parent, Attributes->{attributes}]
	Creates new class object.";

NewInstance::usage =
"NewInstance[class, {attributes}]
    Creates new class instance.";

Begin["`Private`"] (* Begin Private Context *) 

(****************** OOP Layer ******************)

On[Assert];

Capitalize[str_] := 
  ToUpperCase[StringTake[str, 1]] <> StringDrop[str, 1]; 

DefineConstructor[name_] := Module[
	{
		constructorName="Mk" <> name
	},
	SetDelayed[
		Evaluate[Symbol[constructorName]][attrs___], 
		Evaluate[Symbol[name]][attrs]
		]
];
		
DefineAccessor[className_, name_] := Module[
	{
		class = Symbol[className],
		accessor = Symbol[Capitalize[name]]
	},
	SetDelayed[
		Evaluate[accessor][Evaluate[class][attrs___]], 
		Evaluate[Symbol[name]] /. attrs
	]
];


MkClass[name_, attrs_:{}] := (
	DefineConstructor[name];
	DefineAccessor[name, #]& /@ attrs;
	)
(*
Options[MkClass] = {Parent -> Null, Attributes -> {}};
MkClass[name_, OptionsPattern[]] := {
   "name" -> name,
   "parent" -> OptionValue[Parent], 
   "attributes" -> OptionValue[Attributes]};

Attr[object_, key_] := Module[
   {rules = FilterRules[object, {key}]},
   Assert[rules != {}];
   ({key} /. rules)[[1]]
   ];

NewInstance[class_, options_] := Module[
  {},
  Function[
   {attr}, 
   Attr[options, attr]]
  ]
*)

End[] (* End Private Context *)

EndPackage[]