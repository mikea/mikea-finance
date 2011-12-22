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

Decapitalize[str_] := 
  ToLowerCase[StringTake[str, 1]] <> StringDrop[str, 1]; 


ConstructorName[className_] := "Mk" <> Capitalize[className];
Constructor[className_] := Symbol[ConstructorName[className]];

HeadName[className_] := Decapitalize[className];


DefineConstructor[className_] := Module[
	{
		constructor = Constructor[className]
	},
	Evaluate[constructor][attrs___] := Evaluate[Symbol[HeadName[className]]][attrs]
];
		
DefineAccessor[className_, attrName_] := Module[
	{
		headName = Symbol[HeadName[className]],
		accessor = Symbol[Capitalize[attrName]]
	},
	Evaluate[headName] /: Evaluate[accessor][Evaluate[headName][attrs___]] := Evaluate[Symbol[attrName]] /. {attrs}
];

MkClass[className_, attrs_:{}] := (
	DefineConstructor[className];
	DefineAccessor[className, #]& /@ attrs;
	);
	
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