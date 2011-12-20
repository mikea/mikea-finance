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

End[] (* End Private Context *)

EndPackage[]