(* Mathematica Test File *)

CShape = MkClass["shape", Attributes -> {"color" -> "red"}]
CSquare = MkClass["square", Parent -> CShape, Attributes -> {"side"}]
CCircle = MkClass["circle", Parent -> CShape, Attributes -> {"radius"}]
 
 c1 = NewInstance[CCircle, {"radius" -> 1.0}]
 s1 = NewInstance[CSquare, {"side" -> 2.0}]
 c2 = NewInstance[CCircle, {"radius" -> 3.0, "color" -> "blue"}]
 
 
Test[
	c1 @ "radius"
	,
	1.0
	,
	TestID->"OOPTest-Attr1"
]

Test[
	s1 @ "side"
	,
	2.0
	,
	TestID->"OOPTest-Attr2"
]

Test[
	c2 @ "radius"
	,
	3.0
	,
	TestID->"OOPTest-Attr3"
]
