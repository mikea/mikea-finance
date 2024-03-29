MkClass["Square"];
MkClass["Circle", {"radius"}];
MkClass["Rectangle", {"a", "b"}];

Test[
	MkSquare[],
	square[],
	TestID->"OOPTest-Constructor-Square"
]

Test[
	MkCircle[],
	circle[],
	TestID->"OOPTest-Constructor-Circle"
]

Test[
	Radius[MkCircle[radius -> 1.0]],
	1.0,
	TestID->"OOPTest-AttrAccessor"
]
	
Test[
	MkRectangle[a -> 1.0, b -> 2.0],
	rectangle[a -> 1., b -> 2.],
	TestID->"OOPTest-MkRectangle"
]

Test[
	A[MkRectangle[a -> 1.0, b -> 2.0]],
	1.0,
	TestID->"OOPTest-MkRectangle-accesssor1"
]

Test[
	B[MkRectangle[a -> 1.0, b -> 2.0]],
	2.0,
	TestID->"OOPTest-MkRectangle-accesssor2"
]


(* CShape = MkClass["shape", Attributes -> {"color" -> "red"}]
CSquare = MkClass["square", Parent -> CShape, Attributes -> {"side"}]
CCircle = MkClass["circle", Parent -> CShape, Attributes -> {"radius"}]
 
MkClass 
*)

 (*
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
*)