(* ::Package:: *)

Block[
	{format, $tasks},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	format[h_, c_] := If[
		StringStartsQ[h, "analects"] && StringEndsQ[h, "zh"],
		<|"Chapter" -> "\:8ad6\:8a9e|" <> c, "Routing" -> StringTake[h, ;; -4]|>,
		Nothing
	];
	$tasks = Cases[
		Import["https://ctext.org/analects/zh", {"HTML", "XMLObject"}],
		XMLElement["a", {"shape" -> "rect", "href" -> h_}, {c_}] :> format[h, c],
		Infinity
	];
	Export["Chapter.CSV", Dataset@$tasks]
];
