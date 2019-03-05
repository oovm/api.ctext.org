(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Functions*)


MapMonitor = ResourceFunction["DynamicMap"];


(* ::Chapter:: *)
(*Chapters*)


Block[
	{format, $tasks},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	format[h_, c_] := If[
		StringStartsQ[h, "analects"] && StringEndsQ[h, "zh"],
		<|
			"Chapter" -> "\:8ad6\:8a9e|" <> c,
			"Routing" -> StringTake[h, ;; -4],
			"Token" -> "ctp:" <> StringTake[h, ;; -4]
		|>,
		Nothing
	];
	$tasks = Cases[
		Import["https://ctext.org/analects/zh", {"HTML", "XMLObject"}],
		XMLElement["a", {"shape" -> "rect", "href" -> h_}, {c_}] :> format[h, c],
		Infinity
	];
	Export["Chapter.CSV", Dataset@$tasks]
];



(* ::Chapter:: *)
(*Content*)


download[url_, chapter_] := GeneralUtilities`Scope[
	zh = Import["https://ctext.org/analects/" <> url <> "/zh", {"HTML", "XMLObject"}];
	zhs = Import["https://ctext.org/analects/" <> url <> "/zhs", {"HTML", "XMLObject"}];
	simplified = Cases[
		zhs,
		XMLElement["td", {__, "class" -> "ctext"}, {__, text_}] :> text,
		Infinity
	];
	explanation = Cases[
		zhs,
		XMLElement["td", {__, "class" -> "mctext"}, {"\n", _, text_, _}] :> text,
		Infinity
	];
	traditional = Cases[
		zh,
		XMLElement["td", {__, "class" -> "ctext"}, {__, text_}] :> text,
		Infinity
	];
	If[
		! Equal[Length /@ {simplified, traditional, explanation}],
		Print["Error"]
	];
	<|
		"Chapter" -> chapter <> "|" <> url,
		"Paragraphs" -> Table[
			<|
				"Simplified" -> simplified[[i]],
				"Traditional" -> traditional[[i]],
				"Explanation" -> explanation[[i]]
			|>,
			{i, Length@simplified}
		]
	|>
];
download[#url, #chapter]& /@ $tasks
