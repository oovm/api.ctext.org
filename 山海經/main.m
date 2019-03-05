(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Chapters*)


Block[
	{format, $tasks, name = {"\:5c71\:6d77\:7d93", "shan-hai-jing"}},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	format[h_, c_] := If[
		StringStartsQ[h, Last@name] && StringEndsQ[h, "zh"],
		<|
			"Chapter" -> StringJoin[First@name, "|" , c],
			"Routing" -> StringTake[h, ;; -4],
			"Token" -> "ctp:" <> StringTake[h, ;; -4]
		|>,
		Nothing
	];
	$tasks = Cases[
		Import["https://ctext.org/" <> Last@name <> "/zh", {"HTML", "XMLObject"}],
		XMLElement["a", {"shape" -> "rect", "href" -> h_}, {c_}] :> format[h, c],
		Infinity
	];
	Export["Chapter.CSV", Dataset@$tasks]
];


(* ::Chapter:: *)
(*Content*)


Block[
	{$wait = 10, chapters, askS, askT, read},
	If[FileExistsQ@"data.json", Return[Nothing]];
	chapters = Normal@Import["Chapter.CSV", {"CSV", "Dataset"}, "HeaderLines" -> 1];
	askS[url_String] := Block[
		{ask = Import["https://api.ctext.org/gettext?if=zh&remap=gb&urn=" <> url, "RawJSON"]},
		If[!ListQ@ask["fulltext"], Pause@RandomReal[$wait];askS[url], ask]
	];
	askT[url_String] := Block[
		{ask = Import["https://api.ctext.org/gettext?if=zh&urn=" <> url, "RawJSON"]},
		If[!ListQ@ask["fulltext"], Pause@RandomReal[$wait];askS[url], ask]
	];
	read[record_Association] := <|
		"Chapter" -> record@"Chapter",
		"Traditional" -> askT[record@"Token"]["fulltext"],
		"Simplified" -> askS[record@"Token"]["fulltext"]
	|>;
	data = Flatten@Map[read, chapters];
	Export["data.json", data, "RawJSON"]
];



