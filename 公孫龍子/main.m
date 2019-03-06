(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Chapters*)


Block[
	{format, $tasks, name = {"\:516c\:5b6b\:9f8d\:5b50", "gongsunlongzi"}},
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


MapMonitor = ResourceFunction["DynamicMap"];
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
	data = MapMonitor[read, chapters][[2]];
	Export["data.json", Flatten@data, "RawJSON"]
];
