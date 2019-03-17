(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Auxiliary Functions*)


<< NETLink`;
InstallNET[];
LoadNETType["System.Text.Encoding"];
MapMonitor = ResourceFunction["DynamicMap"];


(* ::Chapter:: *)
(*Chapters*)


Block[
	{format, $tasks, name = {"\:5b6b\:5b50\:5175\:6cd5", "art-of-war"}},
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
	{$wait = 10, reader, chapters, ask, json, askS, askT, read},
	If[FileExistsQ@"data.json", Return[Nothing]];
	reader = Encoding`UTF8;
	json = ImportString[FromCharacterCode@ToCharacterCode[#, "UTF-8"], "RawJSON"]&;
	chapters = Normal@Import["Chapter.CSV", {"CSV", "Dataset"}, "HeaderLines" -> 1];
	askS[url_String] := Block[
		{link = "https://api.ctext.org/gettext?if=zh&remap=gb&urn=" <> url},
		ask = json@reader@GetString[Normal@URLRead[link, "BodyByteArray"]];
		If[!ListQ@ask["fulltext"], Pause@RandomReal[$wait];askS[url], ask]
	];
	askT[url_String] := Block[
		{link = "https://api.ctext.org/gettext?if=zh&urn=" <> url},
		ask = json@reader@GetString[Normal@URLRead[link, "BodyByteArray"]];
		If[!ListQ@ask["fulltext"], Pause@RandomReal[$wait];askT[url], ask]
	];
	read[record_Association] := <|
		"Chapter" -> record@"Chapter",
		"Traditional" -> askT[record@"Token"]["fulltext"],
		"Simplified" -> askS[record@"Token"]["fulltext"]
	|>;
	data = MapMonitor[read, chapters][[2]];
	Export["data.json", Flatten@data, "RawJSON"]
];
