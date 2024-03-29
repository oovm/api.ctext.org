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
	{links, format, chapters},
	links = Cases[
		Import["https://ctext.org/zhuangzi/zh", {"HTML", "XMLObject"}],
		XMLElement["a", {_, "href" -> h_}, {c_}] :> If[
			StringStartsQ[h, "zhuangzi"],
			{c, StringTake[h, ;; -4]},
			Nothing
		],
		Infinity
	];
	format[{c_, h_}, {pc_, ph_}] := <|
		"Chapter" -> StringJoin[pc, "|", c],
		"Routing" -> StringJoin[ph, "/", Last@StringSplit[h, "/"]],
		"Token" -> "ctp:" <> h
	|>;
	chapters = Flatten@{
		format[#, {"\:838a\:5b50|\:5167\:7bc7", "zhuangzi/inner-chapters"}]& /@ links[[4 ;; 10]],
		format[#, {"\:838a\:5b50|\:5916\:7bc7", "zhuangzi/outer-chapters"}]& /@ links[[12 ;; 26]],
		format[#, {"\:838a\:5b50|\:96dc\:7bc7", "zhuangzi/miscellaneous-chapters"}]& /@ links[[28 ;; 38]]
	};
	Export["Chapter.CSV", Dataset@chapters]
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
