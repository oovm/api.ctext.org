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


(* ::Chapter::Closed:: *)
(*Chapters*)


Block[
	{links, format, chapters},
	links = Cases[
		Import["https://ctext.org/chu-ci/zh", {"HTML", "XMLObject"}],
		XMLElement["a", {_, "href" -> h_}, {c_}] :> If[
			StringStartsQ[h, "chu-ci"],
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
		format[links[[1]], {"\:695a\:8fad", "chu-ci"}],
		format[links[[14]], {"\:695a\:8fad", "chu-ci"}],
		format[#, {"\:695a\:8fad", "chu-ci"}]& /@ links[[25 ;; 32]],
		format[links[[41]], {"\:695a\:8fad", "chu-ci"}],
		format[#, {"\:695a\:8fad|\:4e03\:8aeb", "chu-ci/qi-jian"}]& /@ links[[34 ;; 40]],
		format[#, {"\:695a\:8fad|\:4e5d\:6b4c", "chu-ci/jiu-ge"}]& /@ links[[3 ;; 13]],
		format[#, {"\:695a\:8fad|\:4e5d\:7ae0", "chu-ci/jiu-zhang"}]& /@ links[[16 ;; 24]],
		format[#, {"\:695a\:8fad|\:4e5d\:61f7", "chu-ci/jiu-huai"}]& /@ links[[43 ;; 51]],
		format[#, {"\:695a\:8fad|\:4e5d\:6b4e", "chu-ci/jiu-tan"}]& /@ links[[53 ;; 61]],
		format[#, {"\:695a\:8fad|\:4e5d\:601d", "chu-ci/jiu-si"}]& /@ links[[63 ;; 71]]
	};
	Export["Chapter.CSV", Dataset@chapters]
];


(* ::Chapter::Closed:: *)
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


(* ::Chapter:: *)
(*Summary*)


reader = Encoding`UTF8;
json = ImportString[FromCharacterCode@ToCharacterCode[#, "UTF-8"], "RawJSON"]&;


text=json@reader@GetString[Normal@ReadByteArray["data.json"]];


chapter=StringDelete[StringJoin[First[text]["Traditional"]],{"\:ff0c","\:3002"}];


chapter


WordCloud


WordCloud[StringSplit[chapter,""],ImageSize->{16,9}*20,FontFamily->"\:6977\:4f53"]
