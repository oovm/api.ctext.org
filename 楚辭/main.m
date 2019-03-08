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
		format[#, {"\:695a\:8fad|\:4e5d\:7ae0", "chu-ci/jiu-zhang"}]& /@ links[[15 ;; 24]],
		format[#, {"\:695a\:8fad|\:4e5d\:61f7", "chu-ci/jiu-huai"}]& /@ links[[42 ;; 51]],
		format[#, {"\:695a\:8fad|\:4e5d\:6b4e", "chu-ci/jiu-tan"}]& /@ links[[52 ;; 61]],
		format[#, {"\:695a\:8fad|\:4e5d\:601d", "chu-ci/jiu-si"}]& /@ links[[62 ;; 71]]
	};
	Export["Chapter.CSV", Dataset@chapters]
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
	data = Flatten@MapMonitor[read, chapters][[2]];
	Export["data.json", data, "RawJSON"]
];
