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
	{format, getTitle, chapters},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	format[link_, chapter_, contant_] := Block[
		{},
		If[!StringContainsQ[link, "book-of-poetry"], Return[Nothing]];
		If[chapter == "\:8a69\:7d93", Return[Nothing]];
		Cases[contant,
			XMLElement["a", {__, "href" -> h_}, {c_}] :> <|
				"chapter" -> StringJoin["\:8a69\:7d93|", chapter, "|", c],
				"routing" -> StringJoin[StringTake[link, ;; -3], StringSplit[h, "/"][[2]]],
				"url" -> "https://ctext.org/" <> h
			|>,
			Infinity
		]
	];
	getTitle[chapter_Association] := Cases[
		Import[chapter["url"], {"HTML", "XMLObject"}],
		XMLElement["a", {"shape" -> "rect", "class" -> "popup", "href" -> h_}, {c_}] :> <|
			"Chapter" -> StringJoin[chapter["chapter"], "|", c],
			"Routing" -> StringJoin[chapter["routing"], "/", StringSplit[h, "/"][[2]]],
			"Token" -> "ctp:" <> StringTake[h, ;; -4]
		|>,
		Infinity
	];
	chapters = Cases[
		Import["https://ctext.org/book-of-poetry/zh", {"HTML", "XMLObject"}],
		XMLElement[
			"span",
			{"class" -> "menuitem container"},
			{
				XMLElement["a", {__, "href" -> link_}, {chapter_}],
				__,
				XMLElement["span", {"class" -> "subcontents"}, contant_]
			}
		] :> format[link, chapter, contant],
		Infinity
	];
	chapters[[-1]] = MapAt[
		StringRiffle[Insert[StringSplit[#, "|"], "\:980c", 2], "|"]&,
		chapters[[-1]],
		{All, 1}
	];
	chapters = Flatten@{
		chapters,
		format["book-of-poetry/odes-of-the-temple-and-the-altar/zh", "\:980c", {
			XMLElement["a", {__, "href" -> "book-of-poetry/praise-odes-of-lu/zh"}, {"\:9b6f\:980c"}],
			XMLElement["a", {__, "href" -> "book-of-poetry/sacrificial-odes-of-shang/zh"}, {"\:5546\:980c"}]
		}]
	};
	Export["Chapter.CSV", Dataset@Flatten[MapMonitor[getTitle, chapters][[2]]]]
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
