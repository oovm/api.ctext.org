(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Auxiliary Functions*)


MapMonitor = ResourceFunction["DynamicMap"];
badge = StringTemplate["
![Chapters](https://img.shields.io/badge/Chapters-`Chapter`-brightgreen.svg)
![Characters](https://img.shields.io/badge/Character-`Character`-blue.svg)
![Zipf](https://img.shields.io/badge/Zipf-`Zipf`-blueviolet.svg)
"];


(* ::Chapter::Closed:: *)
(*Chapters*)


Block[
	{format, $tasks, name = {"\:897f\:904a\:8a18", "xiyouji"}},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	format[h_, c_] := If[
		StringStartsQ[h, Last@name] && StringEndsQ[h, "zh"],
		<|
			"Chapter" -> StringReplace[StringJoin[First@name, "|" , c], "\:3000" -> " "],
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


(* ::Chapter::Closed:: *)
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
	read[record_Association] := (
		PrintTemporary[record@"Chapter"];
		<|
			"Chapter" -> record@"Chapter",
			"Traditional" -> askT[record@"Token"]["fulltext"],
			"Simplified" -> askS[record@"Token"]["fulltext"]
		|>
	);
	data = MapMonitor[read, chapters][[2]];
	Export["data.json", Flatten@data, "RawJSON"]
];



(* ::Chapter:: *)
(*Summary*)


Block[
	{i = 0.9, json, data, text, rank},
	data = Import["data.json", "RawJSON"];
	text = StringDelete[
		StringJoin@Flatten[#["Traditional"]& /@ data],
		StringPartition["\:ff0c\:3001\:3002\:ff1f\:ff01\:ff1b\:ff1a\:300c\:300d\:300f\:300e", 1]
	];
	rank = Reverse@SortBy[Tally@StringSplit[text, ""], Last];
	If[
		FileExistsQ@"WordCloud.png",
		Nothing,
		yun = WordCloud[
			(#2 + RandomReal[{-i, 1 / (1 - i)}]) -> #1& @@@ rank,
			PlotTheme -> "Monochrome", ImageSize -> {16, 9} * 80, FontFamily -> "\:6977\:4f53", AspectRatio -> 9 / 16
		];
		Export["WordCloud.png", yun, Background -> None]
	];
	If[
		FileExistsQ@"WordCloud.png",
		Nothing,
		bar = RectangleChart[
			Association[Style[#1, FontFamily -> "\:6977\:4f53", FontSize -> 14] -> {1, #2}& @@@ Reverse@Take[rank, UpTo@25]],
			BarOrigin -> Left, ChartLabels -> Automatic, BarSpacing -> None,
			TargetUnits -> {"Minutes", "Feet"}, AxesLabel -> Automatic, Ticks -> True, PlotTheme -> "Detailed",
			PerformanceGoal -> "Speed", LabelingFunction -> (Placed[Last@#, Right]&),
			ColorFunction -> Function[{width, height}, ColorData["TemperatureMap"][height]],
			ImageSize -> {16, 9} * 80, AspectRatio -> 9 / 16
		];
		Export["WordFrequency.png", bar, Background -> None]
	];
	readme = StringRiffle[
		{
			"# [\:897f\:6e38\:8bb0](https://ctext.org/xiyouji/zh)",
			badge[<|
				"Chapter" -> Length@data,
				"Character" -> StringLength@StringJoin@Flatten[#["Traditional"]& /@ data],
				"Zipf" -> N[s /. FindDistributionParameters[Last /@ rank, ZipfDistribution[s]], 6]
			|>],
			"![WordCloud](https://github.com/GalAster/api.ctext.org/blob/master/\:897f\:904a\:8a18/WordCloud.png?raw=true)"
		},
		"\n"
	];
	Export["Readme.md", readme,"Text"]
];
