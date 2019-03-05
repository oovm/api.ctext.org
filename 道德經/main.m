(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];


(* ::Chapter:: *)
(*Chapters*)


Block[
	{chapters},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	chapters={
{"Chapter","Routing","Token"},
{"\:9053\:5fb7\:7d93","dao-de-jing","ctp:dao-de-jing"}
};
Export["Chapter.CSV", chapters]
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


\[ScriptH]
