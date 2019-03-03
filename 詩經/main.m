(* ::Package:: *)

(* ::Chapter:: *)
(*Settings*)


SetDirectory@NotebookDirectory[];
$wait = 0.1;


(* ::Chapter:: *)
(*Functions*)


(* ::Chapter:: *)
(*Chapters*)


Block[
	{$base, ask, getSubsections, getTitle, root, expand0, expand1, expand2, $tasks},
	If[FileExistsQ@"Chapter.CSV", Return[Nothing]];
	$base[str_] := "https://api.ctext.org/gettext?urn=" <> str;
	ask[url_] := Check[
		Pause@RandomReal[{$wait, 10$wait}];
		Import[$base@url, "RawJSON"],
		ask[url]
	];
	getSubsections[link_Association] := Block[
		{json, format},
		json = ask@link["url"];
		format = <|
			"title" -> StringJoin[link["title"], "|", json["title"]],
			"route" -> StringJoin[link["route"], "/", Last@StringSplit[#, "/"]],
			"url" -> #
		|>&;
		format /@ json["subsections"]
	];
	getTitle[link_Association] := Block[
		{json, format},
		json = ask@link["url"];
		Pause@RandomReal[{$wait, 2$wait}];
		<|
			"Chapter" -> StringJoin[link["title"], "|", json["title"]],
			"Routing" -> link["route"],
			"Token" -> link["url"]
		|>
	];
	root = Import[$base@"ctp:book-of-poetry", "RawJSON"];
	expand0 = <|"title" -> root["title"], "route" -> StringTake[#, 5 ;;], "url" -> #|>& /@ root["subsections"];
	expand1 = Flatten[getSubsections /@ expand0];
	expand2 = Flatten[getSubsections /@ expand1];
	$tasks = Flatten[getTitle /@ expand2];
	Export["Chapter.CSV", Dataset@$tasks]
];


(* ::Chapter:: *)
(*Content*)
