(* ::Package:: *)

(* ::Title:: *)
(*Queue-SDP-OOP*)


(* ::Author:: *)
(* Author: Gravifer *)
(* Date: 2021-02-21 *)
(* Version: 0.2.0 *)


BeginPackage["QueueSDP`"] (*Using new-style package specification*)
(* ClearAll[Evaluate[Context[] <> "*"]] *)
ClearAll[ProcessMomentTruncation];
ClearAll[$ProcessDomainCanonicalizer,
         $ProcessDomainStylizer,
         $ProcessMomentTruncationSummaryThumbnail];
ClearAll[canonicalizeProcessMomentTruncation,
             validateProcessMomentTruncation,
          instantiateProcessMomentTruncation]
ClearAll[ProcessMomentTruncationQ,
      NotProcessMomentTruncationQ]


PackageExport["ProcessMomentTruncation"]
PackageExport["ProcessMomentTruncationQ"]
PackageExport["NotProcessMomentTruncationQ"]


PackageScope["$ProcessDomainCanonicalizer"]
PackageScope["$ProcessDomainStylizer"]
PackageScope["$ProcessMomentTruncationSummaryThumbnail"]
PackageScope["canonicalizeProcessMomentTruncation"]
PackageScope["validateProcessMomentTruncation"]
PackageScope["instantiateProcessMomentTruncation"]


(* ::Section:: *)
(*Usage messages*)


ProcessMomentTruncation::nocanon="Cannot construct a valid ProcessMomentTruncation from the given options `1`.";
ProcessMomentTruncation::noentry="Cannot construct a valid ProcessMomentTruncation because the entry `1` is not provided and cannot be inferred.";
ProcessMomentTruncation::noimplm="The required feature `1` is not implemented yet.";
ProcessMomentTruncation::excdtrnc="The `1`-th moment exceeds the order of the truncation.";


(* ::Section:: *)
(*Definitions*)


Options[ProcessMomentTruncation] = {(*This allow for setting default values; when initializing a truncation, unaccessed entries should be deleted*)
  "TruncationOrder" -> None, (*positive integer; major parameter*)
  "OriginalProcess" -> None, (*if supplied, the moment sequence is always generated using the moment function of the original distribution; there may be need for memoisation*)
  "MarginalProperty"-> None, (* None|"Independent"|"Identical" *)
  "MomentForm"  -> "Moment", (* "Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant", following the specification of MomentConvert;*)(* TODO: may also support truncated probability sequence in the future*)
  "MomentData"      -> None,
  "MomentDataShape" -> None, (* "Full"|"Overall"|"Function"; "Full" should be assumed. Only meaningful for multi-dimensional distributions when "MarginalProperty" -> None.*)
  "Domain" -> None
};


(* ::Subsection:: *)
(*Initializers*)


(* ::ItemNumbered:: *)
(*From a distribution*)


(* ::SubItemNumbered:: *)
(*No truncation returns usual distribution computations*)


ProcessMomentTruncation[dist_?ProcessParameterQ]:=dist
ProcessMomentTruncation[
  dist_?ProcessParameterQ,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant"
][r_]:=Symbol[type][dist,r]


(* ::SubItemNumbered:: *)
(*Valid truncation specification*)


ProcessMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
][dist_?ProcessParameterQ] := ProcessMomentTruncation[trunc, dist, type, ops]

ProcessMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol,
  dist_?ProcessParameterQ,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
] := ProcessMomentTruncation[trunc, dist,
  "MomentForm" -> type,
  Sequence@@FilterRules[{ops}, Except["TruncationOrder"|"OriginalProcess"|"MomentForm"|"MomentData"|"MomentDataShape"]]
]

ProcessMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol,
  dist_?ProcessParameterQ,
  ops:OptionsPattern[]
] := ProcessMomentTruncation[
  "TruncationOrder" -> trunc,
  "OriginalProcess" -> dist,
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"MomentDataShape"]]
]


(* ::SubItemNumbered:: *)
(*Infinite truncation casts to the original distribution*)


ProcessMomentTruncation[Infinity, dist_?ProcessParameterQ, ops:OptionsPattern[]]:=dist


(* ::ItemNumbered:: *)
(*From a moment function*)


ProcessMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  moments_Function|moments_Symbol,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
] := ProcessMomentTruncation[trunc, moments,
  "MomentForm" -> type,
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentForm"|"MomentData"|"MomentDataShape"]]
]
ProcessMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  moments_Function|moments_Symbol,
  ops:OptionsPattern[]
] := ProcessMomentTruncation[
  "TruncationOrder" -> trunc,
  "MomentData" -> moments,
  "MomentDataShape" -> "Function",
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"MomentDataShape"]]
]


(* ::ItemNumbered:: *)
(*From a moment array*)


ProcessMomentTruncation[
  moments_?VectorQ,
  ops:OptionsPattern[{
    "Domain" -> Interval[{-\[Infinity], \[Infinity]}], (*default to a 1-d distribution*)
    ProcessMomentTruncation
  }]
] := ProcessMomentTruncation[
  "TruncationOrder" -> Length[moments],
  "MomentData" -> moments,
  "Domain" -> OptionValue["Domain"],
  If[ MatchQ[ OptionValue["Domain"], _Interval|_Span], (*one dimensional or not*)
    Unevaluated@Sequence[],
    "MarginalProperty" -> "Identical"
  ],
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"Domain"|"MarginalProperty"]]
]

ProcessMomentTruncation[
  moments_?MatrixQ,(*This is primarily for independent margins*)
  ops:OptionsPattern[{
    "Domain"->None,
    ProcessMomentTruncation
  }]
] := Module[{domain = OptionValue["Domain"]},
  If[SquareMatrixQ[moments]
      && Not@MatchQ[OptionValue["MarginalProperty"], "Identical"|"Independent"]
      && (OptionValue["MomentDataShape"]==="Full"
          || (MatchQ[domain, {_Interval|_Span, _Interval|_Span}]
              && Length[moments] > 2
            )
        ), (*handle the case when the distribution happens to be 2-d*)
    If[domain===None, 
      domain = Table[Interval[{-\[Infinity], \[Infinity]}], 2]
    ]; ProcessMomentTruncation[
      "TruncationOrder" -> Length[moments]-1, (*note this*)
      "MomentData" -> moments,
      "Domain" -> domain,
      "MarginalProperty" -> None,
      Sequence@@FilterRules[{ops}, Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"Domain"|"MarginalProperty"]]
    ], (*otherwise, it must be the "MarginalProperty"->"Independent" case*)
    If[domain===None, 
      domain = Table[Interval[{-\[Infinity], \[Infinity]}], Length[moments]]
    ]; ProcessMomentTruncation[
      "TruncationOrder" -> Length[moments],
      "MomentData" -> moments,
      "Domain" -> domain,
      "MarginalProperty" -> "Independent",
      Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"Domain"|"MarginalProperty"]]
    ]
  ]
]

ProcessMomentTruncation[
  moments_?ArrayQ, (*currently, only "MomentDataShape"->"Full" is supported*)
  ops:OptionsPattern[{
    "Domain"->None,
    ProcessMomentTruncation
  }]
] := Module[{domain = If[OptionValue["Domain"]===None,
              Table[Interval[{-\[Infinity], \[Infinity]}],
              ArrayDepth[moments]]
              ]},
  ProcessMomentTruncation[
    "TruncationOrder" -> Length[moments]-1, (*note this*)
    "MomentData"  -> moments,
    "MomentDataShape" -> "Full",
    "Domain" -> domain,
    "MarginalProperty" -> None,
    Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalProcess"|"MomentData"|"Domain"|"MarginalProperty"]]
  ]
]

ProcessMomentTruncation[ops:OptionsPattern[]] := ProcessMomentTruncation[canonicalizeProcessMomentTruncation[ops]]


(* ::Item:: *)
(*Canonicalize the Truncation*)


$ProcessDomainCanonicalizer=Dispatch@{
  Reals               -> Interval[{-\[Infinity],  \[Infinity]}],
  Integers            ->         ( -\[Infinity];; \[Infinity] ),
  NonNegativeReals    -> Interval[{ 0          ,  \[Infinity]}],
  NonPositiveReals    -> Interval[{-\[Infinity],  0          }],
  NonNegativeIntegers ->         (  0          ;; \[Infinity] ),
  NonPositiveIntegers ->         ( -\[Infinity];; 0           ),
  PositiveIntegers    ->         (  1          ;; \[Infinity] ),
  NegativeIntegers    ->         ( -\[Infinity];;-1           ) };

canonicalizeProcessMomentTruncation[ops:OptionsPattern[ProcessMomentTruncation]]:=Which[
  Length@{ops}<1,
    Message[ProcessMomentTruncation::nocanon,{ops}];
    $Failed,
  OptionValue["MomentDataShape"]==="Function" && (OptionValue["Domain"]===None),
    Message[ProcessMomentTruncation::noentry,{"Domain"}];
    $Failed,
  True,
    Module[{truncdata={ops}},
      If[OptionValue["MomentDataShape"]==="Overall",
        Message[ProcessMomentTruncation::noimplm, "MomentDataShape" -> "Overall"];
        AppendTo[truncdata, "MomentDataShape" -> "Full"]
      ];
      AppendTo[truncdata, "Domain" -> OptionValue["Domain"]/.$ProcessDomainCanonicalizer];
      If[ProcessParameterQ[OptionValue["OriginalProcess"]],
        AppendTo[truncdata, "Domain" -> ProcessDomain[OptionValue["OriginalProcess"]]]
      ];
      AppendTo[truncdata, "MomentForm" -> OptionValue["MomentForm"]];
      Sort@Association[truncdata]
    ]
]


(* ::Subsubsection:: *)
(*Validators*)


(*#TODO: make some validators so you can always be sure you have a valid ProcessMomentTruncation without constantly having to check it*)
validateProcessMomentTruncation[assoc_Association]:=And[
  Length[assoc] > 0,
  Match[ assoc, KeyValuePattern["TruncationOrder"->Integer?Positive]],
  Match[ assoc, KeyValuePattern["Domain"         ->Except[None]    ]]
](*reimplement this*)

ProcessMomentTruncation[assoc_Association]?NotProcessMomentTruncationQ :=
  System`Private`HoldSetValid[ProcessMomentTruncation[assoc]]/;validateProcessMomentTruncation[assoc];

ProcessMomentTruncationQ[distrlx_] := System`Private`HoldValidQ[distrlx];
ProcessMomentTruncationQ[_] := False;
ProcessMomentTruncationQ[symbol_Symbol] := (
  Head[symbol]===ProcessMomentTruncation
  && ProcessMomentTruncationQ[Evaluate[symbol]]
);
ProcessMomentTruncationQ~SetAttributes~HoldFirst;

NotProcessMomentTruncationQ[distrlx_] := Not[ProcessMomentTruncationQ[distrlx]];
NotProcessMomentTruncationQ~SetAttributes~HoldFirst;



instantiateProcessMomentTruncation[
  distrlx_ProcessMomentTruncation,
  ops:OptionsPattern[]
] := Missing["NotAvailable"] (*#TODO: Default to naive polynomial moment matching; possible alternatives including orthogonal polynomials, piecewise-constant (histogram), point-masses, smooth-kernel distributions.*)


(* ::Subsubsection:: *)
(*Accessors*)


ProcessMomentTruncation[a_Association]["Moment"][0]:=1
ProcessMomentTruncation[a_Association]["Moment"][r___] /; KeyMemberQ[a,"OriginalProcess"] := (
  If[Max[r] > a["TruncationOrder"], Message[ProcessMomentTruncation::excdtrnc, r]];
  Moment[a["OriginalProcess"], r]
)

ProcessMomentTruncation[a_Association]["Moment"][r___] /; (a["MomentDataShape"]==="Function")  := (
  If[Max[r] > a["TruncationOrder"], Message[ProcessMomentTruncation::excdtrnc, r]];
  a["MomentData"][r]
)

ProcessMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive, 
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a,KeyValuePattern["MarginalProperty"->None]]) := (
  If[If[a["MomentDataShape"]==="Overall", 
        Total,
        Max
      ][{r}] > a["TruncationOrder"],
    Message[ProcessMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    a["MomentData"][[r]]
  ]
)

ProcessMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive,
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a, KeyValuePattern["MarginalProperty"->"Independent"]]) := (
  If[Max[r] > a["TruncationOrder"],
    Message[ProcessMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    Times@@MapThread[Construct, {
      Extract/@{r},
      a["MomentData"]
    }]
  ]
)

ProcessMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive,
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a, KeyValuePattern["MarginalProperty"->"Identical"]]) := (
  If[Max[r]>a["TruncationOrder"],
    Message[ProcessMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    Times@@a["MomentData"][{r}]
  ]
)

ProcessMomentTruncation[a_Association]["Moment"][r_Integer?Positive] /; MatchQ[a, KeyValuePattern["Domain"->(_Interval|_Span)]] :=
  ProcessMomentTruncation[a]["Moment"][{r}]

ProcessMomentTruncation[a_Association]["Properties"] := Sort@Keys[a]
ProcessMomentTruncation[a_Association][key___] := a[key]


(* ::Subsubsection:: *)
(*Formatting*)


$ProcessMomentTruncationSummaryThumbnail = 
  DensityPlot[1-Exp[-5 (y-(.2+0.5E^(-8 (x+.5)^2)+1.0E^(-10 (x-.3)^2)))^2], {x,-1.,1.}, {y,0,2},
    PlotRange -> {{-1.,1.},{0.,2.}},
    AspectRatio -> 1,
    Frame -> None,
    PlotTheme -> "Monochrome"
  ];
$ProcessDomainStylizer = Dispatch[Reverse/@Normal[$ProcessDomainCanonicalizer]];

SyntaxInformation[ProcessMomentTruncation] = {
  "ArgumentsPattern" -> {___,OptionsPattern[]},
  "OptionNames" -> ToString/@First/@Options[ProcessMomentTruncation]
};

Format[ProcessMomentTruncation[a_Association]?ProcessMomentTruncationQ, StandardForm] := Block[{},
  RawBoxes@BoxForm`ArrangeSummaryBox[
    ProcessMomentTruncation,
    ProcessMomentTruncation[a],
    $ProcessMomentTruncationSummaryThumbnail,
    { { BoxForm`MakeSummaryItem[{"TruncationOrder"     <>": ", a["TruncationOrder"]                    }, StandardForm],
        BoxForm`MakeSummaryItem[{"Domain"              <>": ", a["Domain"]/.$ProcessDomainStylizer}, StandardForm]},
      If[KeyMemberQ[a,"MomentForm"]&&a["MomentForm"]=!="Moment",{
        BoxForm`MakeSummaryItem[{"MomentForm"          <>": ", a["MomentForm"]                         }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ],
      If[KeyMemberQ[a,"OriginalProcess"],{
        BoxForm`MakeSummaryItem[{"OriginalProcess"<>": ", a["OriginalProcess"]               }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ],
      If[KeyMemberQ[a,"MarginalProperty"] && a["MarginalProperty"]=!=None,{
        BoxForm`MakeSummaryItem[{"MarginalProperty"    <>": ", a["MarginalProperty"]                   }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ]
    }, {
      If[a["MomentForm"]==="Moment",{
        BoxForm`MakeSummaryItem[{"MomentForm"          <>": ", a["MomentForm"]                         }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ],
      If[KeyMemberQ[a,"MomentDataShape"],{
        BoxForm`MakeSummaryItem[{"MomentDataShape"     <>": ", a["MomentDataShape"]                    }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ],
      If[KeyMemberQ[a,"MomentData"],{
        BoxForm`MakeSummaryItem[{"MomentData"          <>": ", Short@a["MomentData"]                   }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ]
    },
  StandardForm, "Interpretable" -> Automatic]
]

