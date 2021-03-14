(* ::Package:: *)

(* ::Title:: *)
(*Queue-SDP-OOP*)


(* ::Author:: *)
(* Author: Gravifer *)
(* Date: 2021-03-08 *)
(* Version: 0.1-alpha.3 *)


Package["QueueSDP`"] (*Using new-style package specification*)
(* ClearAll[Evaluate[Context[] <> "*"]] *)
ClearAll[DistributionMomentTruncation];
ClearAll[$DistributionDomainCanonicalizer,
         $DistributionDomainStylizer,
         $DistributionMomentTruncationSummaryThumbnail];
ClearAll[canonicalizeDistributionMomentTruncation,
             validateDistributionMomentTruncation,
          instantiateDistributionMomentTruncation]
ClearAll[DistributionMomentTruncationQ,
      NotDistributionMomentTruncationQ]


PackageExport["DistributionMomentTruncation"]
PackageExport["DistributionMomentTruncationQ"]
PackageExport["NotDistributionMomentTruncationQ"]


PackageScope["$DistributionDomainCanonicalizer"]
PackageScope["$DistributionDomainStylizer"]
PackageScope["$DistributionMomentTruncationSummaryThumbnail"]
PackageScope["canonicalizeDistributionMomentTruncation"]
PackageScope["validateDistributionMomentTruncation"]
PackageScope["instantiateDistributionMomentTruncation"]


(* ::Section:: *)
(*Messages*)


(* ::Subsection:: *)
(*Usage*)


(* ::Subsection:: *)
(*Diagnose*)


DistributionMomentTruncation::nocanon="Cannot construct a valid DistributionMomentTruncation from the given options `1`.";
DistributionMomentTruncation::noentry="Cannot construct a valid DistributionMomentTruncation because the entry `1` is not provided and cannot be inferred.";
DistributionMomentTruncation::noimplm="The required feature `1` is not implemented yet.";
DistributionMomentTruncation::excdtrnc="The `1`-th moment exceeds the order of the truncation.";


(* ::Section:: *)
(*Definitions*)


Options[DistributionMomentTruncation] = {(*This allow for setting default values; when initializing a truncation, unaccessed entries should be deleted*)
  "TruncationOrder"      -> None, (*positive integer; major parameter*)
  "OriginalDistribution" -> None, (*if supplied, the moment sequence is always generated using the moment function of the original distribution; there may be need for memoisation*)
  "MarginalProperty"     -> None, (* None|"Independent"|"Identical" *)
  "MomentForm" -> "Moment", (* "Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant", following the specification of MomentConvert;*)(* TODO: may also support truncated probability sequence in the future*)
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


DistributionMomentTruncation[dist_?DistributionParameterQ]:=dist
DistributionMomentTruncation[
  dist_?DistributionParameterQ,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant"
][r_]:=Symbol[type][dist,r]


(* ::SubItemNumbered:: *)
(*Valid truncation specification*)


DistributionMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
][dist_?DistributionParameterQ] := DistributionMomentTruncation[trunc, dist, type, ops]

DistributionMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol,
  dist_?DistributionParameterQ,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
] := DistributionMomentTruncation[trunc, dist,
  "MomentForm" -> type,
  Sequence@@FilterRules[{ops}, Except["TruncationOrder"|"OriginalDistribution"|"MomentForm"|"MomentData"|"MomentDataShape"]]
]

DistributionMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol,
  dist_?DistributionParameterQ,
  ops:OptionsPattern[]
] := DistributionMomentTruncation[
  "TruncationOrder" -> trunc,
  "OriginalDistribution" -> dist,
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"MomentDataShape"]]
]


(* ::SubItemNumbered:: *)
(*Infinite truncation casts to the original distribution*)


DistributionMomentTruncation[Infinity, dist_?DistributionParameterQ, ops:OptionsPattern[]]:=dist


(* ::ItemNumbered:: *)
(*From a moment function*)


DistributionMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  moments_Function|moments_Symbol,
  type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",
  ops:OptionsPattern[]
] := DistributionMomentTruncation[trunc, moments,
  "MomentForm" -> type,
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentForm"|"MomentData"|"MomentDataShape"]]
]
DistributionMomentTruncation[
  trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,
  moments_Function|moments_Symbol,
  ops:OptionsPattern[]
] := DistributionMomentTruncation[
  "TruncationOrder" -> trunc,
  "MomentData" -> moments,
  "MomentDataShape" -> "Function",
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"MomentDataShape"]]
]


(* ::ItemNumbered:: *)
(*From a moment array*)


DistributionMomentTruncation[
  moments_?VectorQ,
  ops:OptionsPattern[{
    "Domain" -> Interval[{-\[Infinity], \[Infinity]}], (*default to a 1-d distribution*)
    DistributionMomentTruncation
  }]
] := DistributionMomentTruncation[
  "TruncationOrder" -> Length[moments],
  "MomentData" -> moments,
  "Domain" -> OptionValue["Domain"],
  If[ MatchQ[ OptionValue["Domain"], _Interval|_Span], (*one dimensional or not*)
    Unevaluated@Sequence[],
    "MarginalProperty" -> "Identical"
  ],
  Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
]

DistributionMomentTruncation[
  moments_?MatrixQ,(*This is primarily for independent margins*)
  ops:OptionsPattern[{
    "Domain"->None,
    DistributionMomentTruncation
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
    ]; DistributionMomentTruncation[
      "TruncationOrder" -> Length[moments]-1, (*note this*)
      "MomentData" -> moments,
      "Domain" -> domain,
      "MarginalProperty" -> None,
      Sequence@@FilterRules[{ops}, Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
    ], (*otherwise, it must be the "MarginalProperty"->"Independent" case*)
    If[domain===None, 
      domain = Table[Interval[{-\[Infinity], \[Infinity]}], Length[moments]]
    ]; DistributionMomentTruncation[
      "TruncationOrder" -> Length[moments],
      "MomentData" -> moments,
      "Domain" -> domain,
      "MarginalProperty" -> "Independent",
      Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
    ]
  ]
]

DistributionMomentTruncation[
  moments_?ArrayQ, (*currently, only "MomentDataShape"->"Full" is supported*)
  ops:OptionsPattern[{
    "Domain"->None,
    DistributionMomentTruncation
  }]
] := Module[{domain = If[OptionValue["Domain"]===None,
              Table[Interval[{-\[Infinity], \[Infinity]}],
              ArrayDepth[moments]]
              ]},
  DistributionMomentTruncation[
    "TruncationOrder" -> Length[moments]-1, (*note this*)
    "MomentData"  -> moments,
    "MomentDataShape" -> "Full",
    "Domain" -> domain,
    "MarginalProperty" -> None,
    Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
  ]
]

DistributionMomentTruncation[ops:OptionsPattern[]] := DistributionMomentTruncation[canonicalizeDistributionMomentTruncation[ops]]


(* ::Item:: *)
(*Canonicalize the Truncation*)


$DistributionDomainCanonicalizer=Dispatch@{
  Reals               -> Interval[{-\[Infinity],  \[Infinity]}],
  Integers            ->         ( -\[Infinity];; \[Infinity] ),
  NonNegativeReals    -> Interval[{ 0          ,  \[Infinity]}],
  NonPositiveReals    -> Interval[{-\[Infinity],  0          }],
  NonNegativeIntegers ->         (  0          ;; \[Infinity] ),
  NonPositiveIntegers ->         ( -\[Infinity];; 0           ),
  PositiveIntegers    ->         (  1          ;; \[Infinity] ),
  NegativeIntegers    ->         ( -\[Infinity];;-1           ) };

canonicalizeDistributionMomentTruncation[ops:OptionsPattern[DistributionMomentTruncation]]:=Which[
  Length@{ops}<1,
    Message[DistributionMomentTruncation::nocanon,{ops}];
    $Failed,
  OptionValue["MomentDataShape"]==="Function" && (OptionValue["Domain"]===None),
    Message[DistributionMomentTruncation::noentry,{"Domain"}];
    $Failed,
  True,
    Module[{truncdata={ops}},
      If[OptionValue["MomentDataShape"]==="Overall",
        Message[DistributionMomentTruncation::noimplm, "MomentDataShape" -> "Overall"];
        AppendTo[truncdata, "MomentDataShape" -> "Full"]
      ];
      AppendTo[truncdata, "Domain" -> OptionValue["Domain"]/.$DistributionDomainCanonicalizer];
      If[DistributionParameterQ[OptionValue["OriginalDistribution"]],
        AppendTo[truncdata, "Domain" -> DistributionDomain[OptionValue["OriginalDistribution"]]]
      ];
      AppendTo[truncdata, "MomentForm" -> OptionValue["MomentForm"]];
      Sort@Association[truncdata]
    ]
]


(* ::Subsubsection:: *)
(*Validators*)


(*#TODO: make some validators so you can always be sure you have a valid DistributionMomentTruncation without constantly having to check it*)
validateDistributionMomentTruncation[assoc_Association]:=And[
  Length[assoc] > 0,
  Match[ assoc, KeyValuePattern["TruncationOrder"->Integer?Positive]],
  Match[ assoc, KeyValuePattern["Domain"         ->Except[None]    ]]
](*reimplement this*)

DistributionMomentTruncation[assoc_Association]?NotDistributionMomentTruncationQ :=
  System`Private`HoldSetValid[DistributionMomentTruncation[assoc]]/;validateDistributionMomentTruncation[assoc];

DistributionMomentTruncationQ[distrlx_] := System`Private`HoldValidQ[distrlx];
DistributionMomentTruncationQ[_] := False;
DistributionMomentTruncationQ[symbol_Symbol] := (
  Head[symbol]===DistributionMomentTruncation
  && DistributionMomentTruncationQ[Evaluate[symbol]]
);
DistributionMomentTruncationQ~SetAttributes~HoldFirst;

NotDistributionMomentTruncationQ[distrlx_] := Not[DistributionMomentTruncationQ[distrlx]];
NotDistributionMomentTruncationQ~SetAttributes~HoldFirst;



instantiateDistributionMomentTruncation[
  distrlx_DistributionMomentTruncation,
  ops:OptionsPattern[]
] := Missing["NotAvailable"] (*#TODO: Default to naive polynomial moment matching; possible alternatives including orthogonal polynomials, piecewise-constant (histogram), point-masses, smooth-kernel distributions.*)


(* ::Subsubsection:: *)
(*Accessors*)


DistributionMomentTruncation[a_Association]["Moment"][0]:=1
DistributionMomentTruncation[a_Association]["Moment"][r___] /; KeyMemberQ[a,"OriginalDistribution"] := (
  If[Max[r] > a["TruncationOrder"], Message[DistributionMomentTruncation::excdtrnc, r]];
  Moment[a["OriginalDistribution"], r]
)

DistributionMomentTruncation[a_Association]["Moment"][r___] /; (a["MomentDataShape"]==="Function")  := (
  If[Max[r] > a["TruncationOrder"], Message[DistributionMomentTruncation::excdtrnc, r]];
  a["MomentData"][r]
)

DistributionMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive, 
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a,KeyValuePattern["MarginalProperty"->None]]) := (
  If[If[a["MomentDataShape"]==="Overall", 
        Total,
        Max
      ][{r}] > a["TruncationOrder"],
    Message[DistributionMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    a["MomentData"][[r]]
  ]
)

DistributionMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive,
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a, KeyValuePattern["MarginalProperty"->"Independent"]]) := (
  If[Max[r] > a["TruncationOrder"],
    Message[DistributionMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    Times@@MapThread[Construct, {
      Extract/@{r},
      a["MomentData"]
    }]
  ]
)

DistributionMomentTruncation[a_Association]["Moment"][
  {r:Repeated[_Integer?Positive,
      {SequenceCount[a["Domain"], _Interval|_Span]}
    ]}
] /; (MatchQ[a, KeyValuePattern["MarginalProperty"->"Identical"]]) := (
  If[Max[r]>a["TruncationOrder"],
    Message[DistributionMomentTruncation::excdtrnc, r]; Missing["Indeterminate"],
    Times@@a["MomentData"][{r}]
  ]
)

DistributionMomentTruncation[a_Association]["Moment"][r_Integer?Positive] /; MatchQ[a, KeyValuePattern["Domain"->(_Interval|_Span)]] :=
  DistributionMomentTruncation[a]["Moment"][{r}]

DistributionMomentTruncation[a_Association]["Properties"] := Sort@Keys[a]
DistributionMomentTruncation[a_Association][key___] := a[key]


(* ::Subsubsection:: *)
(*Formatting*)


$DistributionMomentTruncationSummaryThumbnail = 
  DensityPlot[1-Exp[-5 (y-(.2+0.5E^(-8 (x+.5)^2)+1.0E^(-10 (x-.3)^2)))^2], {x,-1.,1.}, {y,0,2},
    PlotRange -> {{-1.,1.},{0.,2.}},
    AspectRatio -> 1,
    Frame -> None,
    PlotTheme -> "Monochrome"
  ];
$DistributionDomainStylizer = Dispatch[Reverse/@Normal[$DistributionDomainCanonicalizer]];

SyntaxInformation[DistributionMomentTruncation] = {
  "ArgumentsPattern" -> {___,OptionsPattern[]},
  "OptionNames" -> ToString/@First/@Options[DistributionMomentTruncation]
};

Format[DistributionMomentTruncation[a_Association]?DistributionMomentTruncationQ, StandardForm] := Block[{},
  RawBoxes@BoxForm`ArrangeSummaryBox[
    DistributionMomentTruncation,
    DistributionMomentTruncation[a],
    $DistributionMomentTruncationSummaryThumbnail,
    { { BoxForm`MakeSummaryItem[{"TruncationOrder"     <>": ", a["TruncationOrder"]                    }, StandardForm],
        BoxForm`MakeSummaryItem[{"Domain"              <>": ", a["Domain"]/.$DistributionDomainStylizer}, StandardForm]},
      If[KeyMemberQ[a,"MomentForm"]&&a["MomentForm"]=!="Moment",{
        BoxForm`MakeSummaryItem[{"MomentForm"          <>": ", a["MomentForm"]                         }, StandardForm], SpanFromLeft},
        Unevaluated@Sequence[]
      ],
      If[KeyMemberQ[a,"OriginalDistribution"],{
        BoxForm`MakeSummaryItem[{"OriginalDistribution"<>": ", a["OriginalDistribution"]               }, StandardForm], SpanFromLeft},
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

