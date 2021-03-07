(* ::Package:: *)

(* ::Title:: *)
(*Queue-SDP-OOP*)


(* ::Subsubsection:: *)
(*Preamble*)


(* ::Input::Initialization:: *)
ResourceFunction[ResourceObject[Association["Name" -> "DarkMode", "ShortName" -> "DarkMode", "UUID" -> "6ae9b15e-dd80-4d11-be6e-434bf9ac9265", "ResourceType" -> "Function", "Version" -> "2.0.0", "Description" -> "Restyle notebooks into dark mode", "RepositoryLocation" -> URL["https://www.wolframcloud.com/objects/resourcesystem/api/1.0"], "SymbolName" -> "FunctionRepository`$f2abd2063089401aafe135eb354a8d92`DarkMode", "FunctionLocation" -> CloudObject["https://www.wolframcloud.com/obj/91755122-26ae-43f1-8e41-4043472dcf8a"]], ResourceSystemBase -> Automatic]];
SetOptions[SelectedNotebook[],PrintingStyleEnvironment->"Printout",ShowSyntaxStyles->True]
ClearAll[Evaluate[ToString[Context[]]<>"*"]]


(* ::Section:: *)
(*Definitions-OOP*)


(* ::Subsection:: *)
(*Related Symbols*)


(* ::Input:: *)
(*Names["*Process*"];*)


(* ::Input:: *)
(*Names["*Distribution*"];*)


(* ::Input:: *)
(*MapThread[Construct,{{f,g,h},{a,b,c}}]*)


(* ::Subsection::Closed:: *)
(*Algebra*)


(* ::Input:: *)
(*ClearAll[Algebra];*)
(*Options[Algebra]={"MultTable"->None,"Generators"->None,"Dimension"->None};*)
(*Algebra[ops:OptionsPattern[]]:=Algebra[canonicalizeAlgebraData[ops]];*)
(*(*do the minimum work necessary to make sure all the data for the Algebra is there*)*)
(*canonicalizeAlgebraData[ops:OptionsPattern[]]:=Association[ops];*)


(* ::Input:: *)
(*(*make some validators so you can always be sure you have a valid algebra without constantly having to check it*)validateAlgebraData[a_Association]:=Length[a]>0;(*reimplement this*)Algebra[a_Association]?NotAlgebraQ:=System`Private`HoldSetValid[Algebra[a]]/;validateAlgebraData[a];*)
(*AlgebraQ[a_Algebra]:=System`Private`HoldValidQ[a];*)
(*AlgebraQ[_]:=False;*)
(*AlgebraQ[s_Symbol]:=(Head[s]===Algebra&&AlgebraQ[Evaluate[s]]);*)
(*AlgebraQ~SetAttributes~HoldFirst;*)
(*NotAlgebraQ[a_]:=Not[AlgebraQ[a]];*)
(*NotAlgebraQ~SetAttributes~HoldFirst;*)


(* ::Input:: *)
(*(*define formatting if you want to*)Format[Algebra[a_]?AlgebraQ,StandardForm]:=RawBoxes@BoxForm`ArrangeSummaryBox[Algebra,Algebra[a],None,{"Put summary info here"},{},StandardForm]*)


(* ::Input:: *)
(*(*define some accessors/methods on your alebgra*)Algebra[a_]?AlgebraQ[k_]:=Lookup[a,k];(*general lookup*)(g:Algebra[a_]?AlgebraQ)["Generators"]:=getAlgebraicGenerators[g];*)
(*(g:Algebra[a_]?AlgebraQ)["Dimensions"]:=getAlgebraDimension[g];*)


(* ::Input:: *)
(*(*define some overloads for your algebra*)Algebra/:Dimensions[a_Algebra?AlgebraQ]:=a["Dimensions"];*)
(*Algebra/:a_Algebra?AlgebraQ[[el_]]:=AlgebraicElement[a,el];(*getting elements*)AlgebraicElement/:NonCommutativeMultiply[AlgebraicElement[a_Algebra?AlgebraQ,el1_],AlgebraicElement[a_Algebra?AlgebraQ,el2_]]:=getAlgebraProduct[a,{el1,el2}];*)


(* ::Input:: *)
(*(*allow for natural modifications of the algebraic structure*)mutateAlgebra[Algebra[a_]?AlgebraQ,changes_Association]:=Algebra[Join[changes,a]];*)
(*mutateAlgebra[a_Algebra,changes_]:=mutateAlgebra[a,Association@changes]*)
(*algebraMutationHandler~SetAttributes~HoldAllComplete;*)
(*algebraMutationHandler[AssociateTo[s_Symbol?AlgebraQ,stuff_]]:=(s=mutateAlgebra[s,stuff]);*)
(*algebraMutationHandler[Set[s_Symbol?AlgebraQ[key_],val_]]:=(s=mutateAlgebra[s,key->val]);*)
(*Language`SetMutationHandler[Algebra,algebraMutationHandler];*)


(* ::Input:: *)
(*(*implement the core algebra calculations some other way*)getAlgebraGenerators[a_Algebra?AlgebraQ]:="Generator, TBD";*)
(*getAlgebraDimension[a_Algebra?AlgebraQ]:="Dimension, TBD";*)
(*getAlgebraProduct[a_Algebra?AlgebraQ,{el1_,el2_}]:="Product, TBD";*)


(* ::Input:: *)
(*a=Algebra["MultTable"->{}]*)
(*(*Out:Algebra[<|"MultTable"\[Rule]{}|>]*)*)


(* ::Subsection:: *)
(*Mine*)


(* ::Input::Initialization:: *)
ClearAll["Global`*"]


(* ::Text:: *)
(*Note : I will follow the paradigm suggested in this post. I propose to implement two wrappers: DistributionMomentTruncation and ProcessMomentTruncation. *)


(* ::Subsubsection:: *)
(*Clearing definitions*)


(* ::Input::Initialization:: *)
ClearAll[DistributionMomentTruncation];
ClearAll[$DistributionDomainCanonicalizer,$DistributionDomainStylizer,$DistributionMomentTruncationSummaryThumbnail];
ClearAll[canonicalizeDistributionMomentTruncation,validateDistributionMomentTruncation,instantiateDistributionMomentTruncation]
ClearAll[DistributionMomentTruncationQ,NotDistributionMomentTruncationQ]
ClearAll[ProcessMomentTruncation,QueueMomentTruncation];
Options[DistributionMomentTruncation]={(*This allow for setting default values; *)
"TruncationOrder"->None,(*major parameter*)
"OriginalDistribution"->None,(*if supplied, the moment sequence is always generated using the moment function of the original distribution; there may be need for memoisation*)

(*"IndependentMargins"\[Rule]None,(*If this is true, MomentData is represented by a matrix only; only meaningful for multi-dimensional distributions, should be set to None for 1-d distributions, and is deleted if IdenticalMargins is True.*)
"IdenticalMargins"\[Rule]None,(*If this is true, MomentData can be represented as a vector; only meaningful for multi-dimensional distributions should be set to None for 1-d distributions.*)*)
"MarginalProperty"->None,
"MomentForm"->"Moment",(*following the specification of MomentConvert, absolute, factorial, central moments and cumulants; may also support truncated probability sequence for *)
"MomentData"->None,(*an association (is this really a good idea?) of the moments, with (lists of) non negative integers as keys; an all-zero key can be used to denote an alternative unitisation (a single zero can be used as a shorthand). not instantiated if there is an original distribution.*)(*I decide that we should only support two types of moment data; see "MomentDataShape" below*)
"MomentDataShape"->None,(*allowed types are "Full", "Overall" and "Function"; "Full" should be assumed. If IndependentMargins is True, this specification is ignored; only meaningful for multi-dimensional distributions, should be set to None for 1-d distributions. not instantiated if there is an original distribution.*)
(*"Dimensions"\[Rule]None,(*is this really needed?*)*)
"Domain"->None(*"DistributionDomain"\[Rule]None,*)(*These two should always be synonymous; the latter should not be stored, but only handled in interfaces. We must handle conversions between "domains" and "intervals/spans"*)
(*,"SummaryThumbnail"\[Rule]None*)(*Default to a plot of the moment matched polynomial; if there is an original distribution, it will also be plotted so that their difference is clearly seen. When any of these are hard to evaluate, a default thumbnail will be shown; this may should not be part of the structure, since it is expensive to store.*)
};


(* ::Subsubsection:: *)
(*Initializers*)


(* ::ItemNumbered:: *)
(*From a distribution*)


(* ::Input::Initialization:: *)
DistributionMomentTruncation[dist_?DistributionParameterQ]:=dist
DistributionMomentTruncation[dist_?DistributionParameterQ,type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant"][r_]:=Symbol[type][dist,r]

DistributionMomentTruncation[trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",ops:OptionsPattern[]][dist_?DistributionParameterQ]:=DistributionMomentTruncation[trunc,dist,type,ops]
DistributionMomentTruncation[trunc_Integer?Positive|trunc_Symbol,dist_?DistributionParameterQ,type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",ops:OptionsPattern[]]:=DistributionMomentTruncation[trunc,dist,"MomentForm"->type,Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentForm"|"MomentData"|"MomentDataShape"]]]
DistributionMomentTruncation[trunc_Integer?Positive|trunc_Symbol,dist_?DistributionParameterQ,ops:OptionsPattern[]]:=DistributionMomentTruncation["TruncationOrder"->trunc,"OriginalDistribution"->dist,Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"MomentDataShape"]]]
DistributionMomentTruncation[Infinity,dist_?DistributionParameterQ,ops:OptionsPattern[]]:=dist(*when a distribution is not moment truncated, it gets cast into the original distribution*)


(* ::ItemNumbered:: *)
(*From a moment function*)


(* ::Input::Initialization:: *)
DistributionMomentTruncation[trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,moments_Function|moments_Symbol,type:"Moment"|"FactorialMoment"|"CentralMoment"|"Cumulant":"Moment",ops:OptionsPattern[]]:=DistributionMomentTruncation[
trunc,moments,"MomentForm"->type,Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentForm"|"MomentData"|"MomentDataShape"]]]
DistributionMomentTruncation[trunc_Integer?Positive|trunc_Symbol|trunc:Infinity,moments_Function|moments_Symbol,ops:OptionsPattern[]]:=DistributionMomentTruncation["TruncationOrder"->trunc,"MomentData"->moments,"MomentDataShape"->"Function",Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"MomentDataShape"]]]


(* ::ItemNumbered:: *)
(*From a moment array*)


(* ::Input::Initialization:: *)
DistributionMomentTruncation[moments_?VectorQ,ops:OptionsPattern[{"Domain"->Interval[{-\[Infinity],\[Infinity]}],DistributionMomentTruncation}]]:=DistributionMomentTruncation["TruncationOrder"->Length[moments],"MomentData"->moments,"Domain"->OptionValue["Domain"],If[!MatchQ[OptionValue["Domain"],_Interval|_Span](*one dimensional*),"MarginalProperty"->"Identical",Unevaluated@Sequence[]],Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]](*default to a 1-d distribution*)
DistributionMomentTruncation[moments_?MatrixQ,ops:OptionsPattern[{"Domain"->None,DistributionMomentTruncation}]]:=(*This is for independent margins*)Module[{domain=OptionValue["Domain"]},
If[SquareMatrixQ[moments]&&Not@MatchQ[OptionValue["MarginalProperty"],"Identical"|"Independent"]&&
(OptionValue["MomentDataShape"]==="Full"||
(MatchQ[domain,{_Interval|_Span,_Interval|_Span}]&&Length[moments]>2)),(*handle the case when the distribution happens to be 2-d*)
If[domain===None,domain=Table[Interval[{-\[Infinity],\[Infinity]}],2]];
DistributionMomentTruncation[
"TruncationOrder"->Length[moments]-1(*note this*),"MomentData"->moments,"Domain"->domain,"MarginalProperty"->None,Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
],
If[domain===None,domain=Table[Interval[{-\[Infinity],\[Infinity]}],Length[moments]]];
DistributionMomentTruncation[
"TruncationOrder"->Length[moments],"MomentData"->moments,"Domain"->domain,"MarginalProperty"->"Independent",Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
]
]
]
DistributionMomentTruncation[moments_?ArrayQ,ops:OptionsPattern[{"Domain"->None,DistributionMomentTruncation}]]:=(*currently, only "MomentDataShape"\[Rule]"Full" is supported*)
Module[{domain=If[OptionValue["Domain"]===None,Table[Interval[{-\[Infinity],\[Infinity]}],ArrayDepth[moments]]]},
DistributionMomentTruncation[
"TruncationOrder"->Length[moments]-1(*note this*),"MomentData"->moments,"MomentDataShape"->"Full","Domain"->domain,"MarginalProperty"->None,Sequence@@FilterRules[{ops},Except["TruncationOrder"|"OriginalDistribution"|"MomentData"|"Domain"|"MarginalProperty"]]
]
]
DistributionMomentTruncation[ops:OptionsPattern[]]:=DistributionMomentTruncation[canonicalizeDistributionMomentTruncation[ops]]


(* ::Item:: *)
(*Canonicalize the Truncation*)


(* ::Input::Initialization:: *)
(*do the minimum work necessary to make sure all the data for the DistributionMomentTruncation is there*)
DistributionMomentTruncation::nocanon="Cannot construct a valid DistributionMomentTruncation from the given options `1`.";
DistributionMomentTruncation::noentry="Cannot construct a valid DistributionMomentTruncation because the entry `1` is not provided and cannot be inferred.";
DistributionMomentTruncation::noimplm="The required feature `1` is not implemented yet.";
$DistributionDomainCanonicalizer=Dispatch@{
Reals->Interval[{-\[Infinity],\[Infinity]}],Integers->(-\[Infinity];;\[Infinity]),
NonNegativeReals->Interval[{0,\[Infinity]}],NonPositiveReals->Interval[{-\[Infinity],0}],
NonNegativeIntegers->(0;;\[Infinity]),NonPositiveIntegers->(-\[Infinity];;0),
PositiveIntegers->(1;;\[Infinity]),NegativeIntegers->(-\[Infinity];;-1)};
canonicalizeDistributionMomentTruncation[ops:OptionsPattern[DistributionMomentTruncation]]:=Which[
Length@{ops}<1,Message[DistributionMomentTruncation::nocanon,{ops}];$Failed,
OptionValue["MomentDataShape"]==="Function"&&(OptionValue["Domain"]===None),Message[DistributionMomentTruncation::noentry,{"Domain"}];$Failed,
True,Module[{truncdata={ops}},
If [OptionValue["MomentDataShape"]==="Overall",
Message[DistributionMomentTruncation::noimplm,"MomentDataShape"->"Overall"];AppendTo[truncdata,"MomentDataShape"->"Full"]];
AppendTo[truncdata,"Domain"->OptionValue["Domain"]/.$DistributionDomainCanonicalizer];
If [DistributionParameterQ[OptionValue["OriginalDistribution"]],AppendTo[truncdata,"Domain"->DistributionDomain[OptionValue["OriginalDistribution"]]]];
AppendTo[truncdata,"MomentForm"->OptionValue["MomentForm"]];
Sort@Association[truncdata]
]
]


(* ::Subsubsection:: *)
(*Validators*)


(* ::Input::Initialization:: *)
(*make some validators so you can always be sure you have a valid DistributionMomentTruncation without constantly having to check it*)validateDistributionMomentTruncation[assoc_Association]:=And[
Length[assoc]>0,KeyMemberQ["TruncationOrder"]
](*reimplement this*)
DistributionMomentTruncation[assoc_Association]?NotDistributionMomentTruncationQ:=System`Private`HoldSetValid[DistributionMomentTruncation[assoc]]/;validateDistributionMomentTruncation[assoc];
DistributionMomentTruncationQ[distrlx_DistributionMomentTruncation]:=System`Private`HoldValidQ[distrlx];
DistributionMomentTruncationQ[_]:=False;
DistributionMomentTruncationQ[symbol_Symbol]:=(Head[symbol]===DistributionMomentTruncation&&DistributionMomentTruncationQ[Evaluate[symbol]]);
DistributionMomentTruncationQ~SetAttributes~HoldFirst;
NotDistributionMomentTruncationQ[distrlx_]:=Not[DistributionMomentTruncationQ[distrlx]];
NotDistributionMomentTruncationQ~SetAttributes~HoldFirst;


(* ::Input::Initialization:: *)
instantiateDistributionMomentTruncation[distrlx_DistributionMomentTruncation,ops:OptionsPattern[]]:=Missing["NotAvailable"](*Default to na\[IDoubleDot]ve polynomial moment matching; possible alternatives including orthogonal polynomials, piecewise-constant (histogram), point-masses, smooth-kernel distributions.*)


(* ::Subsubsection:: *)
(*Accessors*)


(* ::Input::Initialization:: *)
DistributionMomentTruncation::excdtrnc="The \!\(\*SuperscriptBox[\(`1`\), \(th\)]\) moment exceeds the order of the truncation.";
DistributionMomentTruncation[a_Association]["Moment"][0]:=1
DistributionMomentTruncation[a_Association]["Moment"][r___]/;KeyMemberQ[a,"OriginalDistribution"]:=
(If[Max[r]>a["TruncationOrder"],Message[DistributionMomentTruncation::excdtrnc,r]];Moment[a["OriginalDistribution"],r])
DistributionMomentTruncation[a_Association]["Moment"][r___]/;(a["MomentDataShape"]==="Function"):=
(If[Max[r]>a["TruncationOrder"],Message[DistributionMomentTruncation::excdtrnc,r]];a["MomentData"][r])
DistributionMomentTruncation[a_Association]["Moment"][{r:Repeated[_Integer?Positive,{SequenceCount[a["Domain"],_Interval|_Span]}]}]/;(MatchQ[a,KeyValuePattern["MarginalProperty"->None]]):=(If[If[a["MomentDataShape"]==="Overall",Total,Max][{r}]>a["TruncationOrder"],
Message[DistributionMomentTruncation::excdtrnc,r];Missing["Indeterminate"],
a["MomentData"][[r]]])
DistributionMomentTruncation[a_Association]["Moment"][{r:Repeated[_Integer?Positive,{SequenceCount[a["Domain"],_Interval|_Span]}]}]/;(MatchQ[a,KeyValuePattern["MarginalProperty"->"Independent"]]):=(If[Max[r]>a["TruncationOrder"],
Message[DistributionMomentTruncation::excdtrnc,r];Missing["Indeterminate"],
Times@@MapThread[Construct,{Extract/@{r},a["MomentData"]}]])
DistributionMomentTruncation[a_Association]["Moment"][{r:Repeated[_Integer?Positive,{SequenceCount[a["Domain"],_Interval|_Span]}]}]/;(MatchQ[a,KeyValuePattern["MarginalProperty"->"Identical"]]):=(If[Max[r]>a["TruncationOrder"],
Message[DistributionMomentTruncation::excdtrnc,r];Missing["Indeterminate"],
Times@@a["MomentData"][{r}]])
DistributionMomentTruncation[a_Association]["Moment"][r_Integer?Positive]/;MatchQ[a,KeyValuePattern["Domain"->(_Interval|_Span)]]:=
DistributionMomentTruncation[a]["Moment"][{r}]
DistributionMomentTruncation[a_Association]["Properties"]:=Sort@Keys[a]
DistributionMomentTruncation[a_Association][key___]:=a[key]


(* ::Subsubsection:: *)
(*Formatting*)


(* ::Input::Initialization:: *)
(*define formatting if you want to*)
$DistributionMomentTruncationSummaryThumbnail=DensityPlot[1-Exp[-5 (y-(.2+0.5E^(-8 (x+.5)^2)+1.0E^(-10 (x-.3)^2)))^2],{x,-1.,1.},{y,0,2},PlotRange->{{-1.,1.},{0.,2.}},AspectRatio->1,Frame->None,PlotTheme->"Monochrome"];
$DistributionDomainStylizer=Dispatch[Reverse/@Normal[$DistributionDomainCanonicalizer]];
SyntaxInformation[DistributionMomentTruncation]={"ArgumentsPattern"->{___,OptionsPattern[]},"OptionNames"->ToString/@First/@Options[DistributionMomentTruncation]};
Format[DistributionMomentTruncation[a_Association]?DistributionMomentTruncationQ,StandardForm]:=Block[{},
RawBoxes@BoxForm`ArrangeSummaryBox[DistributionMomentTruncation,DistributionMomentTruncation[a],$DistributionMomentTruncationSummaryThumbnail,{
{BoxForm`MakeSummaryItem[{"TruncationOrder"<>": ",a["TruncationOrder"]},StandardForm],
BoxForm`MakeSummaryItem[{"Domain"<>": ",a["Domain"]/.$DistributionDomainStylizer},StandardForm]},
If[KeyMemberQ[a,"MomentForm"]&&a["MomentForm"]=!="Moment",
{BoxForm`MakeSummaryItem[{"MomentForm"<>": ",a["MomentForm"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]],
If[KeyMemberQ[a,"OriginalDistribution"],
{BoxForm`MakeSummaryItem[{"OriginalDistribution"<>": ",a["OriginalDistribution"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]],
If[KeyMemberQ[a,"MarginalProperty"]&&a["MarginalProperty"]=!=None,
{BoxForm`MakeSummaryItem[{"MarginalProperty"<>": ",a["MarginalProperty"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]]
},{
If[a["MomentForm"]==="Moment",
{BoxForm`MakeSummaryItem[{"MomentForm"<>": ",a["MomentForm"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]],
If[KeyMemberQ[a,"MomentDataShape"],
{BoxForm`MakeSummaryItem[{"MomentDataShape"<>": ",a["MomentDataShape"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]],
If[KeyMemberQ[a,"MomentData"],
{BoxForm`MakeSummaryItem[{"MomentData"<>": ",Short@a["MomentData"]},StandardForm],SpanFromLeft},Unevaluated@Sequence[]]
},StandardForm,"Interpretable"->Automatic]
]


(* ::Input:: *)
(*DistributionMomentTruncation[s,s,]*)
