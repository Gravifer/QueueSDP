(* ::Package:: *)

(* ::Title:: *)
(*queue-sdp*)


(* ::Author:: *)
(* Author: Gravifer *)
(* Date: 2021-02-21 *)
(* Version: 0.2.0 *)

QueueSDP`Private`ConstraintGeneration::usage="The script should be called from commandline with two arguments.\n"<>
  "Example:\n wolframscript -file queue-sdp-constraint-generation.wl -args K r"<>
  "\nAlternatively, use `Get` in a Wolfram session to execute the script.";
If[Length[$ScriptCommandLine] =!= 3, Message[QueueSDP`Private`ConstraintGeneration::usage]; Return[$Failed]];
If[  Rest[$ScriptCommandLine] =!= {"rm","-rf"}, 
  Return@DeleteDirectory[
    FileNameJoin[DirectoryName[$InputFileName],"cache"], DeleteContents->True
  ]
];


(* ::Section:: *)
(*Usage messages*)


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Dependencies*)


(* ResourceFunction["IntegerCompositions"] *)
IntegerCompositions[n_, k_] := Map[(
  Map[(#[[2]] - #[[1]] - 1)&, Partition[Join[{0}, #, {n + k}], 2, 1]]
  )&, Subsets[Range[n + k - 1], {k - 1}]];
IntegerCompositions::usage="Hard-embedded resource function; "<>
  "gives a list of all compositions of integer $n$ into $k$ parts in canonical order. "<>
  "The original resource function can be found at https://resources.wolframcloud.com/FunctionRepository/resources/IntegerCompositions"


(* ::Subsection:: *)
(*Basic functions*)


integerCompositions[n_, k_] := integerCompositions[n, k] = Reverse[IntegerCompositions[n, k]];
integerCompositions::usage="gives a list of all compositions of integer $n$ into $k$ parts in anti-canonical order."

edg2mat = Outer[Plus,#,#,1]&; (*using `Outer` prevents leaking from named variables in functions*)
(*equivalent form: ((row \[Function] (col \[Function] (row + col)) /@ #) /@ #) &;*)


(* ::Subsection:: *)
(*SDP cone-matrix*)


(* ::Text:: *)
(*For the sake of clarity, the usually known multi-index notation is hereafter called a vecponent (i.e., vector exponent).*)


K::usage = "number of queues";
r::usage = "rank of relaxation";
K = ToExpression@$ScriptCommandLine[[2]];
r = ToExpression@$ScriptCommandLine[[3]];
If[!MissingQ@PersistentValue[StringTemplate["K`1`r`2`"][K, r],
    PersistenceLocation["Local",
      LocalObject@FileNameJoin[DirectoryName[$InputFileName],"cache"]
    ]
  ],
  Return@PersistentValue[StringTemplate["K`1`r`2`"][K, r],
    PersistenceLocation["Local",
      LocalObject@FileNameJoin[DirectoryName[$InputFileName],"cache"]
    ]
  ]
]


\[Alpha]IC::usage = "concerned \[Alpha] vecponents";
\[Alpha]IC = Join @@ Table[
    integerCompositions[\[FormalR],   K], {\[FormalR], 0, 2 r}];

edgIC::usage = "concerned (\[Alpha],\[Beta]) vecponents";
edgIC      = Join @@ Table[
  ArrayReshape[#, {2, K}]& /@ 
    integerCompositions[\[FormalR], 2 K], {\[FormalR], 0,   r}];

matIC::usage = "concerned semi-positive-definite matrix, represented as vecponents";
matIC = edgIC // edg2mat;
matICdim = Dimensions[matIC, 2];

loc::usage = "look up a vecponent in matIC";
loc[vecponent_] := loc[vecponent] = FirstPosition[matIC, vecponent, Missing["NotFound"], {2}];


(* ::Subsubsection:: *)
(*Misc*)


matX::usage = "formal representation of matIC"; 
matX[] = 
  Map[   Superscript[Style[\[FormalX], Larger],    Style[MatrixForm[#, TableSpacing -> {0, 0}], Smaller]] &, matIC, {2}];
matX[s_ : Except[All]] := matX[s] =
  Map[Subsuperscript[Style[\[FormalX], Larger], s, Style[MatrixForm[#, TableSpacing -> {0, 0}], Smaller]] &, matIC, {2}];
matX[All] = Table[matX[k], {k, 1, Power[2,K]}];


(* ::Subsection:: *)
(*Known Moments*)


(*In the semi-definite optimisation procedure, 
m(\[Beta]) = \[DoubleStruckCapitalE][X^\[Beta]]=\[DoubleStruckCapitalE][(S-A)^\[Beta]], 
the moments of the increments to the per-queue waiting times must be known. 
The current version of our code uses the moments of the service time 
and arrival time respectively to calculate these quantities.*)
(* \[Lambda]::usage = "arrival rate";
\[Mu]::usage = "service rate";
\[Lambda] = .5; \[Mu] = 1; *)


(* ::Subsubsection:: *)
(*Increment*)


(*
m::usage = "Moments of X; the generally true form from the arrival and service moments."; 
m[\[Beta]_List] := m[\[Beta]] =
  Product[
    Sum[(                       Binomial[\[Beta][[\[FormalK]]] , \[FormalL]] *
      Power[ Part[outprobs, \[FormalK]], \[Beta][[\[FormalK]]] - \[FormalL]] *
      ServiceMoment[                     \[Beta][[\[FormalK]]] - \[FormalL]] *
      Power[-Part[ inprobs, \[FormalK]], \[FormalL]] *
      ArrivalMoment[                     \[FormalL]]
    ), {\[FormalL], 1, K}], {\[FormalK], 1, K}]
*)


(* ::Subsection:: *)
(*Constraints LHS*)


ConstraintMatView::usage = "Show constraints as matrices; 3-d tensors are shown as a row of matrices.";
ConstraintEqnView::usage = "Show constraints as equations.";
ConstraintMatView[array_List | array_SparseArray] := 
  If[ArrayDepth @ # > 1, Map[Column, #, {ArrayDepth @ # - 1}] &, Identity] @ Map[Row, #, {ArrayDepth@# - 1}]& @
      (Map[MatrixForm, #, {ArrayDepth @ # - 2}]& @ Normal[array]); 
ConstraintEqnView[array_List | array_SparseArray] := 
  If[ArrayDepth@array > 3, Column[Map[(Total[Times[#, matX[All]], {1, 3}] == 0) &, #, {ArrayDepth@array - 3}]]&,
    (Total[Times[#, matX[All]], {1, 3}] == If[ArrayDepth @ array > 3, 0, 1]) &] @ Normal[array]; 

IndieA::usage="The constraint of independence; a list of 3-d arrays.";
IndieA1 = (SparseArray /@ 
  Map[(\[FormalY] |->
        SparseArray[{Prepend[loc[\[FormalY]       ], _] :> 1                   }, Prepend[matICdim, Power[2,K]] ]
    ), DeleteDuplicates[Flatten[#, 1]]
  ] /. (SparseArray[{_ :> 0}, Prepend[matICdim, Power[2,K] ] ] -> Nothing)
) &@matIC;
IndieAm = (SparseArray /@ 
  Map[(\[FormalY] |->
        SparseArray[{Prepend[loc[\[FormalY]*{1, 0}], _] :> \[FormalM][\[FormalY][[-1]] ]}, Prepend[matICdim, Power[2,K]] ]
    ), DeleteDuplicates[Flatten[#, 1]]
  ] /. (SparseArray[{_ :> 0}, Prepend[matICdim, Power[2,K] ] ] -> Nothing)
) &@matIC;
(* IndieA = IndieA1 - IndieAm; *)


CombieA::usage="The constraint of combinatorics; a list of 3-d arrays.";
CombieA = -(SparseArray@
  SparseArray[(
    Flatten[
      Table[(
        CoefficientRules[
          Times @@ (
            Power[(
              Table[
                Indexed[\[FormalW], \[FormalK]] + 
                Indexed[\[FormalX], \[FormalK]]   , 
              {\[FormalK], 1, K}]
            ), #] /. 
            Thread[
              Table[
                Indexed[\[FormalW], \[FormalK]] + 
                Indexed[\[FormalX], \[FormalK]]   , 
              {\[FormalK], Position[IntegerDigits[\[FormalS], 2, K], 0]}]
            -> 0]
          ), 
          Table[Indexed[\[FormalW], \[FormalK]], {\[FormalK], 1, K}] ~Join~
          Table[Indexed[\[FormalX], \[FormalK]], {\[FormalK], 1, K}]
        ] /. (
            (list_
              -> coef_) :> 
            (Prepend[loc[ArrayReshape[list, {2, K}]], \[FormalS] + 1]
              -> If[list[[;; K]] == #, coef - 1, coef])
          )
      ) , 
      {\[FormalS], 0, Power[2,K] - 1}], 
    1] ~Join~ {Prepend[loc[{#, Table[0, K]}], _] -> -1}
  ), Prepend[matICdim, Power[2,K]]
  ] /. (SparseArray[{_ :> 0}, Prepend[matICdim, Power[2,K]]] -> Nothing)
)& /@ Rest[\[Alpha]IC]; 


UnieA::usage="The constraint of unitisation; a list of 3-d arrays.";
UnieA = SparseArray@ SparseArray[Prepend[loc[ConstantArray[0, {2, K}]], _] -> 1, Prepend[matICdim, Power[2,K]]];


CoinA::usage="Additional constraint due to coincident entries; only one 3-d array.";
CoinA = If[# != {}, SparseArray, Identity]@ Flatten[#, 1]& @(
  (mat |-> 
    If[Length[mat] > 2, 
      SparseArray[#,  matICdim] & /@
        ({mat[[1]] -> 1, # -> -1} & /@ mat[[2 ;; Ceiling[Length[mat]/2]]]),
      Nothing]
  ) /@ (Position[matIC, #, 2] & /@ DeleteDuplicates[Flatten[matIC, 1]])
);


PersistentValue[StringTemplate["K`1`r`2`"][K, r],
    PersistenceLocation["Local",
      LocalObject@FileNameJoin[DirectoryName[$InputFileName],"cache"]
    ]
] = <|"IndieA1"->IndieA1,"IndieAm"->IndieAm,"CombieA"->CombieA,"UnieA"->UnieA,"CoinA"->CoinA|>;
Return@PersistentValue[StringTemplate["K`1`r`2`"][K, r],
    PersistenceLocation["Local",
      LocalObject@FileNameJoin[DirectoryName[$InputFileName],"cache"]
    ]
]
