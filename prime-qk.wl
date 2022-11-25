BeginPackage["PrimeQk`"]


(* judge *)
superscript[a_,b_]:=If[b==1,a,Superscript[a,b]]
centerDot[li_]:=If[Length[li]==1,First[li],CenterDot@@li]
j[n_]:=If[PrimeQ[n],ToString[PrimePi[n]]<>"\:756a\:76ee\:306e\:7d20\:6570",centerDot[superscript@@@FactorInteger[n]]]
SetAttributes[j,Listable]


divisible[check_Integer,OptionsPattern[origin->True]]:=Divisible[#,check]&&If[OptionValue[origin],True,#!=check]&
divisible[checkLi_List,OptionsPattern[origin->True]]:=AnyTrue[Table[divisible[check,origin->OptionValue[origin]][#],{check,checkLi}],TrueQ]&


qkEvenQ:=divisible[{2,5}]
qkOddQ:=!qkEvenQ[#]&&#!=1&


checkQ[checkLi_,OptionsPattern[origin->False]]:=!AnyTrue[Table[divisible[check,origin->OptionValue[origin]][#],{check,checkLi}],TrueQ]&


pass3checkQ:=checkQ[{3}]
pass91checkQ:=checkQ[{7,13}]
pass969checkQ:=checkQ[{3,17,19}]
pass1001checkQ:=checkQ[{7,11,13}]
pass2001checkQ:=checkQ[{3,23,29}]


(* alias *)
select[crit_]:=Select[crit]
select[li_,crit_]:=select[crit][li]


selectAllTrue[critLi_List]:=select[AllTrue[Table[crit[#],{crit,critLi}],TrueQ]&];
selectAllTrue[li_,critLi_List]:=selectAllTrue[critLi][li];


selectAnyTrue[critLi_List]:=select[AnyTrue[Table[crit[#],{crit,critLi}],TrueQ]&];
selectAnyTrue[li_,critLi_List]:=selectAnyTrue[critLi][li];


select[critLi_List]:=selectAllTrue[critLi]
select[li_,critLi_List]:=select[critLi][li]


reject[crit_]:=select[!crit[#]&]
reject[li_,crit_]:=reject[crit][li]


rejectAllTrue[critLi_List]:=reject[AllTrue[Table[crit[#],{crit,critLi}],TrueQ]&];
rejectAllTrue[li_,critLi_List]:=rejectAllTrue[critLi][li];


rejectAnyTrue[critLi_List]:=reject[AnyTrue[Table[crit[#],{crit,critLi}],TrueQ]&];
rejectAnyTrue[li_,critLi_List]:=rejectAnyTrue[critLi][li];


reject[critLi_List]:=rejectAllTrue[critLi]
reject[li_,critLi_List]:=reject[critLi][li]


selectPrime[li_]:=select[li,PrimeQ]
rejectPrime[li_]:=reject[li,PrimeQ]


selectEven[li_]:=select[li,EvenQ]
rejectEven[li_]:=reject[li,EvenQ]


selectDivisible[li_,checkLi_List,OptionsPattern[origin->False]]:=select[li,divisible[checkLi,origin->OptionValue[origin]]]
selectDivisible[li_,check_Integer,OptionsPattern[origin->False]]:=selectDivisible[li,{check},origin->OptionValue[origin]]


rejectDivisible[li_,checkLi_List,OptionsPattern[origin->False]]:=reject[li,divisible[checkLi,origin->OptionValue[origin]]]
rejectDivisible[li_,check_Integer,OptionsPattern[origin->False]]:=rejectDivisible[li,{check},origin->OptionValue[origin]]


selectM3[li_]:=selectDivisible[li,3]
rejectM3[li_]:=rejectDivisible[li,3]


ratio[li_,crit_,OptionsPattern[displayStyle->"none"]]:=With[{numerator=Length[select[li,crit]],denominator=Length[li]},With[{r=numerator/denominator,disp=OptionValue[displayStyle]},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"HoldForm", "[", 
FractionBox["numerator", "denominator"], "]"}], 
RowBox[{"disp", "==", "\"\<account\>\""}]},
{
RowBox[{"N", "[", "r", "]"}], 
RowBox[{"disp", "==", "\"\<numerical\>\""}]},
{
RowBox[{"PercentForm", "[", 
RowBox[{"N", "[", "r", "]"}], "]"}], 
RowBox[{"disp", "==", "\"\<percent\>\""}]},
{"r", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False]\)]]


randomInteger[d_]:=RandomInteger[{10^(d-1),10^d-1}]
randomInteger[d_,n_]:=RandomInteger[{10^(d-1),10^d-1},n]


primeRatio[li_,OptionsPattern[displayStyle->"none"]]:=ratio[li,PrimeQ,displayStyle->OptionValue[displayStyle]]


EndPackage[]


(* ::Section:: *)
(*Usage*)


Begin["`Private`"];


1001//j


j/@Range[20]


selectPrime[Range[20]]


rejectPrime[Range[20]]


selectEven[Range[20]]


rejectEven[Range[20]]


selectDivisible[Range[100],13]


selectDivisible[Range[20],{2,5}]
select[Range[20],qkEvenQ]


select[Range[50],qkOddQ]


rejectDivisible[Range[30],10]


rejectDivisible[Range[50],{2,5}]


selectM3[Range[20]]


rejectM3[Range[20]]


select[Range[90,110],pass91checkQ]


select[Range[950,970],pass969checkQ]


select[Range[1000,1010],pass1001checkQ]


select[Range[2000,2010],pass2001checkQ]


reject[Range[1000,1100],pass1001checkQ]


select[Range[999],{qkOddQ,divisible[3]}]


primeRatio[Range[1000,9999],displayStyle->"percent"]


ratio[select[Range[1000,9999],{qkOddQ,pass1001checkQ,pass2001checkQ}],PrimeQ,displayStyle->"account"]


reject[divisible[7]]/@select[{qkOddQ,pass3checkQ}]/@Table[Flatten[Table[FromDigits[Join[IntegerDigits[c],{m},IntegerDigits[c],{n}]],{m,9},{n,9}]],{c,10,13}]
primeRatio[#,displayStyle->"percent"]&/@%


randomInteger[6]


randomInteger[3,8]


End[];
