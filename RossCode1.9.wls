#!/usr/bin/env wolframscript
(* ::Package:: *)

Clear["Global`*"]



#!/usr/bin/env wolframscript


(*"mirror 1 position and home orientation"*)
homeN1={1,0,0};
(*true value:*)  M1={2.53,1.03,4.19};
(*for testing: M1={5.03,0,3.97};*)
(*"mirror 2 home position and orientation"*)
homeN2={0,-1,0};
homeM2={5.03,7.97,3.97};
(*"light pipe position"*)
(*true: *)P={4.87,8.13,4.19};
(*for testing: P={4.87,8.13,4.19};*)
(*"incoming laser direction:"*)
L0={-1,0,0};


(*position of the mirror to reflect the "in" beam 
direction to the "out" beam direction:*)
mirrorNorm[in_,out_]:=Normalize[-in+out]

(*beam direction when the "in" beam is reflected 
around a mirror pointed in the "norm" direction:*)
reflectBeam[in_,norm_]:=in-2(in.norm)norm

(*norm of a mirror that started in "norm0" 
direction, and is rotated by \[Gamma] around the y axis, 
then \[Alpha] around the x axis, then \[Beta] around the z axis:*)
rotatedNorm[\[Alpha]_,\[Gamma]_,\[Beta]_,norm0_]:=RotationMatrix[\[Beta],{0,0,1}].RotationMatrix[-\[Alpha],{1,0,0}].RotationMatrix[-\[Gamma],{0,1,0}].norm0

(*position at which a line of the form 
"position=vector*t+start", where t is a distance
along the line, intersects a specific z position:*)
positionAtZ[vector_,start_,z_]:=start+(z-start[[3]])/vector[[3]]*vector

(*Angle wrt the z axis of normalized vector "L":*)
(*old: \[Theta]ofBeam[L_]:=ArcCos[L[[3]]] *)
\[Theta]ofBeam[L_]:=ArcTan[L[[3]],Sqrt[L[[1]]^2+L[[2]]^2]]

(*Angle in the xy plane, wrt the x axis, of 
normalized vector "L", with differentiable 
form and a 'safe' version:*)
\[Phi]ofBeam[L_]:=ArcTan[L[[1]],L[[2]]](*note that the args are denom, numerator, not the other way around!*)
safe\[Phi]ofBeam[L_]:=If[L[[1]]==0,If[L[[2]]==0,0,\[Pi]/2],\[Phi]ofBeam[L]](*avoids the singularity if we are going purely in z*)

(*angle wrt the xy plane of normalized vector "N":*)
(*old:  pitchOfMirror[N_]:=ArcSin[N[[3]]] *)
pitchOfMirror[N_]:=ArcTan[Sqrt[N[[1]]^2+N[[2]]^2],N[[3]]]

(*angle in the xy plane, wrt Nhome, of 
normalized vector "N":*)
(*old: \[Beta]ofMirror[N_,Nhome_]:=ArcCos[Normalize[N-{0,0,N[[3]]}].Nhome] *)
\[Beta]ofMirror[N_,Nhome_]:=ArcTan[N[[1]],N[[2]]]-ArcTan[Nhome[[1]],Nhome[[2]]]



(*"forward derivation:"*)
forwardN1[\[Gamma]_,\[Beta]_]:=rotatedNorm[0,\[Gamma],\[Beta],homeN1]
forwardN2[\[Alpha]_,\[Beta]_]:=rotatedNorm[\[Alpha],0,\[Beta],homeN2]
forwardL1[N1_]:=reflectBeam[L0,N1]
forwardL1c[\[Gamma]_,\[Beta]_]:=forwardL1[forwardN1[\[Gamma],\[Beta]]]
forwardM2[L1_]:=positionAtZ[L1,M1,homeM2[[3]]]
forwardM2c[\[Gamma]_,\[Beta]_]:=forwardM2[forwardL1c[\[Gamma],\[Beta]]]
forwardL2[L1_,N2_]:=reflectBeam[L1,N2]
forwardL2c[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=forwardL2[forwardL1c[\[Gamma]1,\[Beta]1],forwardN2[\[Alpha]2,\[Beta]2]]


(*Swapper term order to make all agree*)
forward\[Theta][\[Beta]1_,\[Gamma]1_,\[Beta]2_,\[Alpha]2_]:=\[Theta]ofBeam[forwardL2c[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]]
forward\[Phi][\[Beta]1_,\[Gamma]1_,\[Beta]2_,\[Alpha]2_]:=\[Phi]ofBeam[forwardL2c[\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2]]
forward\[Sigma]\[Theta][\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_,\[Sigma]\[Gamma]1_,\[Sigma]\[Beta]1_,\[Sigma]\[Alpha]2_,\[Sigma]\[Beta]2_]:=D[forward\[Theta][a,b,c,d],{{a,b,c,d}}].{\[Sigma]\[Gamma]1,\[Sigma]\[Beta]1,\[Sigma]\[Alpha]2,\[Sigma]\[Beta]2}/.a->\[Gamma]1/.b->\[Beta]1/.c->\[Alpha]2/.d->\[Beta]2
forward\[Theta]Error[angles_,sigmas_]:=(D[forward\[Theta][a,b,c,d],{{a,b,c,d}}]^2).sigmas/.{a->angles[[1]],b->angles[[2]],c->angles[[3]],d->angles[[4]]}
forward\[Phi]Error[angles_,sigmas_]:=(D[forward\[Phi][a,b,c,d],{{a,b,c,d}}]^2).sigmas/.{a->angles[[1]],b->angles[[2]],c->angles[[3]],d->angles[[4]]}

(D[a*x+b*y+c*z+d*q,{{a,b,c,d}}]^2).{1,2,3,4}


forwardPrint[\[Gamma]1_,\[Beta]1_,\[Alpha]2_,\[Beta]2_]:=
Module[{\[Theta],\[Phi],M2,N1,N2,L1,L2},
N1=forwardN1[\[Gamma]1,\[Beta]1];
N2=forwardN2[\[Alpha]2,\[Beta]2];
L1=forwardL1[N1];
M2=forwardM2[L1];
M2=forwardM2c[\[Gamma]1,\[Beta]1];
L2=forwardL2[N2,L1];
\[Theta]=forward\[Theta][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];
\[Phi]=forward\[Phi][\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2];
Print["Forward derivation:"];
Print["N1:",N1];
Print["N2:",N2];
Print["L1:",L1];
Print["L2:",L2];
Print[Column[{"\[Degree]=\[Beta]1 "," "\[Beta]1*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1 "," "\[Gamma]1*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2 "," "\[Beta]2*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2 "," "\[Alpha]2*180/\[Pi]}],
Column[{" ==> "," ==> "}],
Column[{" M2-home "," "Evaluate[M2-homeM2]}],
Column[{" \[Theta] "," "\[Theta]*180/\[Pi]}],
Column[{" \[Phi] "," "\[Phi]*180/\[Pi]}]]
]



(*"backward derivation:"*)
backwardL2[\[Theta]_,\[Phi]_]:={Sin[\[Theta]]Cos[\[Phi]],Sin[\[Theta]]Sin[\[Phi]],Cos[\[Theta]]}
backwardM2[L2_]:=positionAtZ[L2,P,homeM2[[3]]]
backwardM2c[\[Theta]_,\[Phi]_]:=backwardM2[backwardL2[\[Theta],\[Phi]]]
backwardL1[M2_]:=Normalize[M2-M1]
backwardL1c[\[Theta]_,\[Phi]_]:=backwardL1[backwardM2c[\[Theta],\[Phi]]]
backwardN2[L1in_,L2out_]:=mirrorNorm[L1in,L2out]
backwardN2c[\[Theta]_,\[Phi]_]:=backwardN2[backwardL1c[\[Theta],\[Phi]],backwardL2[\[Theta],\[Phi]]]
backwardN1[L0in_,L1out_]:=mirrorNorm[L0in,L1out]
backwardN1c[\[Theta]_,\[Phi]_]:=backwardN1[L0,backwardL1c[\[Theta],\[Phi]]]

backward\[Beta]1[\[Theta]_,\[Phi]_]:=\[Beta]ofMirror[backwardN1c[\[Theta],\[Phi]],homeN1]
backward\[Gamma]1[\[Theta]_,\[Phi]_]:=pitchOfMirror[backwardN1c[\[Theta],\[Phi]]]
backward\[Beta]2[\[Theta]_,\[Phi]_]:=\[Beta]ofMirror[backwardN2c[\[Theta],\[Phi]],homeN2]
backward\[Alpha]2[\[Theta]_,\[Phi]_]:=pitchOfMirror[backwardN2c[\[Theta],\[Phi]]]
backward4Angles[\[Theta]_,\[Phi]_]:={backward\[Beta]1[\[Theta],\[Phi]],backward\[Gamma]1[\[Theta],\[Phi]],backward\[Beta]2[\[Theta],\[Phi]],backward\[Alpha]2[\[Theta],\[Phi]]}


backwardPrint[\[Theta]_,\[Phi]_]:=
Module[{\[Gamma]1,\[Beta]1,\[Alpha]2,\[Beta]2,M2,N1,N2,L1,L2},
L2=backwardL2[\[Theta],\[Phi]];
M2=backwardM2[L2];
L1=backwardL1[M2];
N2=backwardN2[L1,L2];
N1=backwardN1[L0,L1];
\[Gamma]1=backward\[Gamma]1[\[Theta],\[Phi]];
\[Beta]1=backward\[Beta]1[\[Theta],\[Phi]];
\[Alpha]2=backward\[Alpha]2[\[Theta],\[Phi]];
\[Beta]2=backward\[Beta]2[\[Theta],\[Phi]];
Print["Backward derivation:"];
Print["N1:",N1];
Print["N2:",N2];
Print["L1:",L1];
Print["L2:",L2];
Print[Column[{"\[Degree]=\[Beta]1 "," "\[Beta]1*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1 "," "\[Gamma]1*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2 "," "\[Beta]2*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2 "," "\[Alpha]2*180/\[Pi]}],
Column[{" M2-home "," "Evaluate[M2-homeM2]}],
Column[{" <== "," <== "}],
Column[{" \[Theta] "," "\[Theta]*180/\[Pi]}],
Column[{" \[Phi] "," "\[Phi]*180/\[Pi]}]]
]



"calculations for home:"
L2ymax=Normalize[P-(homeM2)];
positionAtZ[L2ymax,(homeM2),P[[3]]]-P;
positionAtZ[L2ymax,P,homeM2[[3]]]-homeM2;
\[Theta]ymax=\[Theta]ofBeam[L2ymax];
\[Theta]ymax*180/\[Pi]"\[Degree]=\[Theta]"
\[Phi]ymax=safe\[Phi]ofBeam[L2ymax];
\[Phi]ymax*180/\[Pi]"\[Degree]=\[Phi]"
"M2-homeM2="
backwardM2c[\[Theta]ymax,\[Phi]ymax]-homeM2
Print[Column[{"\[Degree]=\[Beta]1"," "backward\[Beta]1[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1"," "backward\[Gamma]1[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2"," "backward\[Beta]2[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2"," "backward\[Alpha]2[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}]]
forwardPrint[backward\[Gamma]1[\[Theta]ymax,\[Phi]ymax],
			 backward\[Beta]1[\[Theta]ymax,\[Phi]ymax],
			 backward\[Alpha]2[\[Theta]ymax,\[Phi]ymax],
			 backward\[Beta]2[\[Theta]ymax,\[Phi]ymax]]
backwardPrint[\[Theta]ymax,\[Phi]ymax]



"calculations for YMax:"
L2ymax=Normalize[P-(homeM2+{0,+2.50,0})];
positionAtZ[L2ymax,(homeM2+{0,+2.52,0}),P[[3]]]-P;
positionAtZ[L2ymax,P,homeM2[[3]]]-homeM2;
\[Theta]ymax=\[Theta]ofBeam[L2ymax];
\[Theta]ymax*180/\[Pi]"\[Degree]=\[Theta]"
\[Phi]ymax=\[Phi]ofBeam[L2ymax];
\[Phi]ymax*180/\[Pi]"\[Degree]=\[Phi]"
"M2-M1="
backwardM2c[\[Theta]ymax,\[Phi]ymax]-M1
Print[Column[{"\[Degree]=\[Beta]1"," "backward\[Beta]1[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1"," "backward\[Gamma]1[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2"," "backward\[Beta]2[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2"," "backward\[Alpha]2[\[Theta]ymax,\[Phi]ymax]*180/\[Pi]}]]
forwardPrint[backward\[Gamma]1[\[Theta]ymax,\[Phi]ymax],
			 backward\[Beta]1[\[Theta]ymax,\[Phi]ymax],
			 backward\[Alpha]2[\[Theta]ymax,\[Phi]ymax],
			 backward\[Beta]2[\[Theta]ymax,\[Phi]ymax]]
backwardPrint[\[Theta]ymax,\[Phi]ymax]



"calculations for XYMax:"
L2xymax=Normalize[P-(homeM2+{-2.50,+2.50,0})];
\[Theta]xymax=\[Theta]ofBeam[L2xymax];
\[Theta]xymax*180/\[Pi]"\[Degree]=\[Theta]"
\[Phi]xymax=\[Phi]ofBeam[L2xymax];
\[Phi]xymax*180/\[Pi]"\[Degree]=\[Phi]"
"M2-M1="
backwardM2c[\[Theta]xymax,\[Phi]xymax]-M1
"Laser1="
laserdir=backwardL1c[\[Theta]xymax,\[Phi]xymax]
"Laser tilt="
"\[Degree]=from z"\[Theta]ofBeam[laserdir]*180/\[Pi]
"\[Degree]=from x"\[Phi]ofBeam[laserdir]*180/\[Pi]
"mirror M1 norm="
m1temp=mirrorNorm[homeN1,laserdir]
"pitchOfMirror:"
pitchOfMirror[m1temp]*180/\[Pi]
"manual mirror tilt:"
ArcTan[m1temp[[3]],Sqrt[m1temp[[2]]^2+m1temp[[1]]^2]]*180/\[Pi]
"reflected beam using those tilts:"
reflectBeam[L0,m1temp]
Print[Column[{"\[Degree]=\[Beta]1"," "backward\[Beta]1[\[Theta]xymax,\[Phi]xymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1"," "backward\[Gamma]1[\[Theta]xymax,\[Phi]xymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2"," "backward\[Beta]2[\[Theta]xymax,\[Phi]xymax]*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2"," "backward\[Alpha]2[\[Theta]xymax,\[Phi]xymax]*180/\[Pi]}]]
forwardPrint[backward\[Gamma]1[\[Theta]xymax,\[Phi]xymax],
			 backward\[Beta]1[\[Theta]xymax,\[Phi]xymax],
			 backward\[Alpha]2[\[Theta]xymax,\[Phi]xymax],
			 backward\[Beta]2[\[Theta]xymax,\[Phi]xymax]]
backwardPrint[\[Theta]xymax,\[Phi]xymax]



"calculations for XMax:"
L2xmax=Normalize[P-(homeM2+{-2.50,0,0})];
\[Theta]xmax=\[Theta]ofBeam[L2xmax];
\[Theta]xmax*180/\[Pi]"\[Degree]=\[Theta]"
\[Phi]xmax=\[Phi]ofBeam[L2xmax];
\[Phi]xmax*180/\[Pi]"\[Degree]=\[Phi]"
"M2-homeM2="
backwardM2c[\[Theta]ymax,\[Phi]xmax]-homeM2
Print[Column[{"\[Degree]=\[Beta]1"," "backward\[Beta]1[\[Theta]xmax,\[Phi]xmax]*180/\[Pi]}],
Column[{"\[Degree]=\[Gamma]1"," "backward\[Gamma]1[\[Theta]xmax,\[Phi]xmax]*180/\[Pi]}],
Column[{"\[Degree]=\[Beta]2"," "backward\[Beta]2[\[Theta]xmax,\[Phi]xmax]*180/\[Pi]}],
Column[{"\[Degree]=\[Alpha]2"," "backward\[Alpha]2[\[Theta]xmax,\[Phi]xmax]*180/\[Pi]}]]



(*Unit tests*)
(* for Y Max:
9.436 in Y
-0.217 in Z
0.00 in X *)
homeM2-M1


Plot[Evaluate[
Table[\[Phi]ofBeam[forwardL1c[gamma*\[Pi]/180,beta*\[Pi]/180]]*180/\[Pi],{beta,0,90,15}]
],{gamma,0,45},AxesLabel->{"tilt of mirror","\[Phi] of beam"},
PlotLegends->{Table[beta "\[Degree]\[Beta]",{beta,0,90,15}]}]


Plot[Evaluate[
Table[90-\[Theta]ofBeam[forwardL1c[gamma*\[Pi]/180,beta*\[Pi]/180]]*180/\[Pi],{beta,0,90,15}]
],{gamma,0,45},AxesLabel->{"tilt of mirror","\[Theta] of beam"},
PlotLegends->{Table[beta "\[Degree]\[Beta]",{beta,0,90,15}]}]


ContourPlot[90-\[Theta]ofBeam[forwardL1c[gamma*\[Pi]/180,beta*\[Pi]/180]]*180/\[Pi],
{beta,0,90},{gamma,0,45},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Beta] rotation","\[Degree]tilt"},
PlotLabel->"\[Degree]\[Theta] of beam"]


Plot[\[Beta]ofMirror[rotatedNorm[0,0,\[Beta]*\[Pi]/180,homeN2],homeN2]*180/\[Pi],{\[Beta],-360,360}]



backward4Angles[1,1]
(*ContourPlot[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180].{180/\[Pi],0,0,0},{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"\[Degree]\[Beta] of mirror 1"]
ContourPlot[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180].{0,180/\[Pi],0,0},{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"\[Degree]\[Gamma] of mirror 1"]
ContourPlot[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180].{0,0,180/\[Pi],0},{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"\[Degree]\[Beta] of mirror 2"]
ContourPlot[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180].{0,0,0,180/\[Pi]},{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"\[Degree]\[Alpha] of mirror 2"]*)



forwardError[angles_,sigmas_]

(*forward\[Theta]Error[backward4Angles[\[Theta],\[Phi]],{1,0,0,0}]
DensityPlot[forward\[Theta]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{1,0,0,0}],{\[Theta],5,85},{\[Phi],0,360},PlotPoints->{18,72},Mesh->Full,PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Beta] of mirror 1"]
ContourPlot[forward\[Theta]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,1,0,0}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Gamma] of mirror 1"]
ContourPlot[forward\[Theta]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,0,1,0}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Beta] of mirror 2"]
ContourPlot[forward\[Theta]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,0,0,1}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Alpha] of mirror 2"]*)

DensityPlot[forward\[Phi]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{1,0,0,0}],{\[Theta],5,85},{\[Phi],0,360},PlotPoints->{18,72},Mesh->Full,PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Beta] of mirror 1"]
ContourPlot[forward\[Phi]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,1,0,0}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Gamma] of mirror 1"]
ContourPlot[forward\[Phi]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,0,1,0}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Beta] of mirror 2"]
ContourPlot[forward\[Phi]Error[backward4Angles[\[Theta]*\[Pi]/180,\[Phi]*\[Pi]/180],{0,0,0,1}],{\[Theta],5,85},{\[Phi],0,360},PlotLegends->Automatic, FrameLabel->{"\[Degree]\[Theta]","\[Degree]\[Phi]"},
PlotLabel->"coef of \[Sigma]\[Alpha] of mirror 2"]




