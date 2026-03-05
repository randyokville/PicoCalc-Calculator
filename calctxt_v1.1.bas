'   PicoCALC!  v1.1
'   21:10  2026.03.03
'
' Scientific calculator supporting
' math expressions as they would
' normally be written. For example:
'    8^2+6 <enter> would result in 70.
' Each expression result is assigned
' to a rolling variable A thru F. Those
' variable letters may be used in
' subsequent calculations. Three
' additional variables X, Y & Z may
' be user assigned.
' PicoCALC! uses the MMBASIC Eval
' function to "calculate" results.
' -------------------------------------

Option BASE 1
Const ColrYW=RGB(yellow) , ColrBE=RGB(0,0,70)
Const ColrWE=RGB(white)
Dim STRING Expr(6),VarTag(6),VarDisp(6)
Dim STRING ExprIn,ExprCur,VarCur
Dim STRING OutStr,OptKey
Dim FLOAT Ans(6),A,B,C,D,E,F,AnsCur
Dim FLOAT X,Y,Z
' Index, Instance and pad length counters
Dim INTEGER Indx,Inst,OutLen,OnOff
Dim INTEGER SyntxPg=1
VarTag(1)="A" : VarTag(2)="B" : VarTag(3)="C"
VarTag(4)="D" : VarTag(5)="E" : VarTag(6)="F"
CLS ColrBE
' initialize the fixed screen elements
Box 252,1,67,22,,ColrYW,ColrYW ' top rt corner box
Text 256,4," PicoCalc",LT,7, ,ColrBE,ColrYW   ' smaller text
Text 256,13,"CALCULATOR",LT,7, ,ColrBE,ColrYW  ' w/ inverted colors
Color ColrWE,ColrBE
RBox 1,68,319,110    ' results window
Print @(25,63) "< results stack & variables A-F >"
ActiveBox("InpBox",1)
ActiveBox("VarBox",0)
ActiveBox("SynBox",0)
TOP:
' zero out arrays & wipe answer stack
Array Set 0,Ans()
Array Set "",Expr()
Array Set "",VarDisp()
A=0:B=0:C=0:D=0:E=0:F=0
For Indx=1 To 6
  Print @(16,70+Indx*15) Space$(37); ' for resets
Next Indx
Inst=0
Do  ' main control loop
  Inc Inst
  If Inst=7 Then Inst=1
  ' blank the input box, prompt for input
  Print @(10,33) Space$(37)
  Print @(6,33);
  Line Input "> "; ExprIn
  ExprIn=UCase$(ExprIn)
  Select Case ExprIn
    Case "Q"   ' quit
      Exit Do
    Case "S" ' display syntax
      Assist
      ' decrement the Instance for A-F tracking
      Inst=Inst-1
      Continue DO
    Case "V"  ' get user vars
      UserVars
      ' decrement the Instance for A-F tracking
      Inst=Inst-1
      Continue DO
    Case "R"   ' reset
      Text 16,33,"  resetting  ",,1,,ColrBE,ColrYW  ' w/ inverted colors
      Pause 800
      Text 16,33,Space$(15),,1,,ColrYW,ColrBE  ' blank msg
      Exit Do ' drop out of main loop
    Case "T" ' show time on status line
      Print @(250,307) Time$;
      Pause 1000
      Print @(250,307) Space$(8);
      ' decrement the Instance for A-F tracking
      Inst=Inst-1
      Continue DO
  End Select
  ExprCur=ExprIn
  On ERROR IGNORE  ' temp pause error handling
  AnsCur=Eval(ExprIn)
  If MM.Errno Then   ' error detected
    ExprCur= " ?? "+ExprCur
    AnsCur=0
    Text 16,33,"  ? syntax ?  ",,1,,ColrBE,ColrYW  ' w/ inverted colors
    Pause 750
    Text 16,33,Space$(37),,1,,ColrYW,ColrBE  ' blank msg
  EndIf
  On ERROR ABORT  ' reset to normal error handling
  UpdtScr  ' update results matrix
Loop  ' end control loop (Q pressed)
If ExprIn="R" GoTo TOP  ' reset requested
Print @(17,33) " QUIT"
Print @(1,305) "END"
End


Sub UpdtScr  ' update the results matrix
  ' coming here with ExprCur, AnsCur, Inst (=1->6)
  ' loop thru answer matrix backward and shift
  '   the results down one position
  For Indx=6 To 2 Step -1
    VarDisp(Indx)=VarDisp(Indx-1)
    Expr(Indx)=Expr(Indx-1)
    Ans(Indx)=Ans(Indx-1)
    If VarDisp(Indx)="A" Then A=Ans(indx)
    If VarDisp(Indx)="B" Then B=Ans(indx)
    If VarDisp(Indx)="C" Then C=Ans(indx)
    If VarDisp(Indx)="D" Then D=Ans(indx)
    If VarDisp(Indx)="E" Then E=Ans(indx)
    If VarDisp(Indx)="F" Then F=Ans(indx)
    If Expr(Indx)<>"" Then  ' only applies initial 6 inputs
      Color ColrYW,ColrBE
      Print @(16,70+Indx*15) VarDisp(Indx);
      Color ColrWE,ColrBE
      OutStr= "= "+Str$(Ans(Indx))+" : "+Expr(Indx)
      Print @(24,70+Indx*15) OutStr+Space$(36-Len(OutStr));
    EndIf
  Next Indx
  VarDisp(1)=VarTag(Inst)
  Expr(1)=ExprCur
  Ans(1)=AnsCur
  ' assign new answer to next variable
  If VarDisp(1)="A" Then A=AnsCur
  If VarDisp(1)="B" Then B=AnsCur
  If VarDisp(1)="C" Then C=AnsCur
  If VarDisp(1)="D" Then D=AnsCur
  If VarDisp(1)="E" Then E=AnsCur
  If VarDisp(1)="F" Then F=AnsCur
  OutStr= "= "+Str$(Ans(1))+" : "+Expr(1)
  Color ColrYW,ColrBE
  Print @(16,85) VarDisp(1);
  Color ColrWE,ColrBE
  Print @(24,85) OutStr+Space$(37-Len(OutStr))
  Color ColrYW,ColrBE
End Sub   ' updtscr


Sub UserVars  ' prompt for user vars x,y,z
  OptKey=""
  ActiveBox("InpBox",0)
  ActiveBox("VarBox",1)
  Color ColrYW,ColrBE
  Do While OptKey=""
    OptKey=UCase$(Inkey$)
    Select Case OptKey
      Case "X"    ' prompt for x value
        OptKey=""
        Color ColrYW,ColrBE
        Print @(15,215);
        Input "X= ", X
        Print @(10,215)i Space$(22)
        Print @(24,230) "X";
        Color ColrWE,ColrBE
        Print "= ";X;Space$(15-Len(Str$(X)))
      Case "Y"    ' prompt for y value
        OptKey=""
        Color ColrYW,ColrBE
        Print @(15,215);
        Input "Y= ", Y
        Print @(10,215) Space$(22)
        Print @(24,245) "Y";
        Color ColrWE,ColrBE
        Print "= ";Y;Space$(15-Len(Str$(Y)))
      Case "Z"    ' prompt for z value
        OptKey=""
        Color ColrYW,ColrBE
        Print @(15,215);
        Input "Z= ", Z
        Print @(10,215) Space$(22)
        Print @(24,260) "Z";
        Color ColrWE,ColrBE
        Print "= ";Z;Space$(15-Len(Str$(Z)))
      Case Chr$(13)  'enter
        Exit Do
      Case Else
        OptKey=""
      End Select
  Loop
  ActiveBox("VarBox",0)
  ActiveBox("InpBox",1)
End Sub   'uservars


Sub Assist  'syntax help screen
  OptKey=""
  ActiveBox("InpBox",0)
  ActiveBox("SynBox",1)
  Color ColrWE,ColrBE
  Do While OptKey=""
    OptKey=UCase$(Inkey$)
    Select Case OptKey
      Case Chr$(129)  'dn key
        OptKey=""
        Inc SyntxPg
        If SyntxPg>2 Then SyntxPg=2
        ActiveBox("SynBox",1)
      Case Chr$(128)  'up key
        OptKey=""
        Inc SyntxPg, -1
        If SyntxPg=0 Then SyntxPg=1
        ActiveBox("SynBox",1)
      Case Chr$(13)   'enter key
        Exit Do
      Case Else
        OptKey=""
      End Select
  Loop
  ActiveBox("InpBox",1)
  ActiveBox("SynBox",0)
End Sub   'assist



Sub ActiveBox(BoxNam$,OnOff)
Select Case BoxNam$
  Case "SynBox"
    If OnOff Then  '=1, box ON colors
      Color ColrYW,ColrBE
    Else           '=0, box OFF colors
      Color ColrWE,ColrBE
    EndIf
    RBox 195, 195, 124, 99
    Print @(217,190) "< syntax >"
    Print @(205,288) "<up><dn><ent>"
    Color ColrWE,ColrBE
    If SyntxPg=1 Then
      Print @(200,207) "+-*/   PI     "
      Print @(200,222) "x^y    x^(1/y)"
      Print @(200,237) "SQR(x) RND    "
      Print @(200,252) "LOG(x) EXP(x) "
      Print @(200,271) "    1of2      "
    EndIf
    If SyntxPg=2 Then
      Print @(200,207) "SIN(x) ASIN(x)"
      Print @(200,222) "COS(x) ACOS(x)"
      Print @(200,237) "TAN(x) ATN(x) "
      Print @(200,252) "DEG(x) RAD(x) "
      Print @(200,271) "    2of2      "
    EndIf
  Case "InpBox"
    If OnOff Then  '=1, box on colors
      Color ColrYW,ColrBE
    Else           '=0, box off colors
      Color ColrBE,ColrBE
    EndIf
    RBox 1, 26, 319, 25  ' input window coords
    Print @(5,4) "<Q>uit <S>yntax <R>eset <V>ars";
    Color ColrYW,ColrBE  ' reset normal
    Print @(6,33) Space$(5);
  Case "VarBox"
    If OnOff Then  '=1, box on colors
      Color ColrYW,ColrBE
    Else           '=0, box off colors
      Color ColrWE,ColrBE
    EndIf
    RBox 3, 195, 190, 99
    Print @(22,288) "<X> <Y> <Z> <enter>"
    Print @(25,190) "< user variables >"
  End Select
  Color ColrYW,ColrBE  ' reset normal
End Sub 'ActiveBox


Sub mm.prompt  ' set system prompt
   Print Time$ " > ";
End Sub
