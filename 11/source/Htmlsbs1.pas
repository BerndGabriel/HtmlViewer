{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

{$I htmlcons.inc}

unit Htmlsbs1;

interface

uses
  Windows, Classes, Graphics{$ifdef LCL}, Interfaces{$endif}, Controls,
  HtmlUn2, HtmlSubs, StyleUn;

type

  TOptionObj = class(TObject) {used by TListBoxFormControlObj for <option> information}
  public
    Value: ThtString; {<option>  Value=  }
    Selected: boolean; {set if Selected found in <option>}
    Attributes: TStringList; {list of <option> attributes}
    destructor Destroy; override;
  end;

  ThtOptionStringList = class(ThtStringList)
  private
    function GetValue(Index: integer): ThtString;
    function GetSelected(Index: integer): boolean;
    procedure SetSelected(Index: integer; Value: boolean);
    function GetAttribute(Index: integer; const AttrName: string): string;
  public
    property Value[Index: integer]: ThtString read GetValue;
    property Selected[Index: integer]: boolean read GetSelected write SetSelected;
    property AttributeValue[Index: integer; const AttrName: string]: string read GetAttribute;
    destructor Destroy; override;
  end;

  TListBoxFormControlObj = class(TFormControlObj)
  {Select with "Multiple" or Size > 1}
  private
    LBSize, Longest: integer;
    TheFont: TFont;
  private
    EnterItems: integer;
    EnterSelected: array[0..50] of boolean;
{$IFDEF OpOnChange}
    procedure OptionalOnChange(Sender: TObject);
{$ENDIF}
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    TheOptions: ThtOptionStringList;
    {TheOptions is the original Options list for reseting.  It does not reflect
     the current selected state.  The Strings part is the Items in the list/combobox.
     The Options part is TOptionObj defined above}

    constructor Create(AMasterList: TSectionList; Position: integer;
      L: TAttributeList; Prop: TProperties);
    destructor Destroy; override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure AddStr(const WS: WideString; Selected: boolean; Attr: TStringList; CodePage: integer);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: ThtString): boolean; override;
    procedure SetData(Index: integer; const V: ThtString); override;
  end;

  TComboFormControlObj = class(TListBoxFormControlObj)
  {Select with single selection and Size = 1}
  private
    EnterIndex: integer;
{$IFDEF OpOnChange}
    procedure OptionalOnChange(Sender: TObject);
{$ENDIF}
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    constructor Create(AMasterList: TSectionList; Position: integer;
      L: TAttributeList; Prop: TProperties);
    procedure ProcessProperties(Prop: TProperties); override;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ResetToValue; override;
    procedure SetHeightWidth(ACanvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: ThtString): boolean; override;
    procedure SetData(Index: integer; const V: ThtString); override;
  end;

  TTextAreaFormControlObj = class(TFormControlObj)
  private
    EnterContents: ThtString;
  protected
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    Wrap: (wrOff, wrSoft, wrHard);
    Rows, Cols: integer;
    TheText: ThtString;
    constructor Create(AMasterList: TSectionList; Position: integer;
      L: TAttributeList; Prop: TProperties);
    destructor Destroy; override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure Draw(Canvas: TCanvas; X1, Y1: integer); override;
    procedure AddStr(const S: ThtString);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    function GetSubmission(Index: integer; var S: ThtString): boolean; override;
    procedure SetData(Index: integer; const V: ThtString); override;
  end;

  TFormControlList = class(TList) {a list of TFormControlObj's} {not TFreeList}
  public
    function FindControl(Posn: integer): TFormControlObj;
    function GetHeightAt(Posn: integer; var FormAlign: AlignmentType): Integer;
    function GetWidthAt(Posn: integer; var HSpcL, HSpcR: integer): integer;
    function GetControlCountAt(Posn: integer): integer;
    procedure Decrement(N: integer);
  end;

implementation

uses
  SysUtils, {$IFDEF Delphi6_Plus}Variants, {$ENDIF}Forms, StdCtrls, Math;

destructor TOptionObj.Destroy;
begin
  Attributes.Free;
  inherited;
end;

function ThtOptionStringList.GetValue(Index: integer): ThtString;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TOptionObj(Objects[Index]).Value
  else
    Result := '';
end;

function ThtOptionStringList.GetSelected(Index: integer): boolean;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TOptionObj(Objects[Index]).Selected
  else
    Result := False;
end;

procedure ThtOptionStringList.SetSelected(Index: integer; Value: boolean);
begin
  if (Index >= 0) and (Index < Count) then
    TOptionObj(Objects[Index]).Selected := Value;
end;

function ThtOptionStringList.GetAttribute(Index: integer; const AttrName: string): string;
begin
  if (Index >= 0) and (Index < Count) and Assigned(TOptionObj(Objects[Index]).Attributes) then
    Result := TOptionObj(Objects[Index]).Attributes.Values[AttrName]
  else
    Result := '';
end;

destructor ThtOptionStringList.Destroy;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    TOptionObj(Objects[I]).Free;
  inherited;
end;

{----------------TListBoxFormControlObj.Create}

constructor TListBoxFormControlObj.Create(AMasterList: TSectionList;
  Position: integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  Multiple: boolean;
  PntPanel: TWinControl; //TPaintPanel;
  Tmp: TMyFont;
begin
  inherited Create(AMasterList, Position, L);
  CodePage := Prop.CodePage;
  TheOptions := ThtOptionStringList.Create;
  Multiple := L.Find(MultipleSy, T);
  if L.Find(SizeSy, T) then
    LBSize := T.Value
  else
    LBSize := -1;
  Longest := 3; {the minimum size}
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtListbox.Create(PntPanel);
  with ThtListbox(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    Parent := PntPanel;
    if (Prop.GetBorderStyle <> bssNone) then
      BorderStyle := bsNone;
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    TheFont := Font;
    Tmp.Free;
    MultiSelect := Multiple;
    ExtendedSelect := Multiple;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
{$IFDEF OpOnChange}
    OnClick := OptionalOnChange;
{$ELSE}
    OnClick := FormControlClick;
{$ENDIF}
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
  end;
end;

destructor TListBoxFormControlObj.Destroy;
begin
  TheOptions.Free;
  inherited Destroy;
end;

procedure TListboxFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited;
  if BkColor <> clNone then
    TListbox(FControl).Color := BkColor;
end;

procedure TListBoxFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I, Addon: integer;
  LB: ThtListbox;
  ARect: TRect;
begin
  LB := FControl as ThtListbox; {watch it, TListBox has a canvas too}
  if LB.BorderStyle <> bsNone then
  begin
    FormControlRect(Canvas, X1, Y1, X1 + LB.Width, Y1 + LB.Height, False, MasterList.PrintMonoBlack, False, TListbox(FControl).Color);
    Addon := 4;
  end
  else
  begin
    FillRectWhite(Canvas, X1, Y1, X1 + LB.Width, Y1 + LB.Height, TListbox(FControl).Color);
    Addon := 2;
  end;

  Canvas.Brush.Style := bsClear;
  Canvas.Font := LB.Font;
  H2 := Abs(Canvas.Font.Height);
  SetTextAlign(Canvas.handle, TA_Left + TA_Top);
  ARect := Rect(X1 + Addon, Y1 + Addon, X1 + LB.Width - 2 * Addon, Y1 + LB.Height - 2 * Addon);
  for I := LB.TopIndex to Min(LB.Items.Count - 1, LB.TopIndex + LBSize - 1) do
{$ifdef UseUnicodeControls}
    ExtTextOutW(Canvas.Handle, X1 + Addon, Y1 + Addon + (I - LB.TopIndex) * H2, ETO_CLIPPED, @ARect,
      PWideChar(LB.Items[I]), Length(LB.Items[I]), nil)
{$else}
    Canvas.TextRect(ARect, X1 + Addon, Y1 + Addon + (I - LB.TopIndex) * H2, LB.Items[I]);
{$endif}
end;

procedure TListBoxFormControlObj.AddStr(const WS: WideString; Selected: boolean;
  Attr: TStringList; CodePage: integer);
var
  Opt: TOptionObj;
  DC: HDC;
  OldFont: THandle;
  ExtS: TSize;
  S1, S2: ThtString;
begin
{$ifdef UNICODE}
  S1 := WS;
{$else}
  {$ifdef UseUnicodeControls}
  S1 := WS;
  {$else}
  S1 := WideStringToMultibyte(CodePage, WS);
  {$endif}
{$endif}
  if S1 = '' then
    S1 := ' ';
  Opt := TOptionObj.Create;
  if Assigned(Attr) then
    S2 := Attr.Values['Value']
  else
    S2 := '';
  if S2 <> '' then
    Opt.Value := S2
  else
    Opt.Value := S1;

  Opt.Selected := Selected;
  Opt.Attributes := Attr;
  TheOptions.AddObject(S1, Opt);

  DC := GetDC(0);
  try
    OldFont := SelectObject(DC, TheFont.Handle);
{$ifdef UseUnicodeControls}
    GetTextExtentPoint32W(DC, PWideChar(S1), Length(S1), ExtS);
{$else}
    GetTextExtentPoint32(DC, PChar(S1), Length(S1), ExtS);
{$endif}
    SelectObject(DC, OldFont);
  finally
    ReleaseDC(0, DC);
  end;
  Longest := Max(Longest, ExtS.cx);
end;

procedure TListBoxFormControlObj.ResetToValue;
var
  I: Integer;
  Tmp: boolean;
begin
  with (FControl as ThtListbox) do
  begin
    Items.Clear;
    for I := 0 to TheOptions.Count - 1 do
    begin
      Items.Add(TheOptions[I]);
      Tmp := TheOptions.Selected[I];
      if MultiSelect then
        Selected[I] := Tmp
      else if Tmp then
        ItemIndex := I;
    end;
    if ItemIndex < 0 then
      ItemIndex := 0;
    TopIndex := 0;
  end;
end;

procedure TListBoxFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with ThtListbox(FControl) do
  begin
    Canvas.Font := Font;
    if LBSize = -1 then
      LBSize := Max(1, Min(8, TheOptions.Count));
    if FHeight >= 10 then
      ClientHeight := FHeight
    else
      ClientHeight := Canvas.TextHeight('A') * LBSize;
    if not PercentWidth then
      if (FWidth >= 10) then
        Width := FWidth
      else
        Width := Longest + GetSystemMetrics(sm_cxvscroll) + 10
    else
    begin
      Left := -4000; {percent width set later}
      Width := 10;
    end;
  end;
end;

function TListBoxFormControlObj.GetSubmission(Index: integer; var S: ThtString): boolean;
begin
  with (FControl as ThtListbox) do
  begin
    Result := (Index < Items.Count);
    if Result then
    begin
      S := '';
      if MultiSelect and Selected[Index] or not MultiSelect and (ItemIndex = Index) then
        S := Self.FName + '=' + TheOptions.Value[Index];
    end;
  end;
end;

procedure TListBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
var
  I: integer;
begin
  with ThtListbox(FControl) do
  begin
    EnterItems := Items.Count;
    for I := 0 to Min(Items.Count - 1, 50) do
      EnterSelected[I] := Selected[I];
  end;
end;

procedure TListBoxFormControlObj.DoOnChange;
var
  I: integer;
  Changed: boolean;
begin
  Changed := False;
  with ThtListbox(FControl) do
  begin
    if Items.Count <> EnterItems then
      Changed := True
    else
      for I := 0 to Min(Items.Count - 1, 50) do
        if EnterSelected[I] <> Selected[I] then
        begin
          Changed := True;
          Break;
        end;
  end;
  if Changed then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

{$IFDEF OpOnChange}

procedure TListBoxFormControlObj.OptionalOnChange(Sender: TObject);
var
  Pt: TPoint;
begin
  DoOnChange;
  SaveContents;
  if GetCursorPos(Pt) and (WindowFromPoint(Pt) = TheControl.Handle) then
    FormControlClick(Self);
end;
{$ENDIF}

procedure TListBoxFormControlObj.SetData(Index: integer; const V: ThtString);
var
  I: integer;
  LB: ThtListbox;
begin
  LB := FControl as ThtListbox;
  if Index = 0 then
    LB.ItemIndex := 0;
  for I := 0 to TheOptions.Count - 1 do
  begin
    if Index = 0 then
    begin
      with LB do
        if MultiSelect then
          Selected[I] := False;
    end;
    if htCompareText(V, TheOptions.Value[I]) = 0 then
    begin
      with LB do
        if MultiSelect then
          Selected[I] := True
        else
          ItemIndex := I;
    end;
  end;
  LB.TopIndex := 0;
end;

{----------------TComboFormControlObj.Create}

constructor TComboFormControlObj.Create(AMasterList: TSectionList;
  Position: integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
  Tmp: TMyFont;
begin
  inherited Create(AMasterList, Position, L, Prop);
  CodePage := Prop.CodePage;
  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  PntPanel.RemoveControl(FControl);
  FControl.Free; {don't want the inherited one}
  FControl := ThtCombobox.Create(PntPanel);
  with ThtCombobox(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    TheFont := Font;
    Tmp.Free;
    Style := csDropDownList;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
{$IFDEF OpOnChange}
    OnChange := OptionalOnChange;
{$ELSE}
    OnClick := FormControlClick;
{$ENDIF}
{$IFDEF UseElpack} {others don't have onmousemove}
    OnMouseMove := HandleMouseMove;
{$ENDIF}
    OnDropDown := FormControlClick;
    Enabled := not Disabled;
  end;
  FControl.Parent := PntPanel;
end;

procedure TComboFormControlObj.ResetToValue;
var
  I: Integer;
begin
  with (FControl as ThtCombobox) do
  begin
    Items.Clear;
    for I := 0 to TheOptions.Count - 1 do
    begin
      Items.Add(TheOptions[I]);
      if TheOptions.Selected[I] then
        ItemIndex := I;
    end;
    if ItemIndex < 0 then
      ItemIndex := 0;
  end;
end;

procedure TComboFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited;
  if BkColor <> clNone then
    TCombobox(FControl).Color := BkColor;
end;

procedure TComboFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  CB: ThtCombobox;
  ARect: TRect;
begin
  CB := FControl as ThtCombobox; {watch it, TComboBox has a canvas too}
  FormControlRect(Canvas, X1, Y1, X1 + CB.Width, Y1 + CB.Height, False, MasterList.PrintMonoBlack, False, TCombobox(FControl).Color);
  Canvas.Brush.Style := bsClear;
  Canvas.Font := CB.Font;
  SetTextAlign(Canvas.handle, TA_Left + TA_Top);
  ARect := Rect(X1 + 4, Y1 + 4, X1 + CB.Width - 8, Y1 + CB.Height - 3);
{$ifdef UseUnicodeControls}
  ExtTextOutW(Canvas.Handle, X1 + 4, Y1 + 4, ETO_CLIPPED, @ARect,
    PWideChar(CB.Items[CB.ItemIndex]), Length(CB.Items[CB.ItemIndex]), nil)
{$else}
  Canvas.TextRect(ARect, X1 + 4, Y1 + 4, CB.Items[CB.ItemIndex]);
{$endif}
end;

procedure TComboFormControlObj.SetHeightWidth(ACanvas: TCanvas);
begin
  with ThtCombobox(FControl) do
  begin
    if FHeight >= 10 then
      Height := FHeight;
    if not PercentWidth then
      if (FWidth >= 10) and not PercentWidth then
        Width := FWidth
      else
        ClientWidth := Longest + GetSystemMetrics(sm_cxvscroll) + 10
    else
    begin
      Left := -4000; {percent width set later}
      Width := 10;
    end;
  end;
end;

function TComboFormControlObj.GetSubmission(Index: integer; var S: ThtString): boolean;
begin
  Result := Index = 0;
  if Result then
    with (FControl as ThtCombobox) do
      if (ItemIndex >= 0) and (ItemIndex <= Items.Count) then
        S := Self.FName + '=' + TheOptions.Value[ItemIndex];
end;

procedure TComboFormControlObj.SetData(Index: integer; const V: ThtString);
var
  CB: ThtCombobox;
  I: integer;
begin
  CB := FControl as ThtCombobox;
  for I := 0 to TheOptions.Count - 1 do
  begin
    if htCompareText(V, TheOptions.Value[I]) = 0 then
      CB.ItemIndex := I;
  end;
end;

procedure TComboFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterIndex := ThtCombobox(FControl).ItemIndex;
end;

{$IFDEF OpOnChange}

procedure TComboFormControlObj.OptionalOnChange(Sender: TObject);
begin
  if ThtCombobox(FControl).ItemIndex <> EnterIndex then
  begin
    SaveContents;
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
  end;
end;
{$ENDIF}

procedure TComboFormControlObj.DoOnChange;
begin
{$IFNDEF OpOnChange}
  if ThtCombobox(FControl).ItemIndex <> EnterIndex then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
{$ENDIF}
end;

{----------------TTextAreaFormControlObj.Create}

constructor TTextAreaFormControlObj.Create(AMasterList: TSectionList;
  Position: integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: integer;
  SB: TScrollStyle;
  Tmp: TMyFont;
begin
  inherited Create(AMasterList, Position, L);
  CodePage := Prop.CodePage;
  Rows := 5;
  Cols := 30;
  Wrap := wrSoft;
  SB := StdCtrls.ssVertical;

  for I := 0 to L.Count - 1 do
    with TAttribute(L[I]) do
      case Which of
        RowsSy: Rows := Value;
        ColsSy: Cols := Value;
        WrapSy:
          if (Lowercase(Name) = 'off') then
          begin
            SB := StdCtrls.ssBoth;
            Wrap := wrOff;
          end
          else if (Lowercase(Name) = 'hard') then
          begin
            Wrap := wrHard;
          end;
      end;

  PntPanel := {TPaintPanel(}AMasterList.PPanel{)};
  FControl := ThtMemo.Create(PntPanel);
  with ThtMemo(FControl) do
  begin
    Left := -4000; {so will be invisible until placed}
    if (Prop.GetBorderStyle <> bssNone) then
      BorderStyle := bsNone;
    Tmp := Prop.GetFont;
    Font.Assign(Tmp);
    Tmp.Free;
    ScrollBars := SB;
    Wordwrap := Wrap in [wrSoft, wrHard];
    OnKeyPress := MyForm.ControlKeyPress;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnClick := FormControlClick;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
    ReadOnly := Self.Readonly;
  end;
  FControl.Parent := PntPanel;
end;

destructor TTextAreaFormControlObj.Destroy;
begin
  inherited Destroy;
end;

procedure TTextAreaFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited;
  if BkColor <> clNone then
    TMemo(FControl).Color := BkColor;
end;

procedure TTextAreaFormControlObj.Draw(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I, Addon: integer;
  ARect: TRect;
begin
  with ThtMemo(FControl) do
  begin
    if BorderStyle <> bsNone then
    begin
      FormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, MasterList.PrintMonoBlack, False, TMemo(FControl).Color);
      Addon := 4;
    end
    else
    begin
      FillRectWhite(Canvas, X1, Y1, X1 + Width, Y1 + Height, TMemo(FControl).Color);
      Addon := 2;
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Font;
    H2 := Canvas.TextHeight('A');
    SetTextAlign(Canvas.handle, TA_Left + TA_Top);
    ARect := Rect(X1 + Addon, Y1 + Addon, X1 + Width - 2 * Addon, Y1 + Height - 2 * Addon);
    for I := 0 to Min(Lines.Count - 1, Rows - 1) do
{$ifdef UseUnicodeControls}
      ExtTextOutW(Canvas.Handle, X1 + Addon, Y1 + Addon + I * H2, ETO_CLIPPED, @ARect,
        PWideChar(Lines[I]), Length(Lines[I]), nil)
{$else}
      Canvas.TextRect(ARect, X1 + Addon, Y1 + Addon + I * H2, Lines[I]);
{$endif}
  end;
end;

procedure TTextAreaFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with ThtMemo(FControl) do
  begin
    Canvas.Font := Font;
    if FHeight >= 10 then
      Height := FHeight
    else
      ClientHeight := Canvas.TextHeight('A') * Rows + 5;
    if not PercentWidth then
    begin
      if (FWidth >= 10) then {percent width set later}
        Width := FWidth
      else
        ClientWidth := Canvas.TextWidth('s') * Cols + 5;
    end
    else
    begin
      Left := -4000;
      Width := 50;
    end;
  end;
end;

procedure TTextAreaFormControlObj.AddStr(const S: ThtString);
begin
  TheText := TheText + S;
end;

procedure TTextAreaFormControlObj.ResetToValue;
begin
  with (FControl as ThtMemo) do
  begin
    Text := TheText;
    SelStart := 0;
    SelLength := 0;
  end;
end;

function TTextAreaFormControlObj.GetSubmission(Index: integer; var S: ThtString): boolean;
var
  I: integer;
begin
  Result := Index = 0;
  if Result then
  begin
    S := FName + '=';
    if Wrap in [wrOff, wrSoft] then
      S := S + (FControl as ThtMemo).Text
    else
      with (FControl as ThtMemo) do
        for I := 0 to Lines.Count - 1 do
        begin
          S := S + Lines[I];
          if (I < Lines.Count - 1) then
            S := S + CRLF;
        end;
  end;
end;

procedure TTextAreaFormControlObj.SetData(Index: integer; const V: ThtString);
begin
  (FControl as ThtMemo).Text := V;
end;

procedure TTextAreaFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterContents := ThtMemo(FControl).Text;
end;

procedure TTextAreaFormControlObj.DoOnChange;
begin
  if ThtMemo(FControl).Text <> EnterContents then
    if Assigned(MasterList.ObjectChange) then
      MasterList.ObjectChange(MasterList.TheOwner, Self, OnChangeMessage);
end;

function TFormControlList.FindControl(Posn: integer): TFormControlObj;
{find the control at a given character position}
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    if TFormControlObj(Items[I]).Pos = Posn then
    begin
      Result := Items[I];
      Exit;
    end;
  Result := nil;
end;

function TFormControlList.GetHeightAt(Posn: integer;
  var FormAlign: AlignmentType): Integer;
var
  Ctrl: TFormControlObj;
begin
  Ctrl := FindControl(Posn);
  if Assigned(Ctrl) then
  begin
    Result := Ctrl.FControl.Height;
    FormAlign := Ctrl.FormAlign;
  end
  else
    Result := -1;
end;

function TFormControlList.GetWidthAt(Posn: integer; var HSpcL, HSpcR: integer): integer;
var
  Ctrl: TFormControlObj;
begin
  Ctrl := FindControl(Posn);
  if Assigned(Ctrl) then
  begin
    Result := Ctrl.FControl.Width;
    HSpcL := Ctrl.HSpaceL;
    HSpcR := Ctrl.HSpaceR;
  end
  else
    Result := -1;
end;

function TFormControlList.GetControlCountAt(Posn: integer): integer;
{Return count of chars before the next form control.  0 if at the control,
 9999 if no controls after Posn}
var
  I, Pos: integer;
begin
  if Count = 0 then
  begin
    Result := 9999;
    Exit;
  end;
  I := 0;
  while I < count do
  begin
    Pos := TFormControlObj(Items[I]).Pos;
    if Pos >= Posn then
      break;
    Inc(I);
  end;
  if I = Count then
    Result := 9999
  else
    Result := TFormControlObj(Items[I]).Pos - Posn;
end;

procedure TFormControlList.Decrement(N: integer);
{called when a character is removed to change the Position figure}
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    with TFormControlObj(Items[I]) do
      if Pos > N then
        Dec(Pos);
end;

end.
