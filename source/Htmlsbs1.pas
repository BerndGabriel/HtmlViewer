{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2023 by HtmlViewer Team

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
{$ifdef VCL}
  Windows,
{$endif}
  Messages,
{$ifdef LCL}
  LclIntf, LclType, Types, LclProc, HtmlMisc,
{$endif}
{$ifdef scrollbarInClasses}
  System.Classes,
{$else}
  Classes,
{$endif}
  Graphics, Controls,
  //
  HtmlGlobals,
  HtmlImages,
  HtmlFonts,
  HtmlSymb,
  HTMLUn2,
  HTMLSubs,
  StyleTypes,
  StyleUn;

type
  TTypedFormControlObj = class(TFormControlObj)
  protected
    FInputType : THtString;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    property InputType : THtString read FInputType;
  end;

  THiddenFormControlObj = class(TFormControlObj)
  protected
    function GetControl: TWinControl; override;
    function GetClientHeight: Integer; override;
    function GetClientLeft: Integer; override;
    function GetTabOrder: Integer; override;
    function GetTabStop: Boolean; override;
    function GetClientTop: Integer; override;
    function GetClientWidth: Integer; override;
    function IsHidden: Boolean; override;
    procedure SetClientHeight(Value: Integer); override;
    procedure SetClientLeft(Value: Integer); override;
    procedure SetTabOrder(Value: Integer); override;
    procedure SetTabStop(Value: Boolean); override;
    procedure SetClientTop(Value: Integer); override;
    procedure SetClientWidth(Value: Integer); override;
  public
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; override;
    procedure Hide; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure Show; override;
  end;

  TEditBaseFormControlObj = class(TTypedFormControlObj)
  protected
    FPlaceholder : ThtString;
    FSpellCheck : Boolean;
    FMaxLength : Integer;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    property Placeholder : ThtString read FPlaceholder;
    property SpellCheck : Boolean read FSpellCheck;
    property MaxLength : Integer read FMaxLength;
  end;
  
  TEditFormControlObj = class(TEditBaseFormControlObj)
  private
    FControl: ThtEdit;
    EnterContents: ThtString;
    tmAveCharWidth: Integer;

    function getText: ThtString;
    procedure setText(const Value: ThtString);
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    EditSize: Integer;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    property Text: ThtString read getText write setText;
  end;

  TWhichType = (Submit, ResetB, Button, Browse);

  TButtonFormControlObj = class(TTypedFormControlObj)
  private
    FControl: ThtButton;
  protected
    function GetControl: TWinControl; override;
  public
    Which: TWhichType;
    MyEdit: TEditFormControlObj;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure ButtonClick(Sender: TObject);
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TOptionObj = class(TObject) {used by TListBoxFormControlObj for <option> information}
  public
    Value: ThtString; {<option>  Value=  }
    Selected: Boolean; {set if Selected found in <option>}
    Attributes: ThtStringList; {list of <option> attributes}
    destructor Destroy; override;
  end;

  ThtOptionStringList = class(ThtStringList)
  private
    function GetValue(Index: integer): ThtString;
    function GetSelected(Index: integer): Boolean;
    procedure SetSelected(Index: integer; Value: Boolean);
    function GetAttribute(Index: integer; const AttrName: ThtString): ThtString;
  public
    property Value[Index: integer]: ThtString read GetValue;
    property Selected[Index: integer]: Boolean read GetSelected write SetSelected;
    property AttributeValue[Index: integer; const AttrName: ThtString]: ThtString read GetAttribute;
    destructor Destroy; override;
  end;

  //BG, 16.01.2011:
  TOptionsFormControlObj = class(TFormControlObj)
  private
    FOptions: ThtOptionStringList;
    FFont: TFont;
    LBSize, Longest: integer;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    procedure AddStr(const S: ThtString; Selected: Boolean; Attr: ThtStringList; CodePage: integer);
    property TheOptions: ThtOptionStringList read FOptions;
    property TheFont: TFont read FFont write FFont;
    {TheOptions is the original Options list for reseting.  It does not reflect
     the current selected state.  The Strings part is the Items in the list/combobox.
     The Options part is TOptionObj defined above}
  end;

  TListBoxFormControlObj = class(TOptionsFormControlObj)
  {Select with "Multiple" or Size > 1}
  private
    FControl: ThtListbox;
    EnterItems: integer;
    EnterSelected: array[0..50] of Boolean;
{$IFDEF OpOnChange}
    procedure OptionalOnChange(Sender: TObject);
{$ENDIF}
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: integer; out S: ThtString): Boolean; override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    procedure SetData(Index: integer; const V: ThtString); override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
  end;

  TFormCheckBox = class(ThtCheckBox)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;

  TCheckBoxFormControlObj = class(TFormControlObj)
  private
    FControl: TFormCheckBox;
    WasChecked: Boolean;
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    IsChecked: Boolean;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: Integer; out S: ThtString): Boolean; override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: Integer); override;
    procedure ResetToValue; override;
    procedure SetData(Index: Integer; const V: ThtString); override;
    procedure SetDataInit; override;
    property Checked: Boolean read GetChecked write SetChecked;
  end;

  TComboFormControlObj = class(TOptionsFormControlObj)
  {Select with single selection and Size = 1}
  private
    FControl: ThtCombobox;
    EnterIndex: integer;
    FPlaceholder : ThtString;
{$IFDEF OpOnChange}
    procedure OptionalOnChange(Sender: TObject);
{$ENDIF}
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: integer; out S: ThtString): Boolean; override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: integer); override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure ResetToValue; override;
    procedure SetData(Index: integer; const V: ThtString); override;
    procedure SetHeightWidth(ACanvas: TCanvas); override;
  end;

  { TTextAreaFormControlObj }

  TTextAreaFormControlObj = class(TEditBaseFormControlObj)
  private
    FControl: ThtMemo;
    EnterContents: ThtString;

    function GetLine(Index: Integer): ThtString;
    function GetText: ThtString;
    procedure SetText(const AValue: ThtString);
  protected
    function GetControl: TWinControl; override;
    procedure DoOnChange; override;
    procedure SaveContents; override;
  public
    Wrap: (wrOff, wrSoft, wrHard);
    Rows, Cols: integer;
    TheText: ThtString;
    constructor Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties); override;
    constructor CreateCopy(Parent: TCellBasic; Source: THtmlNode); override;
    destructor Destroy; override;
    function GetSubmission(Index: integer; out S: ThtString): Boolean; override;
    procedure ProcessProperties(Prop: TProperties); override;
    procedure DrawInline1(Canvas: TCanvas; X1, Y1: integer); override;
    procedure AddStr(const S: ThtString);
    procedure ResetToValue; override;
    procedure SetHeightWidth(Canvas: TCanvas); override;
    procedure SetData(Index: integer; const V: ThtString); override;
    property Lines[Index: Integer]: ThtString read GetLine;
    property Text: ThtString read GetText write SetText;
  end;

implementation

uses
  {$ifdef UseVCLStyles}
  Vcl.Themes,
  {$endif}
  {$ifdef  HasSystemUITypes}
  System.UITypes,
  {$endif}
  SysUtils, Variants, Forms, StdCtrls, Math;

destructor TOptionObj.Destroy;
begin
  Attributes.Free;
  inherited Destroy;
end;

function ThtOptionStringList.GetValue(Index: integer): ThtString;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TOptionObj(Objects[Index]).Value
  else
    Result := '';
end;

function ThtOptionStringList.GetSelected(Index: integer): Boolean;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TOptionObj(Objects[Index]).Selected
  else
    Result := False;
end;

procedure ThtOptionStringList.SetSelected(Index: integer; Value: Boolean);
begin
  if (Index >= 0) and (Index < Count) then
    TOptionObj(Objects[Index]).Selected := Value;
end;

function ThtOptionStringList.GetAttribute(Index: integer; const AttrName: ThtString): ThtString;
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
  inherited Destroy;
end;

{----------------TTypedFormControlObj.Create}
constructor TTypedFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
begin
  inherited Create(Parent, Position, L, Prop);
  T := nil;
  if L.Find(TypeSy, T) then
      FInputType := htLowerCase(T.Name);
end;

constructor TTypedFormControlObj.CreateCopy(Parent: TCellBasic;
  Source: THtmlNode);
var
  T: TTypedFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FInputType := T.InputType;
end;

{ TEditBaseFormControlObj }

constructor TEditBaseFormControlObj.Create(Parent: TCellBasic;
  Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
begin
  inherited Create(Parent, Position, L, Prop);
  T := nil;
  if L.Find(MaxLengthSy, T) then
    FMaxLength := T.Value;
  if L.Find(PlaceholderSy, T) then
  begin
    FPlaceholder := T.Name;
  end;
  // default is true
  FSpellCheck := True;
  if L.Find(SpellCheckSy,T) then begin
    if Lowercase(T.Name) = 'false' then begin
      FSpellCheck := False;
    end;
  end;
end;

constructor TEditBaseFormControlObj.CreateCopy(Parent: TCellBasic;
  Source: THtmlNode);
var
  T: TEditBaseFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent, Source);
  FMaxLength := T.MaxLength;
  FPlaceholder := T.Placeholder;
  FSpellCheck := T.SpellCheck;
end;

{----------------TListBoxFormControlObj.Create}

constructor TListBoxFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  Multiple: Boolean;
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(Parent, Position, L, Prop);
  T := nil;
  CodePage := Prop.CodePage;
  Multiple := L.Find(MultipleSy, T);
  PntPanel := Document.PPanel;
  FControl := ThtListbox.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Parent := PntPanel;
    if Prop.HasBorderStyle then
      BorderStyle := bsNone;
    Font.Assign(Prop.Font);
    TheFont := Font;
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
{$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
{$endif}
  end;
end;

//-- BG ---------------------------------------------------------- 30.08.2013 --
constructor TListBoxFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TListBoxFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  EnterItems := T.EnterItems;
  EnterSelected := T.EnterSelected;
end;

destructor TListBoxFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TListboxFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited ProcessProperties(Prop);
  if BkColor <> clNone then
    FControl.Color := BkColor;
end;

procedure TListBoxFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I, Addon: integer;
  ARect: TRect;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
  begin
    ARect := Rect(X1, Y1, X1 + FControl.Width, Y1 + FControl.Height);
    if FControl.BorderStyle <> bsNone then
    begin
      DrawFormControlRect(Canvas, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, False, Document.PrintMonoBlack, False, FControl.Color, Document.ThemedColorToRGB);
      Addon := 4;
    end
    else
    begin
      FillRectWhite(Canvas, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, Document.ThemedColorToRGB(FControl.Color, htseClient));
      Addon := 2;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font := FControl.Font;
    Canvas.Font.Color := Document.ThemedColorToRGB( FControl.Font.Color, htseFont);
    H2 := Abs(Canvas.Font.Height);
    SetTextAlign(Canvas.Handle, TA_Left + TA_Top);
    InflateRect(ARect, -Addon, -Addon);
    for I := FControl.TopIndex to Min(FControl.Items.Count - 1, FControl.TopIndex + LBSize - 1) do
      ThtCanvas(Canvas).htTextRect(
        ARect, ARect.Left, ARect.Top + (I - FControl.TopIndex) * H2,
        {$ifdef LCL}Utf8Decode{$endif}(FControl.Items[I]));
  end;
end;

procedure TListBoxFormControlObj.ResetToValue;
var
  I: Integer;
  Tmp: Boolean;
begin
  with FControl do
  begin
    Items.Clear;
    for I := 0 to TheOptions.Count - 1 do
    begin
      Items.Add({$ifdef LCL}Utf8Encode{$endif}(TheOptions[I]));
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
  with FControl do
  begin
    Canvas.Font := Font;
    Canvas.Font.Color := Document.ThemedColorToRGB( Canvas.Font.Color, htseFont);

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

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TListBoxFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TListBoxFormControlObj.GetSubmission(Index: integer; out S: ThtString): Boolean;
begin
  with FControl do
  begin
    Result := (Index < Items.Count);
    if Result then
    begin
      S := '';
      if MultiSelect and Selected[Index] or not MultiSelect and (ItemIndex = Index) then
        S := Self.Name + '=' + TheOptions.Value[Index];
    end;
  end;
end;

procedure TListBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
var
  I: integer;
begin
  with FControl do
  begin
    EnterItems := Items.Count;
    for I := 0 to Min(Items.Count - 1, 50) do
      EnterSelected[I] := Selected[I];
  end;
end;

procedure TListBoxFormControlObj.DoOnChange;
var
  I: integer;
  Changed: Boolean;
begin
  Changed := False;
  with FControl do
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
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
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
begin
  if Index = 0 then
    FControl.ItemIndex := 0;
  for I := 0 to TheOptions.Count - 1 do
  begin
    if Index = 0 then
    begin
      if FControl.MultiSelect then
        FControl.Selected[I] := False;
    end;
    if htCompareText(V, TheOptions.Value[I]) = 0 then
    begin
      if FControl.MultiSelect then
        FControl.Selected[I] := True
      else
        FControl.ItemIndex := I;
    end;
  end;
  FControl.TopIndex := 0;
end;

{----------------TComboFormControlObj.Create}

constructor TComboFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
  T : TAttribute;
begin
  inherited Create(Parent, Position, L, Prop);
  T := nil;
  CodePage := Prop.CodePage;
  PntPanel := Document.PPanel;
  FControl := ThtCombobox.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Font.Assign(Prop.Font);
    TheFont := Font;
    Style := csDropDownList;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
{$ifdef OpOnChange}
    OnChange := OptionalOnChange;
{$else}
    OnClick := FormControlClick;
{$endif}
{$ifdef UseElPack} {others don't have onmousemove}
    OnMouseMove := HandleMouseMove;
{$endif}
    OnDropDown := FormControlClick;
    Enabled := not Disabled;
{$ifdef has_StyleElements}
    FControl.StyleElements := Document.StyleElements;
{$endif}
  end;
  FControl.Parent := PntPanel;
  if L.Find(PlaceholderSy, T) then
  begin
    FPlaceholder := T.Name;
    FControl.TextHint := FPlaceholder;
  end;
end;

procedure TComboFormControlObj.ResetToValue;
var
  I: Integer;
begin
  with FControl do
  begin
    Items.Clear;
    for I := 0 to TheOptions.Count - 1 do
    begin
      Items.Add({$ifdef LCL}Utf8Encode{$endif}(TheOptions[I]));
      if TheOptions.Selected[I] then
        ItemIndex := I;
    end;
    if ItemIndex < 0 then
      ItemIndex := 0;
  end;
end;

procedure TComboFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited  ProcessProperties(Prop);
  if BkColor <> clNone then
    FControl.Color := BkColor;
end;

procedure TComboFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: integer);
var
  ARect: TRect;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
  begin
    ARect := Rect(X1, Y1, X1 + FControl.Width, Y1 + FControl.Height);
    DrawFormControlRect(Canvas, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, False, Document.PrintMonoBlack, False, FControl.Color, Document.ThemedColorToRGB);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := FControl.Font;
    Canvas.Font.Color := Document.ThemedColorToRGB(FControl.Font.Color, htseFont);
    SetTextAlign(Canvas.Handle, TA_Left + TA_Top);
    InflateRect(ARect, -4, -4);
    Inc(ARect.Bottom, 5);
    ThtCanvas(Canvas).htTextRect(ARect, ARect.Left, ARect.Top, {$ifdef LCL}Utf8Decode{$endif}(FControl.Items[FControl.ItemIndex]));
  end;
end;

procedure TComboFormControlObj.SetHeightWidth(ACanvas: TCanvas);
begin
  with FControl do
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

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TComboFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TComboFormControlObj.GetSubmission(Index: integer; out S: ThtString): Boolean;
begin
  Result := Index = 0;
  if Result then
    with FControl do
      if (ItemIndex >= 0) and (ItemIndex <= Items.Count) then
        S := Self.Name + '=' + TheOptions.Value[ItemIndex];
end;

procedure TComboFormControlObj.SetData(Index: integer; const V: ThtString);
var
  I: integer;
begin
  for I := 0 to TheOptions.Count - 1 do
  begin
    if htCompareText(V, TheOptions.Value[I]) = 0 then
      FControl.ItemIndex := I;
  end;
end;

procedure TComboFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterIndex := FControl.ItemIndex;
end;

{$IFDEF OpOnChange}

procedure TComboFormControlObj.OptionalOnChange(Sender: TObject);
begin
  if FControl.ItemIndex <> EnterIndex then
  begin
    SaveContents;
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
  end;
end;
{$ENDIF}

//-- BG ---------------------------------------------------------- 30.08.2013 --
constructor TComboFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TComboFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  EnterIndex := T.EnterIndex;
  FPlaceholder := T.FPlaceholder;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
destructor TComboFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TComboFormControlObj.DoOnChange;
begin
{$IFNDEF OpOnChange}
  if FControl.ItemIndex <> EnterIndex then
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
{$ENDIF}
end;

{----------------TTextAreaFormControlObj.Create}

constructor TTextAreaFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
  I: integer;
  SB: ThtScrollStyle;
begin
  inherited Create(Parent,Position,L,Prop);
  CodePage := Prop.CodePage;
  Rows := 5;
  Cols := 30;
  Wrap := wrSoft;
  SB := StdCtrls.ssVertical;

  for I := 0 to L.Count - 1 do
    with L[I] do
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

  PntPanel := Document.PPanel;
  FControl := ThtMemo.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    if Prop.HasBorderStyle then
      BorderStyle := bsNone;
    Font.Assign(Prop.Font);
    ScrollBars := SB;
    Wordwrap := Wrap in [wrSoft, wrHard];
    OnKeyPress := MyForm.ControlKeyPress;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnClick := FormControlClick;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
    ReadOnly := Self.Readonly;
     {$ifdef has_StyleElements}
    FControl.StyleElements := Document.StyleElements;
     {$endif}
  end;
  FControl.Parent := PntPanel;
  FControl.TextHint.Clear;
  if FPlaceholder <> '' then begin
    FControl.TextHint.Add( htStringToString(FPlaceholder));
  end;
  if FMaxLength <> 0 then begin
    FControl.MaxLength := FMaxLength;
  end;
end;

constructor TTextAreaFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TTextAreaFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  EnterContents := T.EnterContents;
  Wrap := T.Wrap;
  Rows := T.Rows;
  Cols := T.Cols;
  TheText := T.TheText;
end;

destructor TTextAreaFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TTextAreaFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited ProcessProperties(Prop);
  if BkColor <> clNone then
    FControl.Color := BkColor;
end;

procedure TTextAreaFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: integer);
var
  H2, I, Addon: integer;
  ARect: TRect;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
    with FControl do
    begin
      if BorderStyle <> bsNone then
      begin
        DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, Document.PrintMonoBlack, False, FControl.Color, Document.ThemedColorToRGB);
        Addon := 4;
      end
      else
      begin
        FillRectWhite(Canvas, X1, Y1, X1 + Width, Y1 + Height, Document.ThemedColorToRGB(FControl.Color, htseClient));
        Addon := 2;
      end;
      Canvas.Brush.Style := bsClear;
      Canvas.Font := Font;
      H2 := Canvas.TextHeight('A');
      SetTextAlign(Canvas.handle, TA_Left + TA_Top);
      ARect := Rect(X1 + Addon, Y1 + Addon, X1 + Width - 2 * Addon, Y1 + Height - 2 * Addon);
      for I := 0 to Min(Lines.Count - 1, Rows - 1) do
        ThtCanvas(Canvas).htTextRect(ARect, X1 + Addon, Y1 + Addon + I * H2, htString(Lines[I]));
    end;
end;

procedure TTextAreaFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with FControl do
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
  htAppendStr(TheText, S);
end;

procedure TTextAreaFormControlObj.ResetToValue;
begin
  with FControl do
  begin
    SelStart := 0;
    SelLength := 0;
  end;
  Text := TheText;
end;

function TTextAreaFormControlObj.GetLine(Index: Integer): ThtString;
begin
  Result := {$ifdef LCL}Utf8Decode{$endif}(FControl.Lines[Index]);
end;

function TTextAreaFormControlObj.GetText: ThtString;
begin
  Result := {$ifdef LCL}Utf8Decode{$endif}(FControl.Text);
end;

procedure TTextAreaFormControlObj.SetText(const AValue: ThtString);
begin
  FControl.Text := {$ifdef LCL}Utf8Encode{$endif}(AValue);
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TTextAreaFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TTextAreaFormControlObj.GetSubmission(Index: integer; out S: ThtString): Boolean;
var
  I: integer;
begin
  Result := Index = 0;
  if Result then
  begin
    S := Name + '=';
    if Wrap in [wrOff, wrSoft] then
      S := S + Text
    else
      for I := 0 to FControl.Lines.Count - 1 do
      begin
        S := S + Lines[I];
        if I < FControl.Lines.Count - 1 then
          S := S + CRLF;
      end;
  end;
end;

procedure TTextAreaFormControlObj.SetData(Index: integer; const V: ThtString);
begin
  Text := V;
end;

procedure TTextAreaFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterContents := Text;
end;

procedure TTextAreaFormControlObj.DoOnChange;
begin
  if Text <> EnterContents then
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{ TOptionsFormControlObj }

procedure TOptionsFormControlObj.AddStr(const S: ThtString; Selected: Boolean;
  Attr: ThtStringList; CodePage: integer);
var
  Opt: TOptionObj;
  DC: HDC;
  OldFont: HFONT;
  ExtS: TSize;
  S1, S2: ThtString;
begin
  S1 := S;
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
    GetTextExtentPoint32W(DC, PWideChar(S1), Length(S1), ExtS);
    SelectObject(DC, OldFont);
  finally
    ReleaseDC(0, DC);
  end;
  Longest := Max(Longest, ExtS.cx);
end;

constructor TOptionsFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
begin
  inherited Create(Parent, Position, L, Prop);
  FOptions := ThtOptionStringList.Create;
  T := nil;
  if L.Find(SizeSy, T) then
    LBSize := T.Value
  else
    LBSize := -1;
  Longest := 3; {the minimum size}
end;

//-- BG ---------------------------------------------------------- 29.08.2013 --
constructor TOptionsFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TOptionsFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FOptions := T.FOptions;
  FFont := T.FFont;
  LBSize := T.LBSize;
  Longest := T.Longest;
end;

destructor TOptionsFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FOptions.Free;
  end;
  inherited Destroy;
end;

{----------------THiddenFormControlObj.GetSubmission}

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetControl: TWinControl;
begin
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetClientHeight: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetClientLeft: Integer;
begin
  Result := -4000;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
begin
  Result := Index = 0;
  if Result then
    S := Name + '=' + Value;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetTabOrder: Integer;
begin
  Result := -1;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function THiddenFormControlObj.GetTabStop: Boolean;
begin
  Result := False;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetClientTop: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.GetClientWidth: Integer;
begin
  Result := 0;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.Hide;
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function THiddenFormControlObj.IsHidden: Boolean;
begin
  Result := True;
end;

procedure THiddenFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  Value := V;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetClientHeight(Value: Integer);
begin
// do nothing
end;

procedure THiddenFormControlObj.SetClientLeft(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure THiddenFormControlObj.SetTabOrder(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure THiddenFormControlObj.SetTabStop(Value: Boolean);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetClientTop(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.SetClientWidth(Value: Integer);
begin
// do nothing
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure THiddenFormControlObj.Show;
begin
// do nothing
end;

{----------------TEditFormControlObj.Create}

constructor TEditFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
  I: Integer;
begin
  inherited Create(Parent, Position, L, Prop);
  CodePage := Prop.CodePage;
  EditSize := 15;
  T := nil;
  if L.Find(SizeSy, T) then
  begin
    if T.Value > 0 then
      EditSize := T.Value
    else
    begin {see if it's comma delimited list}
      I := Min(System.Pos(',', T.Name), System.Pos(' ', T.Name));
      if I > 1 then
        EditSize := StrToIntDef(copy(htStringToString(T.Name), 1, I - 1), 20);
    end;
  end;
  PntPanel := Document.PPanel;
  FControl := ThtEdit.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Width := 120;
    if Prop.HasBorderStyle then
      BorderStyle := bsNone;
    Parent := PntPanel;
    Font.Assign(Prop.Font);
    FHeight := Height; {Height can change when font assigned}
    tmAveCharWidth := Prop.Font.tmAveCharWidth;
    Text := htStringToString(Value);

    if FInputType= 'password'  then begin
      PassWordChar := '*';
    end;
    OnKeyPress := MyForm.ControlKeyPress;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnClick := FormControlClick;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
    ReadOnly := Self.Readonly;
    {$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
    {$endif}
    if FPlaceholder <> '' then begin
      TextHint := FPlaceholder;
    end;
    if FMaxLength <> 0 then begin
      MaxLength := FMaxLength;
    end;
  end;
end;

procedure TEditFormControlObj.ProcessProperties(Prop: TProperties);
begin
  inherited ProcessProperties(Prop);
  if BkColor <> clNone then
    FControl.Color := BkColor;
end;

procedure TEditFormControlObj.ResetToValue;
begin
  Text := Value;
end;

procedure TEditFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: Integer);
var
  H2, Addon: Integer;
  ARect: TRect;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
  begin
    if FControl.BorderStyle <> bsNone then
      Addon := 4 {normal 3D border}
    else
      Addon := 2; {inline border, 3D Border removed}
    with FControl do
    begin
      Canvas.Font := Font;
      H2 := Abs(Font.Height);
      if BorderStyle <> bsNone then
        DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, Document.PrintMonoBlack, False, Color, Document.ThemedColorToRGB)
      else
        FillRectWhite(Canvas, X1, Y1, X1 + Width, Y1 + Height, Document.ThemedColorToRGB( Color, htseClient ));
      SetTextAlign(Canvas.handle, TA_Left);
      SetBkMode(Canvas.Handle, {$ifdef FPC}Lcltype.{$endif}Transparent);
      Canvas.Brush.Style := bsClear;
      ARect := Rect(X1 + Addon, Y1, X1 + Width - (Addon div 2), Y1 + Height);
      ThtCanvas(Canvas).htTextRect(ARect, ARect.Left, Y1 + (Height - H2) div 2 - 1, htString(Text));
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TEditFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TEditFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
begin
  Result := Index = 0;
  if Result then
    S := Name + '=' + Text;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
function TEditFormControlObj.getText: ThtString;
begin
  Result := {$ifdef LCL}UTF8Decode{$endif}(FControl.Text);
end;

procedure TEditFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  Text := V;
end;

procedure TEditFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with FControl do
  begin
    Canvas.Font := Font;
    if not PercentWidth then
      if (FWidth >= 10) then
        Width := FWidth
      else
        Width := tmAveCharWidth * EditSize + 23
    else
    begin {percent width set later}
      Left := -4000;
      Width := 10;
    end;
    Height := Max(FHeight, Max(Canvas.TextHeight('A'), 10));
  end;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
procedure TEditFormControlObj.setText(const Value: ThtString);
begin
  FControl.Text := {$ifdef LCL}Utf8Encode{$endif}(Value);
end;

procedure TEditFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  EnterContents := Text;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
constructor TEditFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TEditFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  EnterContents := T.EnterContents;
  tmAveCharWidth := T.tmAveCharWidth;
  FPlaceholder := T.FPlaceholder;
end;

destructor TEditFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TEditFormControlObj.DoOnChange;
begin
  if Text <> EnterContents then
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

{----------------TButtonFormControlObj.Create}

constructor TButtonFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(Parent, Position, L, Prop);
  if FInputType = 'submit' then
  begin
    Which := Submit;
    if Value = '' then
      Value := 'Submit';
  end
  else if FInputType = 'reset' then
  begin
    Which := ResetB;
    if Value = '' then
      Value := 'Reset';
  end
  else if FInputType = 'file' then
  begin
    Which := Browse;
    Value := '';
    Name := '';
    Id := '';
  end
  else
  begin
    Which := Button;
    if Value = '' then
      Value := 'Button';
  end;
  PntPanel := Document.PPanel;
  FControl := ThtButton.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Font.Assign(Prop.Font);
    OnClick := Self.ButtonClick;
    if Which = Browse then
      Caption := 'Browse...'
    else
      Caption := {$ifdef LCL}Utf8Encode{$endif}(Value);
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
     {$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
     {$endif}
  end;
  FControl.Parent := PntPanel;
{$ifdef UseElPack}
      FControl.Color := clBtnFace;
{$endif}
end;

//-- BG ---------------------------------------------------------- 29.08.2013 --
constructor TButtonFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TButtonFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  Which := T.Which;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TButtonFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TButtonFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: Integer);
var
  H2: Integer;
  MonoBlack: Boolean;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
    with FControl do
    begin
      MonoBlack := Document.PrintMonoBlack and (GetDeviceCaps(Canvas.Handle, BITSPIXEL) = 1) and
        (GetDeviceCaps(Canvas.Handle, PLANES) = 1);
      if not MonoBlack then
      begin
        try
          if not Assigned(PaintBitmap) then
          begin
            PaintBitmap := TBitmap.Create;
            PaintBitmap.Width := Width;
            PaintBitmap.Height := Height;
            PaintBitmap.Canvas.Lock;
            PaintTo(PaintBitmap.Canvas.Handle, 0, 0);
            PaintBitmap.Canvas.UnLock;
          end;
          PrintBitmap(Canvas, X1, Y1, Width, Height, PaintBitmap);
        except end;
      end
      else
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Font := Font;
        Canvas.Font.Color := Document.ThemedColorToRGB(Font.Color, htseFont);
        DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, True, Document.PrintMonoBlack, False, clWhite, Document.ThemedColorToRGB);
        H2 := Canvas.TextHeight('A');
        SetTextAlign(Canvas.handle, TA_Center + TA_Top);
        ThtCanvas(Canvas).htTextRect(Rect(X1, Y1, X1 + Width, Y1 + Height), X1 + (Width div 2), Y1 + (Height - H2) div 2, Value);
      end;
    end;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TButtonFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

procedure TButtonFormControlObj.ButtonClick(Sender: TObject);
var
  S: ThtString;
begin
  FormControlClick(Self);
  if Which = ResetB then
    MyForm.ResetControls
  else if Which = Submit then
    if Name = '' then
      MyForm.SubmitTheForm('')
    else
    begin
      S := Name;
      MyForm.SubmitTheForm(S + '=' + Value);
    end
  else if Which = Browse then
    if Assigned(Document.FileBrowse) and (MyEdit is TEditFormControlObj) then
    begin
      S := MyEdit.Text;
      Document.FileBrowse(Document.TheOwner, MyEdit, S);
      MyEdit.Text := S;
    end;
end;

procedure TButtonFormControlObj.SetHeightWidth(Canvas: TCanvas);
begin
  with FControl do
  begin
    Canvas.Font := Font;
    if FHeight >= Canvas.TextHeight('A') then
      Height := FHeight
    else
      Height := Canvas.TextHeight('A') + 8;
    if (FWidth >= 10) and not PercentWidth then {percent width set later}
      Width := FWidth
    else
      Width := Canvas.TextWidth(Caption) + 20;
  end;
end;

{----------------TFormCheckBox.WMGetDlgCode}

procedure TFormCheckBox.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WantArrows; {this to eat the arrow keys}
end;

{----------------TCheckBoxFormControlObj.Create}

constructor TCheckBoxFormControlObj.Create(Parent: TCellBasic; Position: Integer; L: TAttributeList; Prop: TProperties);
var
  T: TAttribute;
  PntPanel: TWinControl; //TPaintPanel;
begin
  inherited Create(Parent, Position, L, Prop);
  if Value = '' then
    Value := 'on';
  VertAlign := ABaseline;
  T := nil;
  if L.Find(CheckedSy, T) then
    IsChecked := True;
  PntPanel := Document.PPanel;
  FControl := TFormCheckBox.Create(PntPanel);
  with FControl do
  begin
    Left := -4000; {so will be invisible until placed}
    Width := 13;
    Height := 13;
    OnKeyDown := MyForm.AKeyDown;
    OnEnter := EnterEvent;
    OnExit := ExitEvent;
    OnMouseMove := HandleMouseMove;
    Enabled := not Disabled;
    Parent := PntPanel;
    Checked := IsChecked; {must precede setting OnClick}
    OnClick := FormControlClick;
    {$ifdef has_StyleElements}
    StyleElements := Document.StyleElements;
    {$endif}
  end;
end;

procedure TCheckBoxFormControlObj.ResetToValue;
begin
  FControl.Checked := IsChecked;
end;

procedure TCheckBoxFormControlObj.DrawInline1(Canvas: TCanvas; X1, Y1: Integer);
var
  x, y: Integer;
begin
  inherited DrawInline1(Canvas,X1,Y1);
  if IsCopy then
    with FControl do
    begin
      DrawFormControlRect(Canvas, X1, Y1, X1 + Width, Y1 + Height, False, Document.PrintMonoBlack, Disabled, clWhite, Document.ThemedColorToRGB);
      if Checked then
        with Canvas do
        begin
          Pen.Color := clBlack;
          x := X1 + 3; y := Y1 + Height div 2;
          MoveTo(x, y);
          LineTo(x + 2, y + 2);
          LineTo(x + 6, y - 2);
        end;
    end
  else
  begin
    if Active and Document.TheOwner.ShowFocusRect then //MK20091107
    begin
      Canvas.Brush.Color := clWhite;
      Canvas.DrawFocusRect(Rect(Left - 3, Top - 3, Left + 16, Top + 16));
    end;
  end;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TCheckBoxFormControlObj.GetChecked: Boolean;
begin
  Result := FControl.Checked;
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
function TCheckBoxFormControlObj.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TCheckBoxFormControlObj.GetSubmission(Index: Integer; out S: ThtString): Boolean;
begin
  Result := (Index = 0) and FControl.Checked;
  if Result then
    S := Name + '=' + Value;
end;

procedure TCheckBoxFormControlObj.SetDataInit;
begin
  FControl.Checked := False; {not checked unless later data says so}
end;

//-- BG ---------------------------------------------------------- 16.01.2011 --
procedure TCheckBoxFormControlObj.SetChecked(Value: Boolean);
begin
  FControl.Checked := Value;
end;

procedure TCheckBoxFormControlObj.SetData(Index: Integer; const V: ThtString);
begin
  if htCompareText(V, Value) = 0 then
    FControl.Checked := True;
end;

procedure TCheckBoxFormControlObj.SaveContents;
{Save the current value to see if it has changed when focus is lost}
begin
  WasChecked := FControl.Checked;
end;

//-- BG ---------------------------------------------------------- 30.08.2013 --
constructor TCheckBoxFormControlObj.CreateCopy(Parent: TCellBasic; Source: THtmlNode);
var
  T: TCheckBoxFormControlObj absolute Source;
begin
  inherited CreateCopy(Parent,Source);
  FControl := T.FControl;
  WasChecked := T.WasChecked;
  IsChecked := T.IsChecked;
end;

//-- BG ---------------------------------------------------------- 15.01.2011 --
destructor TCheckBoxFormControlObj.Destroy;
begin
  if not IsCopy then
  begin
    FControl.Parent := nil;
    FControl.Free;
  end;
  inherited Destroy;
end;

procedure TCheckBoxFormControlObj.DoOnChange;
begin
  if FControl.Checked <> WasChecked then
    if Assigned(Document.ObjectChange) then
      Document.ObjectChange(Document.TheOwner, Self, OnChangeMessage);
end;

end.
