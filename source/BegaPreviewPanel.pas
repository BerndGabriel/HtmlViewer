{-------------------------------------------------------------------------------
Copyright (C) 2006-2012 by Bernd Gabriel.

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
-------------------------------------------------------------------------------}

{$include htmlcons.inc}

unit BegaPreviewPanel;

interface
{$ifndef NoMetaFile}

uses
{$ifdef MSWindows}
  Windows,
{$endif}
  Classes,
{$ifdef LCL}
  LclIntf, LclType, HtmlMisc,
{$endif LCL}
  Controls, ExtCtrls, Graphics, Math,
  // own units
  BegaMetaFilePrinter;

type
  TBegaCustomPreviewPanel = class;

  TBegaUpdatePreviewEvent = procedure(Sender: TBegaCustomPreviewPanel; var Width, Height: Integer) of object;

  //BG, 05.11.2006
  TBegaCustomPreviewPanel = class(TCustomPanel)
  private
    FColumnCount: Integer;
    FColumnWidth: Integer;
    FInnerWidth: Integer;
    FRowCount: Integer;
    FRowHeight: Integer;
    FOnUpdatePreview: TBegaUpdatePreviewEvent;
    function getColumnWidth(Column: Integer): Integer;
    function getRowCount: Integer;
    function getRowHeight(Row: Integer): Integer;
    function setColumnWidthIfNew(const Value: Integer): Boolean;
    function setRowHeightIfNew(const Value: Integer): Boolean;
    procedure doUpdatePreview(var Width, Height: Integer);
    procedure setColumnCount(const Value: Integer);
    procedure setColumnWidth(const Value: Integer);
    procedure setInnerWidth(const Value: Integer);
    procedure setRowCount(const Value: Integer);
    procedure setRowHeight(const Value: Integer);
  protected
    function getCell(Column, Row: Integer): TControl; virtual; abstract;
    function getCellCount: Integer; virtual; abstract;
    function indexOfCell(Column, Row: Integer): Integer;
    property CommonColumnWidth: Integer read FColumnWidth write setColumnWidth;
    property CommonRowHeight: Integer read FRowHeight write setRowHeight;
    property CellCount: Integer read getCellCount;
    property Cells[Column: Integer; Row: Integer]: TControl read getCell; default;
    property ColumnCount: Integer read FColumnCount write setColumnCount default 1;
    property ColumnWidth[Column: Integer]: Integer read getColumnWidth;
    property InnerWidth: Integer read FInnerWidth write setInnerWidth default 4;
    property OnUpdatePreview: TBegaUpdatePreviewEvent read FOnUpdatePreview write FOnUpdatePreview;
    property RowCount: Integer read getRowCount write setRowCount default 1;
    property RowHeight[Row: Integer]: Integer read getRowHeight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure updateColumnWidth;
    procedure updateRowHeight;
    procedure updatePreview; overload; virtual;
    procedure updatePreview(NewCommonColumnWidth, NewCommonRowHeight: Integer); overload;
  end;

  //BG, 05.11.2006
  TBegaCellPreviewPanel = class(TBegaCustomPreviewPanel)
  private
    FCells: TList;
    function getItem(Index: Integer): TControl;
  protected
    function createCell(Column, Row: Integer): TControl; virtual; abstract;
    function getCell(Column, Row: Integer): TControl; override;
    function getCellCount: Integer; override;
    property Items[Index: Integer]: TControl read getItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  //BG, 05.11.2006

  { TBegaMetafilePreviewCell }

  TBegaMetafilePreviewCell = class(TGraphicControl)
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  //BG, 05.11.2006
  TBegaMetaFilePageEvent = procedure(Sender: TBegaCellPreviewPanel; Canvas: TCanvas; PageIndex: Integer) of object;

  //BG, 05.11.2006
  TBegaMetafilePreviewPanel = class(TBegaCellPreviewPanel)
  private
    FMFPrinter: TBegaMetaFilePrinter;
    FFirstPage: Integer;
    FOnPaintPage: TBegaMetaFilePageEvent;
    procedure setMFPrinter(const Value: TBegaMetaFilePrinter);
    procedure doPaintPage(Cell: TBegaMetafilePreviewCell);
    procedure setFirstPage(const Value: Integer);
  protected
    function createCell(Column, Row: Integer): TControl; override;
    function getCellCount: Integer; override;
    procedure Paint; override;
  public
    procedure updatePreview; override;
    property FirstPage: Integer read FFirstPage write setFirstPage;
    property MFPrinter: TBegaMetaFilePrinter read FMFPrinter write setMFPrinter;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property ColumnCount;
    property CommonColumnWidth;
    property CommonRowHeight;
    property Constraints;
{$ifndef LCL}
    property Ctl3D;
{$endif LCL}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property InnerWidth;
{$ifndef LCL}
    property Locked;
{$endif LCL}
    property ParentBiDiMode;
    property ParentColor;
{$ifndef LCL}
    property ParentCtl3D;
{$endif LCL}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
{$ifndef LCL}
    property OnCanResize;
{$endif LCL}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaintPage: TBegaMetaFilePageEvent read FOnPaintPage write FOnPaintPage;
    property OnResize;
    property OnStartDrag;
    property OnUpdatePreview;
  end;

{$endif NoMetaFile}
implementation
{$ifndef NoMetaFile}

uses
{$ifdef UseVCLStyles}
  System.Types,
  System.UITypes,
  Vcl.Themes,
{$endif}
  MetaFilePrinter;

{ TBegaCustomPreviewPanel }

//- BG ----------------------------------------------------------- 05.11.2006 --
constructor TBegaCustomPreviewPanel.Create(AOwner: TComponent);
begin
  inherited;
  FColumnCount := 1;
  FRowCount := 1;
  FInnerWidth := 4;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
destructor TBegaCustomPreviewPanel.Destroy;
begin
  inherited;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewPanel.doUpdatePreview(var Width,
  Height: Integer);
begin
  if assigned(FOnUpdatePreview) then
    FOnUpdatePreview(self, Width, Height);
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCustomPreviewPanel.getColumnWidth(Column: Integer): Integer;
var
  Row: Integer;
  Cell: TControl;
begin
  Result := FColumnWidth;
  if Result = 0 then
    for Row := 0 to RowCount - 1 do
    begin
      Cell := Cells[Column, Row];
      if Cell <> nil then
        Result := max(Result, Cell.Width);
    end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCustomPreviewPanel.getRowCount: Integer;
begin
  Result := FRowCount;
  if Result <= 0 then
  begin
    Result := CellCount;
    if Result > 0 then
      Result := (Result - 1) div FColumnCount + 1;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCustomPreviewPanel.getRowHeight(Row: Integer): Integer;
var
  Column: Integer;
  Cell: TControl;
begin
  Result := FRowHeight;
  if Result = 0 then
    for Column := 0 to ColumnCount - 1 do
    begin
      Cell := Cells[Column, Row];
      if Cell <> nil then
        Result := max(Result, Cell.Height);
    end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCustomPreviewPanel.indexOfCell(Column,
  Row: Integer): Integer;
begin
  Result := FColumnCount * Row + Column;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewPanel.setColumnCount(const Value: Integer);
begin
  if FColumnCount <> Value then
  begin
    FColumnCount := Value;
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewPanel.setColumnWidth(const Value: Integer);
begin
  if setColumnWidthIfNew(Value) then
  begin
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
function TBegaCustomPreviewPanel.setColumnWidthIfNew(
  const Value: Integer): Boolean;
begin
  Result := FColumnWidth <> Value;
  if Result then
  begin
    FColumnWidth := Value;
    updateColumnWidth;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewPanel.setInnerWidth(const Value: Integer);
begin
  if FInnerWidth <> Value then
  begin
    FInnerWidth := Value;
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
procedure TBegaCustomPreviewPanel.setRowCount(const Value: Integer);
begin
  if FRowCount <> Value then
  begin
    FRowCount := Value;
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewPanel.setRowHeight(const Value: Integer);
begin
  if setRowHeightIfNew(Value) then
  begin
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
function TBegaCustomPreviewPanel.setRowHeightIfNew(const Value: Integer): Boolean;
begin
  Result := FRowHeight <> Value;
  if Result then
  begin
    FRowHeight := Value;
    updateRowHeight;
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewPanel.updateColumnWidth;
var
  Column, Row: Integer;
  Cell: TControl;
begin
  for Row := 0 to RowCount - 1 do
    for Column := 0 to ColumnCount - 1 do
    begin
      Cell := Cells[Column, Row];
      if Cell <> nil then
        Cell.Width := FColumnWidth;
    end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaCustomPreviewPanel.updatePreview;
var
  Column: Integer;
  Row: Integer;
  CellTop: Integer;
  Cell: TControl;
  CellLeft: array of Integer;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  if (ColumnCount > 0) and (RowCount > 0) then
  begin
    // pre-calc Left of columns
    setLength(CellLeft, ColumnCount);
    CellLeft[0] := BorderWidth;
    for Column := 0 to ColumnCount - 2 do
      CellLeft[Column + 1] := CellLeft[Column] + ColumnWidth[Column] + InnerWidth;
    // re-calc Left and Top of all cells
    CellTop := BorderWidth;
    for Row := 0 to RowCount - 1 do
    begin
      for Column := 0 to ColumnCount - 1 do
      begin
        Cell := Cells[Column, Row];
        Cell.Top := CellTop;
        Cell.Left := CellLeft[Column];
      end;
      inc(CellTop, RowHeight[Row] + InnerWidth);
    end;
    Column := ColumnCount - 1;
    NewWidth := CellLeft[Column] + ColumnWidth[Column] + BorderWidth;
    NewHeight := CellTop + BorderWidth - InnerWidth;
    doUpdatePreview(NewWidth, NewHeight);
    Width := NewWidth;
    Height := NewHeight;
  end;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewPanel.updatePreview(NewCommonColumnWidth,
  NewCommonRowHeight: Integer);
var
  IsNew: Boolean;
begin
  IsNew := False;
  if setColumnWidthIfNew(NewCommonColumnWidth) then
    IsNew := True;
  if setRowHeightIfNew(NewCommonRowHeight) then
    IsNew := True;
  if IsNew then
    updatePreview;
  invalidate;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaCustomPreviewPanel.updateRowHeight;
var
  Column, Row: Integer;
  Cell: TControl;
begin
  for Row := 0 to RowCount - 1 do
    for Column := 0 to ColumnCount - 1 do
    begin
      Cell := Cells[Column, Row];
      if Cell <> nil then
        Cell.Height := FRowHeight;
    end;
end;

{ TBegaCellPreviewPanel }

//- BG ----------------------------------------------------------- 05.11.2006 --
constructor TBegaCellPreviewPanel.Create(AOwner: TComponent);
begin
  inherited;
  FCells := TList.Create;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
destructor TBegaCellPreviewPanel.Destroy;
begin
  FCells.free;
  inherited;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCellPreviewPanel.getCell(Column, Row: Integer): TControl;
var
  Index: Integer;
begin
  Index := indexOfCell(Column, Row);
  if Index >= FCells.Count then
    FCells.Count := Index + 1;
  Result := FCells[Index];
  if Result = nil then
  begin
    Result := createCell(Column, Row);
    FCells[Index] := Result;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaCellPreviewPanel.getCellCount: Integer;
begin
  Result := FCells.Count;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
function TBegaCellPreviewPanel.getItem(Index: Integer): TControl;
begin
  Result := FCells[Index];
end;

{ TBegaMetafilePreviewCell }

//- BG ----------------------------------------------------------- 05.11.2006 --
constructor TBegaMetafilePreviewCell.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 340;
  Height := 440;
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaMetafilePreviewCell.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToParent(Point(X, Y));
  TBegaMetafilePreviewPanel(Owner).MouseDown(Button, Shift, P.X, P.Y);
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaMetafilePreviewCell.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToParent(Point(X, Y));
  TBegaMetafilePreviewPanel(Owner).MouseMove(Shift, P.X, P.Y);
end;

//- BG ----------------------------------------------------------- 06.11.2006 --
procedure TBegaMetafilePreviewCell.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := ClientToParent(Point(X, Y));
  TBegaMetafilePreviewPanel(Owner).MouseUp(Button, Shift, P.X, P.Y);
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaMetafilePreviewCell.Paint;
var
  Page: TMetaFile;
  MFPrinter: TBegaMetaFilePrinter;
  Rect: TBegaRect;
begin
  MFPrinter := TBegaMetafilePreviewPanel(Owner).MFPrinter;
  if (csDesigning in ComponentState) or (MFPrinter = nil) then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end
  else
  begin
    Rect := MFPrinter.PaperArea;
    SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
    SetWindowExtEx(Canvas.Handle, Rect.Width, Rect.Height, nil);
    SetWindowOrgEx(Canvas.Handle, -Rect.Left, -Rect.Top, nil);
    SetViewportExtEx(Canvas.Handle, Width, Height, nil);
    SetViewportOrgEx(Canvas.Handle, Left, Top, nil);
    if (Tag >= 0) and (Tag < MFPrinter.PageNumber) then
    begin
      Page := MFPrinter.MetaFiles[Tag];
      if Page <> nil then
      begin
        Canvas.Draw(0, 0, Page);
        TBegaMetafilePreviewPanel(Owner).doPaintPage(self);
      end;
    end;
  end;
end;

{ TBegaMetafilePreviewPanel }

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaMetafilePreviewPanel.createCell(Column, Row: Integer): TControl;
begin
  Result := TBegaMetafilePreviewCell.Create(self);
  Result.Parent := self;
  Result.Tag := indexOfCell(Column, Row) + FirstPage;
  if FColumnWidth > 0 then
    Result.Width := FColumnWidth;
  if FRowHeight > 0 then
    Result.Height := FRowHeight;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaMetafilePreviewPanel.doPaintPage(Cell: TBegaMetafilePreviewCell);
begin
  if assigned(FOnPaintPage) then
    FOnPaintPage(self, Cell.Canvas, Cell.Tag);
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
function TBegaMetafilePreviewPanel.getCellCount: Integer;
begin
  if FMFPrinter <> nil then
    Result := min(max(FMFPrinter.PageNumber - FirstPage, 0), RowCount * ColumnCount)
  else
    Result := inherited getCellCount;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaMetafilePreviewPanel.Paint;
begin
  inherited;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
procedure TBegaMetafilePreviewPanel.setFirstPage(const Value: Integer);
begin
  if FFirstPage <> Value then
  begin
    FFirstPage := Value;
    updatePreview;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 05.11.2006 --
procedure TBegaMetafilePreviewPanel.setMFPrinter(
  const Value: TBegaMetaFilePrinter);
begin
  if FMFPrinter <> Value then
  begin
    FMFPrinter := Value;
    invalidate;
  end;
end;

//- BG ----------------------------------------------------------- 08.11.2006 --
procedure TBegaMetafilePreviewPanel.updatePreview;
var
  Index: Integer;
begin
  for Index := 0 to min(CellCount, FCells.Count) - 1 do
    Items[Index].Tag := Index + FirstPage;
  for Index := FCells.Count - 1 downto CellCount do
  begin
    Items[Index].free;
    FCells.delete(Index);
  end;
  inherited;
end;

{$endif NoMetaFile}
end.
