{-------------------------------------------------------------------------------
Version   11.10
Copyright (C) 2022-2023 by Bernd Gabriel.

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

{$I htmlcons.inc}

unit HtmlPrinter;

interface

uses
{$ifdef FPC}
  RtlConsts,
{$else}
  Consts,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  SysUtils, Forms,
  Classes, Contnrs, Graphics, Printers,
  HtmlGlobals;

type
  // BG, 19.02.2022: moved from vwPrint.pas
  ThtPrinter = class(TComponent)
  private
    FOffsetX: Integer;      // Physical Printable Area x margin
    FOffsetY: Integer;      // Physical Printable Area y margin
    FPaperHeight: Integer;  // Physical Height in device units
    FPaperWidth: Integer;   // Physical Width in device units
    FPgHeight: Integer;     // Vertical height in pixels
    FPgWidth: Integer;      // Horizontal width in pixels
    FPPIX: Integer;         // Logical pixels per inch in X
    FPPIY: Integer;         // Logical pixels per inch in Y
    FPrinting: Boolean;
    FTitle: ThtString;         // Printed Document's Title
    FAborted: Boolean;
  protected
    function GetCanvas: TCanvas; virtual; abstract;
    function GetPageNum: Integer; virtual; abstract;
    procedure CheckPrinting(Value: Boolean);
    procedure GetPrinterCapsOf(Printer: TPrinter);
    procedure SetPrinting(Value: Boolean);
    procedure SetAborted(Value: Boolean);
  public
    procedure BeginDoc; virtual; abstract;
    procedure NewPage; virtual; abstract;
    procedure EndDoc; virtual; abstract;
    procedure Abort; virtual; abstract;
    procedure Assign(Source: TPersistent); override;
    property Aborted: Boolean read FAborted;
    property Canvas: TCanvas read GetCanvas;
    property OffsetX: Integer read FOffsetX;
    property OffsetY: Integer read FOffsetY;
    property PageNumber: Integer read GetPageNum;
    property PageHeight: Integer read FPgHeight;
    property PageWidth: Integer read FPgWidth;
    property PaperHeight: Integer read FPaperHeight;
    property PaperWidth: Integer read FPaperWidth;
    property PixelsPerInchX: Integer read FPPIX;
    property PixelsPerInchY: Integer read FPPIY;
    property Printing: Boolean read FPrinting; // becomes True in BeginDoc and back to False in EndDoc.
    property Title: ThtString read FTitle write FTitle;
  end;

  TUnits = (unInches, unCentimeters);
  TPageEvent = procedure(Sender: TObject; NumPage: Integer;
    var StopPrinting: Boolean) of object;

  // BG, 19.02.2022 extracted from TMetafilePrinter
  ThtPagePrinter = class(ThtPrinter)
  private
    FPreviewPixelsPerInch: Integer;
  protected
{$ifdef UseGenerics}
    FPages: TObjectList<TObject>;
{$else}
    FPages: TObjectList;
{$endif}
    FCurCanvas: TCanvas;
    FUnits: TUnits;
    FConvFac: Double;
    FUsedPage: Boolean;
    FOnPageEvent: TPageEvent;

    procedure FreePages;
    function GetPage(I: integer): TObject;
    procedure SetUnits(Val: TUnits);
    function GetLastAvailPage: Integer;

    function GetCanvas: TCanvas; override;
    function GetPageNum: integer; override;
    procedure CreatePage(var NextPage: TObject; var NextCanvas: TCanvas); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

      // Printer Methods
    procedure BeginDoc; override;
    procedure NewPage; override;
    procedure EndDoc; override;
    procedure Abort; override;

    property Pages[I: Integer]: TObject read GetPage;
    //property Printing: boolean read FPrinting;
    property LastAvailablePage: Integer read GetLastAvailPage;
    property PreviewPixelsPerInch: Integer read FPreviewPixelsPerInch write FPreviewPixelsPerInch;
  published
    property Units: TUnits read FUnits write SetUnits;
    property OnPageEvent: TPageEvent read FOnPageEvent write FOnPageEvent;
  end;


implementation

const
  INCH_TO_CM = 2.54;

resourcestring
  SPrnNotAvail = 'Printer not available';

{ ThtPrinter }

//-- BG ---------------------------------------------------------- 29.01.2012 --
procedure ThtPrinter.Assign(Source: TPersistent);
var
  Src: ThtPrinter absolute Source;
begin
  inherited;
  if Source is ThtPrinter then
  begin
    FPPIX := Src.FPPIX;
    FPPIY := Src.FPPIY;
    FPaperWidth  := Src.FPaperWidth;
    FPaperHeight := Src.FPaperHeight;
    FOffsetX  := Src.FOffsetX;
    FOffsetY  := Src.FOffsetY;
    FPgHeight := Src.FPgHeight;
    FPgWidth  := Src.FPgWidth;
  end;
end;

procedure ThtPrinter.CheckPrinting(Value: Boolean);
begin
  if Printing <> Value then
    if Value then
      EPrinter.Create(SNotPrinting)
    else
      EPrinter.Create(SPrinting);
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
procedure ThtPrinter.GetPrinterCapsOf(Printer: TPrinter);
begin
  if Printer.Printers.Count = 0 then
    raise EPrinter.Create(SPrnNotAvail);

{$ifdef LCL}
  FPPIX := Printer.XDPI;
  FPPIY := Printer.YDPI;
  FPaperWidth := Printer.PaperSize.PaperRect.PhysicalRect.Right;
  FPaperHeight := Printer.PaperSize.PaperRect.PhysicalRect.Bottom;
  FOffsetX := Printer.PaperSize.PaperRect.WorkRect.Left;
  FOffsetY := Printer.PaperSize.PaperRect.WorkRect.Top;
  FPgHeight := Printer.PageHeight;
  FPgWidth := Printer.PageWidth;
{$else}
  FPPIX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  FPPIY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  FPaperWidth := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
  FPaperHeight := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
  FOffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  FOffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  FPgHeight := Printer.PageHeight;
  FPgWidth := Printer.PageWidth;
{$endif}
end;

procedure ThtPrinter.SetAborted(Value: Boolean);
begin
  FAborted := Value;
end;

procedure ThtPrinter.SetPrinting(Value: Boolean);
begin
  FPrinting := Value;
end;

{ ThtPagePrinter }

constructor ThtPagePrinter.Create(AOwner: TComponent);
begin
  inherited;
{$ifdef UseGenerics}
  FPages := TObjectList<TObject>.Create;
{$else}
  FPages := TObjectList.Create;
{$endif}
  FUnits := unInches;
  FPreviewPixelsPerInch := Screen.PixelsPerInch;
end;

destructor ThtPagePrinter.Destroy;
begin
  FreePages;
  FPages.Free;
  inherited;
end;


procedure ThtPagePrinter.FreePages;
begin
  FPages.Clear;
  FreeAndNil(FCurCanvas);
end;

function ThtPagePrinter.GetPage(I: integer): TObject;
begin
  Result := FPages[I];
end;

procedure ThtPagePrinter.SetUnits(Val: TUnits);
begin
  FUnits := Val;
  case FUnits of
    unInches: FConvFac := 1;
    unCentimeters: FConvFac := INCH_TO_CM;
  end;
end;

procedure ThtPagePrinter.BeginDoc;
begin
  SetPrinting(True);
  FreePages;

  GetPrinterCapsOf(Printer);

  NewPage;
end;

procedure ThtPagePrinter.EndDoc;
begin
  SetPrinting(False);
  FreeAndNil(FCurCanvas);

   // in case NewPage was called but nothing drawn on it
  if not FUsedPage then
    FPages.Delete(FPages.Count - 1);
end;

procedure ThtPagePrinter.Abort;
begin
  SetPrinting(False);
  FreeAndNil(FCurCanvas);
  FreePages;
end;

procedure ThtPagePrinter.NewPage;
var
  NextPage: TObject;
  NextCanvas: TCanvas;
  Done: Boolean;
begin
  NextPage := nil;
  NextCanvas := FCurCanvas;
  CreatePage(NextPage, NextCanvas);

  { fill the page with "whiteness" }

  NextCanvas.Brush.Color := clWhite;
  NextCanvas.Pen.Color := clWhite;
  NextCanvas.Brush.Style := bsSolid;
  NextCanvas.Rectangle(0, 0, PaperWidth, PaperHeight);

  NextCanvas.Brush.Style := bsClear;
  if FCurCanvas = nil then
  begin
    NextCanvas.Font.PixelsPerInch := PreviewPixelsPerInch;
    NextCanvas.Font.Name := FontSans;
    NextCanvas.Font.Size := 10;
    FCurCanvas := NextCanvas;
  end
  else if NextCanvas <> FCurCanvas then
  begin
    NextCanvas.Font.Assign(FCurCanvas.Font);
    FCurCanvas.Free;
    FCurCanvas := NextCanvas;
  end;

  FPages.Add(NextPage);
  FUsedPage := False;

  if Assigned(FOnPageEvent) then
  begin
    Done := False;
    FOnPageEvent(Self, FPages.Count, Done);
  end;
end;

function ThtPagePrinter.GetPageNum: Integer;
begin
  Result := FPages.Count;
end;

function ThtPagePrinter.GetLastAvailPage: Integer;
begin
  Result := GetPageNum;
end;

function ThtPagePrinter.GetCanvas: TCanvas;
begin
  Result := FCurCanvas;
  FUsedPage := True;
end;

end.
