{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2016 by Bernd Gabriel

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
{This is a modification of code found in Borland's Printers.Pas}
{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{                                                       }
{       Copyright (c) 1995,96 Borland International     }
{                                                       }
{*******************************************************}

{$I htmlcons.inc}

unit vwPrint;

interface

uses
  {$ifdef FPC}
    RtlConsts,
    {$IFDEF MSWindows}
      WinUtilPrn,
    {$else}
      LclType,
      HtmlMisc,
    {$endif}
    LCLVersion,
  {$else}
    Consts,
  {$endif}
  {$ifdef MSWINDOWS}
    Windows,
  {$endif}
  SysUtils, Forms, Contnrs,
  Classes, Graphics, Printers,
  HtmlGlobals;

type

  // BG, 30.01.2012: base class for TvwPrinter and TMetaFilePrinter
  // Allows merging the lengthy duplicate methods THtmlViewer.Print()
  // and THtmlViewer.PrintPreview() at last.
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
  protected
    function GetCanvas: TCanvas; virtual; abstract;
    function GetPageNum: Integer; virtual; abstract;
    procedure CheckPrinting(Value: Boolean);
    procedure GetPrinterCapsOf(Printer: TPrinter);
    procedure SetPrinting(Value: Boolean);
  public
    procedure BeginDoc; virtual; abstract;
    procedure NewPage; virtual; abstract;
    procedure EndDoc; virtual; abstract;
    procedure Abort; virtual; abstract;
    procedure Assign(Source: TPersistent); override;
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

  TvwPrinterState = (psNoHandle, psHandleIC, psHandleDC);

  TvwPrinter = class(ThtPrinter)
  private
    FCanvas: TCanvas;
    FPageNumber: Integer;
    FAborted: Boolean;
    State: TvwPrinterState;
    DC: HDC;
    DevMode: PDeviceMode;
    DeviceMode: HGLOBAL;
    procedure SetState(Value: TvwPrinterState);
    function GetHandle: HDC;
  protected
    function GetCanvas: TCanvas; override;
    function GetPageNum: Integer; override;
  public
    constructor Create; reintroduce; overload;
    destructor Destroy; override;
    procedure Abort; override;
    procedure BeginDoc; override;
    procedure EndDoc; override;
    procedure NewPage; override;
    property Aborted: Boolean read FAborted;
    property Handle: HDC read GetHandle;
  end;

implementation

type
  // BG, 29.01.2012: allow multiple prints of several THtmlViewer components at a time.
  TMapItem = class(TObject)
  private
    FKey: HDC;
    FValue: Pointer;
  public
    constructor Create(Key: HDC; Value: Pointer);
  end;

  // BG, 29.01.2012: allow multiple prints of several THtmlViewer components at a time.
  TMap = class(TObject)
  private
    FItems: TObjectList;
    function GetItem(Index: HDC): TMapItem;
    property Items[Index: HDC]: TMapItem read GetItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(Key: HDC): Pointer;
    function Put(Key: HDC; Value: Pointer): Pointer;
    function Remove(Key: HDC): Pointer;
  end;

var
  FPrinters: TMap;

procedure RaiseError(const Msg: string);
begin
  raise EPrinter.Create(Msg);
end;

function AbortProc(Prn: HDC; Error: Integer): Bool; stdcall;
var
  Printer: TvwPrinter;
begin
  Application.ProcessMessages;
  if FPrinters <> nil then
  begin
    Printer := FPrinters.Get(Prn);
    Result := (Printer <> nil) and not Printer.Aborted;
  end
  else
    Result := False;
end;

{ TMapItem }

//-- BG ---------------------------------------------------------- 29.01.2012 --
constructor TMapItem.Create(Key: HDC; Value: Pointer);
begin
  inherited Create;
  FKey := Key;
  FValue := Value;
end;

{ TMap }

//-- BG ---------------------------------------------------------- 29.01.2012 --
constructor TMap.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
destructor TMap.Destroy;
begin
  FItems.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
function TMap.Get(Key: HDC): Pointer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if Items[I].FKey = Key then
    begin
      Result := Items[I].FValue;
      exit;
    end;
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
function TMap.GetItem(Index: HDC): TMapItem;
begin
  Result := TMapItem(FItems[Index]);
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
function TMap.Put(Key: HDC; Value: Pointer): Pointer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if Items[I].FKey = Key then
    begin
      Result := Items[I].FValue;
      Items[I].FValue := Value;
      exit;
    end;

  Result := nil;
  FItems.add(TMapItem.Create(Key, Value));
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
function TMap.Remove(Key: HDC): Pointer;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if Items[I].FKey = Key then
    begin
      Result := Items[I].FValue;
      FItems.Delete(I);
      exit;
    end;
  Result := nil;
end;

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
      RaiseError(SNotPrinting)
    else
      RaiseError(SPrinting);
end;

//-- BG ---------------------------------------------------------- 29.01.2012 --
procedure ThtPrinter.GetPrinterCapsOf(Printer: TPrinter);
begin
  if Printer.Printers.Count = 0 then
    raise Exception.Create('Printer not available');

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

procedure ThtPrinter.SetPrinting(Value: Boolean);
begin
  FPrinting := Value;
end;

{ TPrinterCanvas }

type
  TPrinterCanvas = class(TCanvas)
    vwPrinter: TvwPrinter;
    constructor Create(APrinter: TvwPrinter);
    procedure CreateHandle; override;
    procedure Changing; override;
  end;

constructor TPrinterCanvas.Create(APrinter: TvwPrinter);
begin
  inherited Create;
  vwPrinter := APrinter;
end;

procedure TPrinterCanvas.CreateHandle;
begin
  vwPrinter.SetState(psHandleIC);
  Handle := vwPrinter.DC;
end;

procedure TPrinterCanvas.Changing;
begin
  vwPrinter.CheckPrinting(True);
  inherited Changing;
end;

{ TvwPrinter }

constructor TvwPrinter.Create;
begin
  inherited Create(nil);
end;

destructor TvwPrinter.Destroy;
begin
  if Printing then
    EndDoc;
  SetState(psNoHandle);
  FCanvas.Free;
  inherited Destroy;
end;

{$ifdef LCL}
{$else}
function CopyData(Handle: HGLOBAL): HGLOBAL;
var
  Src, Dest: PByte;
  Size: Integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
    try
      Src := GlobalLock(Handle);
      Dest := GlobalLock(Result);
      if (Src <> nil) and (Dest <> nil) then
        Move(Src^, Dest^, Size);
    finally
      GlobalUnlock(Handle);
      GlobalUnlock(Result);
    end
  end
  else
    Result := 0;
end;
{$endif LCL}

procedure TvwPrinter.SetState(Value: TvwPrinterState);
{$ifdef MsWindows}
type
  TCreateHandleFunc = function(DriverName, DeviceName, Output: PChar; InitData: PDeviceMode): HDC stdcall;
var
  CreateHandleFunc: TCreateHandleFunc;
  Driver, Device, Port: array[0..100] of char;
{$ifdef LCL}
  PrnDev: TPrinterDevice;
{$else}
  TmpDeviceMode: HGLOBAL;
{$endif}
begin
  if Value <> State then
  begin
    CreateHandleFunc := nil;
    case Value of
      psNoHandle:
        begin
          CheckPrinting(False);
          if Assigned(FCanvas) then
            FCanvas.Handle := 0;
          DeleteDC(DC);
          DC := 0;
        end;
      psHandleIC:
        if State <> psHandleDC then
          CreateHandleFunc := CreateIC
        else
          Exit;
      psHandleDC:
        begin
          if FCanvas <> nil then
            FCanvas.Handle := 0;
          if DC <> 0 then
            DeleteDC(DC);
          CreateHandleFunc := CreateDC;
        end;
    end;
    if Assigned(CreateHandleFunc) then
    begin
{$ifdef LCL}
      PrnDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
{$if lcl_fullversion >= 1020000}
      DevMode := PrnDev.DevModeA;
{$else}
      DevMode := PrnDev.DevMode;
{$ifend}
      StrCopy(Device, PChar(PrnDev.Device));
      StrCopy(Driver, PChar(PrnDev.Driver));
      StrCopy(Port, PChar(PrnDev.Port));
{$else}
      Printers.Printer.GetPrinter(Device, Driver, Port, TmpDeviceMode);
      if DeviceMode <> 0 then
      begin
        GlobalUnlock(DeviceMode);
        GlobalFree(DeviceMode);
      end;
      DevMode := nil;
      if TmpDeviceMode <> 0 then
      begin
        DeviceMode := CopyData(TmpDeviceMode);
        if DeviceMode <> 0 then
          DevMode := GlobalLock(DeviceMode);
      end;
{$endif}

      DC := CreateHandleFunc(Driver, Device, Port, DevMode);
      if DC = 0 then
        RaiseError(SInvalidPrinter);
      if FCanvas <> nil then
        FCanvas.Handle := DC;

    end;
    State := Value;
  end;
{$else MsWindows}
begin
  raise Exception.Create('TvwPrinter.SetState() not yet implemented.');
{$endif MsWindows}
end;

procedure TvwPrinter.Abort;
begin
  CheckPrinting(True);
{$ifdef MsWindows}
  AbortDoc(Canvas.Handle);
{$endif}
  FAborted := True;
  EndDoc;
end;

procedure TvwPrinter.BeginDoc;
var
  CTitle: array[0..31] of Char;
{$ifdef MsWindows}
  DocInfo: TDocInfo;
{$endif}
begin
  CheckPrinting(False);
  SetState(psHandleDC);
  GetPrinterCapsOf(Printer);
  Canvas.Refresh;
  FAborted := False;
  FPageNumber := 1;

{$ifdef MsWindows}
  StrPLCopy(CTitle, Title, Length(CTitle) - 1);
  FillChar(DocInfo, SizeOf(DocInfo), 0);
  with DocInfo do
  begin
    cbSize := SizeOf(DocInfo);
    lpszDocName := CTitle;
    lpszOutput := nil;
  end;
{$endif}
  FPrinters.Put(DC, Self);
{$ifdef MsWindows}
  SetAbortProc(DC, AbortProc);
  StartDoc(DC, DocInfo);
{$endif}
  SetPrinting(True);
{$ifdef MsWindows}
  StartPage(DC);
{$endif}
end;

procedure TvwPrinter.EndDoc;
begin
  CheckPrinting(True);
{$ifdef MsWindows}
  EndPage(DC);
{$endif}
  FPrinters.Remove(DC);
{$ifdef MsWindows}
  if not Aborted then
    Windows.EndDoc(DC);
{$endif}
  SetPrinting(False);
  FAborted := False;
  FPageNumber := 0;
  if DeviceMode <> 0 then
  begin
    GlobalUnlock(DeviceMode);
    GlobalFree(DeviceMode);
  end;
end;

procedure TvwPrinter.NewPage;
begin
  CheckPrinting(True);
{$ifdef MsWindows}
  EndPage(DC);
  StartPage(DC);
{$endif}
  Inc(FPageNumber);
  Canvas.Refresh;
end;

function TvwPrinter.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    FCanvas := TPrinterCanvas.Create(Self);
  Result := FCanvas;
end;

function TvwPrinter.GetHandle: HDC;
begin
  SetState(psHandleIC);
  Result := DC;
end;

function TvwPrinter.GetPageNum: Integer;
begin
  Result := FPageNumber;
end;

initialization
  FPrinters := TMap.Create;
finalization
  FreeAndNil(FPrinters);
end.
