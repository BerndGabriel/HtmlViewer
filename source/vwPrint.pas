{
Version   11.5
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

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
  Windows, Classes, Graphics, Printers,
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
    FPPIX: Integer;         // Logical pixelsinch in X
    FPPIY: Integer;         // Logical pixelsinch in Y
    FPrinting: Boolean;
    FTitle: ThtString;      // Printed Document's Title
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
    DeviceMode: THandle;
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

{ vwPrinter function - Replaces the Printer global variable of previous versions,
  to improve smart linking (reduce exe size by 2.5k in projects that don't use
  the printer).  Code which assigned to the Printer global variable
  must call SetPrinter instead.  SetPrinter returns current printer object
  and makes the new printer object the current printer.  It is the caller's
  responsibility to free the old printer, if appropriate.  (This allows
  toggling between different printer objects without destroying configuration
  settings.) }

implementation

uses
{$ifdef FPC}
  RtlConsts,
{$else}
  Consts,
{$endif}
  SysUtils, Forms;

type
  // BG, 29.01.2012: allow multiple prints of several THtmlViewer components at a time.
  TMap = class(TObject)
  private
    FKeys: TList;
    FValues: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(Key: Pointer): Pointer;
    function Put(Key, Value: Pointer): Pointer;
    function Remove(Key: Pointer): Pointer;
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
    Printer := FPrinters.Get(Ptr(Prn));
    Result := (Printer <> nil) and not Printer.Aborted;
  end
  else
    Result := False;
end;

{ TMap }

constructor TMap.Create;
begin
  inherited Create;
  FKeys := TList.Create;
  FValues := TList.Create;
end;

destructor TMap.Destroy;
begin
  FKeys.Free;
  FValues.Free;
  inherited;
end;

function TMap.Get(Key: Pointer): Pointer;
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I >= 0 then
    Result := FValues[I]
  else
    Result := nil;
end;

function TMap.Put(Key, Value: Pointer): Pointer;
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I >= 0 then
  begin
    Result := FValues[I];
    FValues[I] := Value;
  end
  else
  begin
    Result := nil;
    FKeys.add(Key);
    FValues.add(Value);
  end;
end;

function TMap.Remove(Key: Pointer): Pointer;
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I >= 0 then
  begin
    Result := FValues[I];
    FKeys.Delete(I);
    FValues.Delete(I);
  end
  else
    Result := nil;
end;

{ ThtPrinter }

procedure ThtPrinter.CheckPrinting(Value: Boolean);
begin
  if Printing <> Value then
    if Value then
      RaiseError(SNotPrinting)
    else
      RaiseError(SPrinting);
end;

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

function CopyData(Handle: THandle): THandle;
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

procedure TvwPrinter.SetState(Value: TvwPrinterState);
type
  TCreateHandleFunc = function(DriverName, DeviceName, Output: PChar; InitData: PDeviceMode): HDC stdcall;
var
  CreateHandleFunc: TCreateHandleFunc;
{$ifndef FPC_TODO_PRINTING}
  Driver, Device, Port: array[0..100] of char;
  TmpDeviceMode: THandle;
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
{$ifndef FPC_TODO_PRINTING}
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

      DC := CreateHandleFunc(Driver, Device, Port, DevMode);
      if DC = 0 then
        RaiseError(SInvalidPrinter);
      if FCanvas <> nil then
        FCanvas.Handle := DC;

{$endif}
    end;
    State := Value;
  end;
end;

procedure TvwPrinter.Abort;
begin
  CheckPrinting(True);
  AbortDoc(Canvas.Handle);
  FAborted := True;
  EndDoc;
end;

procedure TvwPrinter.BeginDoc;
var
  CTitle: array[0..31] of Char;
  DocInfo: TDocInfo;
begin
  CheckPrinting(False);
  SetState(psHandleDC);
  getPrinterCapsOf(Printer);
  Canvas.Refresh;
  FAborted := False;
  FPageNumber := 1;

  StrPLCopy(CTitle, Title, Length(CTitle) - 1);
  FillChar(DocInfo, SizeOf(DocInfo), 0);
  with DocInfo do
  begin
    cbSize := SizeOf(DocInfo);
    lpszDocName := CTitle;
    lpszOutput := nil;
  end;
  FPrinters.Put(Ptr(DC), Self);
  SetAbortProc(DC, AbortProc);
  StartDoc(DC, DocInfo);
  SetPrinting(True);
  StartPage(DC);
end;

procedure TvwPrinter.EndDoc;
begin
  CheckPrinting(True);
  EndPage(DC);
  FPrinters.Remove(Ptr(DC));
  if not Aborted then
    Windows.EndDoc(DC);
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
  EndPage(DC);
  StartPage(DC);
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

