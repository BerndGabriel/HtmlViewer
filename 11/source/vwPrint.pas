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
  Windows, Classes, Graphics, Printers;

type
  TvwPrinterState = (psNoHandle, psHandleIC, psHandleDC);

  TvwPrinter = class(TObject)
  private
    FCanvas: TCanvas;
    FPageNumber: Integer;
    FTitle: string;
    FPrinting: Boolean;
    FAborted: Boolean;
    State: TvwPrinterState;
    DC: HDC;
    DevMode: PDeviceMode;
    DeviceMode: THandle;
    procedure SetState(Value: TvwPrinterState);
    function GetCanvas: TCanvas;
    function GetHandle: HDC;
    function GetPageHeight: Integer;
    function GetPageWidth: Integer;
    procedure CheckPrinting(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    property Aborted: Boolean read FAborted;
    property Canvas: TCanvas read GetCanvas;
    property Handle: HDC read GetHandle;
    property PageHeight: Integer read GetPageHeight;
    property PageWidth: Integer read GetPageWidth;
    property PageNumber: Integer read FPageNumber;
    property Printing: Boolean read FPrinting;
    property Title: string read FTitle write FTitle;
  end;

{ vwPrinter function - Replaces the Printer global variable of previous versions,
  to improve smart linking (reduce exe size by 2.5k in projects that don't use
  the printer).  Code which assigned to the Printer global variable
  must call SetPrinter instead.  SetPrinter returns current printer object
  and makes the new printer object the current printer.  It is the caller's
  responsibility to free the old printer, if appropriate.  (This allows
  toggling between different printer objects without destroying configuration
  settings.) }

function vwPrinter: TvwPrinter;
function vwSetPrinter(NewPrinter: TvwPrinter): TvwPrinter;

implementation

uses
{$ifdef FPC}
  RtlConsts,
{$else}
  Consts,
{$endif}
  SysUtils, Forms;

var
  FPrinter: TvwPrinter;

procedure RaiseError(const Msg: string);
begin
  raise EPrinter.Create(Msg);
end;

function AbortProc(Prn: HDC; Error: Integer): Bool; stdcall;
begin
  Application.ProcessMessages;
  Result := not FPrinter.Aborted;
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
  inherited Create;
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
  TCreateHandleFunc = function(DriverName, DeviceName, Output: PChar;
    InitData: PDeviceMode): HDC stdcall;
var
  CreateHandleFunc: TCreateHandleFunc;
  Driver, Device, Port: array[0..100] of char;
  TmpDeviceMode: THandle;
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

procedure TvwPrinter.CheckPrinting(Value: Boolean);
begin
  if Printing <> Value then
    if Value then
      RaiseError(SNotPrinting)
    else
      RaiseError(SPrinting);
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
  Canvas.Refresh;
  FPrinting := True;
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
  SetAbortProc(DC, AbortProc);
  StartDoc(DC, DocInfo);
  StartPage(DC);
end;

procedure TvwPrinter.EndDoc;
begin
  CheckPrinting(True);
  EndPage(DC);
  if not Aborted then
    Windows.EndDoc(DC);
  FPrinting := False;
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


function TvwPrinter.GetPageHeight: Integer;
begin
  SetState(psHandleIC);
  Result := GetDeviceCaps(DC, VertRes);
end;

function TvwPrinter.GetPageWidth: Integer;
begin
  SetState(psHandleIC);
  Result := GetDeviceCaps(DC, HorzRes);
end;

function vwPrinter: TvwPrinter;
begin
  if FPrinter = nil then
    FPrinter := TvwPrinter.Create;
  Result := FPrinter;
end;

function vwSetPrinter(NewPrinter: TvwPrinter): TvwPrinter;
begin
  Result := FPrinter;
  FPrinter := NewPrinter;
end;

initialization
  FPrinter := nil;
finalization
  FPrinter.Free;
end.
