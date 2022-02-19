{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2022 by Bernd Gabriel

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
  LclType,
  HtmlMisc,
  {$IFDEF MSWindows}
    WinUtilPrn,
  {$else}
  {$endif}
  LCLVersion,
{$else}
  Consts,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  SysUtils, Forms, Contnrs,
  Classes, Graphics, Printers,
  HtmlGlobals,
  HtmlPrinter;

type
  TvwPrinterState = (psNoHandle, psHandleIC, psHandleDC);

  TvwPrinter = class(ThtPrinter)
  private
    FCanvas: TCanvas;
    FPageNumber: Integer;
    State: TvwPrinterState;
    DC: HDC;
{$ifdef MSWINDOWS}
    DevMode: {$if lcl_fullversion >= 1080000} PDeviceModeW {$else} PDeviceMode {$ifend};
{$endif}
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
{$ifdef UseGenerics}
    FItems: TObjectList<TMapItem>;
{$else}
    FItems: TObjectList;
{$endif}
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
  Printer: ThtPrinter;
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
{$ifdef UseGenerics}
  FItems := TObjectList<TMapItem>.Create;
{$else}
  FItems := TObjectList.Create;
{$endif}
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
  function CreateDefaultPrinterHandle(CreateHandleFunc: TvwPrinterState): HDC;
{$ifdef LCL}
  var
    PrnDev: TPrinterDevice;
    Driver, Device, Port: {$if lcl_fullversion >= 1080000} WideString {$else} string {$ifend};
  begin
    PrnDev := TPrinterDevice(Printer.Printers.Objects[Printer.PrinterIndex]);
    Device := PrnDev.Device;
    Driver := PrnDev.Driver;
    Port := PrnDev.Port;
    {$if lcl_fullversion >= 1080000}
      DevMode := PrnDev.DevModeW;
      if CreateHandleFunc = psHandleDC then
        Result := CreateDCW(PWideChar(Driver), PWideChar(Device), PWideChar(Port), DevMode)
      else
        Result := CreateICW(PWideChar(Driver), PWideChar(Device), PWideChar(Port), DevMode);
    {$else}
      DevMode := PrnDev.{$if lcl_fullversion >= 1020000} DevModeA {$else} DevMode {$ifend};
      if CreateHandleFunc = psHandleDC then
        Result := CreateDC(PChar(Driver), PChar(Device), PChar(Port), DevMode)
      else
        Result := CreateIC(PChar(Driver), PChar(Device), PChar(Port), DevMode);
    {$ifend}
{$else}
  var
    TmpDeviceMode: HGLOBAL;
    Driver, Device, Port: array[0..200] of {$ifdef UNICODE} WideChar {$else} char {$endif};
  begin
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
    if CreateHandleFunc = psHandleDC then
      Result := CreateDC(Driver, Device, Port, DevMode)
    else
      Result := CreateIC(Driver, Device, Port, DevMode);
{$endif}
  end;

var
  CreateHandleFunc: TvwPrinterState;
begin
  if Value <> State then
  begin
    CreateHandleFunc := psNoHandle;
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
          CreateHandleFunc := psHandleIC
        else
          // don't change the state from psHandleDC
          Exit;

      psHandleDC:
        begin
          if FCanvas <> nil then
            FCanvas.Handle := 0;
          if DC <> 0 then
            DeleteDC(DC);
          CreateHandleFunc := psHandleDC;
        end;
    end;
    if CreateHandleFunc <> psNoHandle then
    begin
      DC := CreateDefaultPrinterHandle(CreateHandleFunc);
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
  SetAborted(True);
  EndDoc;
end;

procedure TvwPrinter.BeginDoc;
{$ifdef MsWindows}
var
  DocInfo: {$if defined(LCL) and (lcl_fullversion < 1060000)} TDocInfo {$else} TDocInfoW {$ifend};
{$endif}
begin
  CheckPrinting(False);
  SetState(psHandleDC);
  GetPrinterCapsOf(Printer);
  Canvas.Refresh;
  SetAborted(False);
  FPageNumber := 1;

{$ifdef MsWindows}
  FillChar(DocInfo, SizeOf(DocInfo), 0);
  with DocInfo do
  begin
    cbSize := SizeOf(DocInfo);
    lpszDocName := {$if defined(LCL) and (lcl_fullversion < 1060000)} PChar(Title) {$else} PWideChar(Title) {$ifend};;
  end;
  SetAbortProc(DC, AbortProc);
{$endif}
  FPrinters.Put(DC, Self);
{$ifdef MsWindows}
  StartDocW(DC, {$ifdef LCL}@{$endif}DocInfo);
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
  SetAborted(False);
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
