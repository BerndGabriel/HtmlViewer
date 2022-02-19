{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2016 by HtmlViewer Team
Copyright (c) 2016-2022 by Bernd Gabriel

***************************************************************
*                                                             *
*              Thanks to Chris Wallace for the                *
*         ideas and most of the code for Print Preview        *
*                                                             *
***************************************************************

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

unit MetaFilePrinter;

{$I htmlcons.inc}

interface
{$ifndef NoMetaFile}

uses
  {$ifdef MSWINDOWS}
    Windows,
  {$else}
    Types,
    {$ifdef LCL}
      LclType,
    {$endif}
  {$endif}
  Classes, Graphics, Printers, SysUtils, Forms,
  HtmlGlobals,
  HtmlPrinter;

type

{$ifdef MetaFileMissing}

{$ifdef MsWindows}
{$else}
  HENHMETAFILE = longint;
{$endif}

  TMetafile = class(ThtGraphic)
  private
    FImageHandle: HENHMETAFILE;
    FImageMMWidth: Integer;      // are in 0.01 mm logical pixels
    FImageMMHeight: Integer;     // are in 0.01 mm logical pixels
    FImagePxWidth: Integer;  // in device pixels
    FImagePxHeight: Integer; // in device pixels

    procedure DeleteImage;
    function GetAuthor: String;
    function GetDescription: String;
    function GetHandle: HENHMETAFILE;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
    procedure SetHandle(Value: HENHMETAFILE);
    procedure SetMMHeight(Value: Integer);
    procedure SetMMWidth(Value: Integer);
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear; {$ifdef LCL} override; {$endif}
    procedure LoadFromFile(const Filename: String); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const Filename: String); override;
    procedure SaveToStream(Stream: TStream); override;


    function ReleaseHandle: HENHMETAFILE;
    property Handle: HENHMETAFILE read GetHandle write SetHandle;
    property Empty: boolean read GetEmpty;

    property CreatedBy: String read GetAuthor;
    property Description: String read GetDescription;


    property MMWidth: Integer read GetMMWidth write SetMMWidth;
    property MMHeight: Integer read GetMMHeight write SetMMHeight;
  end;

  TMetafileCanvas = class(TCanvas)
  private
    FMetafile: TMetafile;
  public
    constructor Create(AMetafile: TMetafile; ReferenceDevice: HDC);
    constructor CreateWithComment(AMetafile: TMetafile; ReferenceDevice: HDC;
      const CreatedBy, Description: String);
    destructor Destroy; override;
  end;
{$endif}

  TMetaFilePrinter = class(ThtPagePrinter)
  private
    function GetMetaFile(I: integer): TMetaFile;
  protected
    procedure CreatePage(var NextPage: TObject; var NextCanvas: TCanvas); override;
  public
    property MetaFiles[I: integer]: TMetaFile read GetMetaFile;
  end;

{$endif NoMetaFile}

implementation

{$ifndef NoMetaFile}
{$ifndef NoGDIPlus}
uses
  GDIPL2A;
{$endif NoGDIPlus}

const
  INCH_TO_CM = 2.54;

{$ifdef MetaFileMissing}

{ TMetafile }

procedure TMetafile.DeleteImage;
begin
  if FImageHandle <> 0 then
     DeleteEnhMetafile(FImageHandle);
   FImageHandle := 0;
end;

function TMetafile.GetAuthor: String;
var
  NC: Integer;
begin
  Result := '';
  if FImageHandle = 0 then Exit;

  NC := GetEnhMetafileDescription(FImageHandle, 0, nil);
  if NC <= 0 then Exit
  else begin
     SetLength(Result, NC);
     GetEnhMetafileDescription(FImageHandle, NC, PChar(Result));
     SetLength(Result, StrLen(PChar(Result)) );
  end;
end;

function TMetafile.GetDescription: String;
var
  NC: Integer;
begin
  Result := '';
  if FImageHandle = 0 then Exit;

  NC := GetEnhMetafileDescription(FImageHandle, 0, nil);
  if NC <= 0 then Exit
  else begin
     SetLength(Result, NC);
     GetEnhMetafileDescription(FImageHandle, NC, PChar(Result));
     Delete(Result, 1, StrLen(PChar(Result))+1);
     SetLength(Result, StrLen(PChar(Result)));
  end;
end;

function TMetafile.GetEmpty: Boolean;
begin
 Result := (FImageHandle = 0);
end;


function TMetafile.GetHandle: HENHMETAFILE;
begin
    Result := FImageHandle
end;


function TMetafile.GetMMHeight: Integer;
begin
  Result := FImageMMHeight;
end;

function TMetafile.GetMMWidth: Integer;
begin
  Result := FImageMMWidth;
end;


procedure TMetafile.SetHandle(Value: HENHMETAFILE);
var
  EnhHeader: TEnhMetaHeader;
begin
  if (Value <> 0) and (GetEnhMetafileHeader(Value, sizeof(EnhHeader), @EnhHeader) = 0) then
     raise EInvalidImage.Create('Invalid Metafile');;

  if FImageHandle <> 0 then DeleteImage;

  FImageHandle := Value;
  FImagePxWidth := 0;
  FImagePxHeight := 0;
  FImageMMWidth := EnhHeader.rclFrame.Right - EnhHeader.rclFrame.Left;
  FImageMMHeight := EnhHeader.rclFrame.Bottom - EnhHeader.rclFrame.Top;
end;


procedure TMetafile.SetMMHeight(Value: Integer);
begin
  FImagePxHeight := 0;
  if FImageMMHeight <> Value then FImageMMHeight := Value;
end;

procedure TMetafile.SetMMWidth(Value: Integer);
begin
  FImagePxWidth := 0;
  if FImageMMWidth <> Value then FImageMMWidth := Value;
end;

procedure TMetafile.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  RT: TRect;
begin
  if FImageHandle = 0 then Exit;
  RT := Rect;
  PlayEnhMetaFile(ACanvas.Handle, FImageHandle, RT);
end;

function TMetafile.GetHeight: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
     Result := FImagePxHeight
  else begin               // convert 0.01mm units to device pixels
       GetEnhMetaFileHeader(FImageHandle, Sizeof(EMFHeader), @EMFHeader);
       Result := MulDiv(FImageMMHeight,               // metafile height in 0.01mm
         EMFHeader.szlDevice.cy,                      // device height in pixels
         EMFHeader.szlMillimeters.cy*100);            // device height in mm
     end
end;

function TMetafile.GetWidth: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
    if FImageHandle = 0 then
     Result := FImagePxWidth
  else begin     // convert 0.01mm units to device pixels
        GetEnhMetaFileHeader(FImageHandle, Sizeof(EMFHeader), @EMFHeader);
        Result := MulDiv(FImageMMWidth,                // metafile width in 0.01mm
          EMFHeader.szlDevice.cx,                      // device width in pixels
          EMFHeader.szlMillimeters.cx*100);            // device width in 0.01mm
      end
end;


procedure TMetafile.SetHeight(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
      if FImageHandle = 0 then
       FImagePxHeight := Value
    else begin                 // convert device pixels to 0.01mm units
       GetEnhMetaFileHeader(FImageHandle, Sizeof(EMFHeader), @EMFHeader);
       MMHeight := MulDiv(Value,                      // metafile height in pixels
          EMFHeader.szlMillimeters.cy*100,             // device height in 0.01mm
          EMFHeader.szlDevice.cy);                     // device height in pixels
    end
end;

procedure TMetafile.SetWidth(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
     FImagePxWidth := Value
  else begin                 // convert device pixels to 0.01mm units
        GetEnhMetaFileHeader(FImageHandle, Sizeof(EMFHeader), @EMFHeader);
        MMWidth := MulDiv(Value,                      // metafile width in pixels
          EMFHeader.szlMillimeters.cx*100,            // device width in mm
          EMFHeader.szlDevice.cx);                    // device width in pixels
  end
end;

constructor TMetafile.Create;
begin
  inherited Create;
  FImageHandle := 0;
end;

destructor TMetafile.Destroy;
begin
  DeleteImage;
  inherited Destroy;
end;

procedure TMetafile.Assign(Source: TPersistent);
begin
  if (Source = nil) or (Source is TMetafile) then begin
    if FImageHandle <> 0 then DeleteImage;
    if Assigned(Source) then begin
      FImageHandle := TMetafile(Source).Handle;
      FImageMMWidth := TMetafile(Source).MMWidth;
      FImageMMHeight := TMetafile(Source).MMHeight;
      FImagePxWidth := TMetafile(Source).Width;
      FImagePxHeight := TMetafile(Source).Height;
    end
  end
  else
    inherited Assign(Source);
end;


procedure TMetafile.Clear;
begin
{$ifdef LCL}
  inherited Clear;
{$endif}
  DeleteImage;
end;

procedure TMetafile.LoadFromFile(const Filename: String);
begin
      raise EComponentError.Create('Not Implemented');
end;

procedure TMetafile.SaveToFile(const Filename: String);
begin
      raise EComponentError.Create('Not Implemented');
end;

procedure TMetafile.LoadFromStream(Stream: TStream);
begin
      raise EComponentError.Create('Not Implemented');
end;

procedure TMetafile.SaveToStream(Stream: TStream);
begin
      raise EComponentError.Create('Not Implemented');
end;

function TMetafile.ReleaseHandle: HENHMETAFILE;
begin
  DeleteImage;
  Result := FImageHandle;
  FImageHandle := 0;
end;

{ TMetafileCanvas }

constructor TMetafileCanvas.Create(AMetafile: TMetafile; ReferenceDevice: HDC);
begin
  CreateWithComment(AMetafile, ReferenceDevice, AMetafile.CreatedBy,
    AMetafile.Description);
end;

constructor TMetafileCanvas.CreateWithComment(AMetafile: TMetafile;
  ReferenceDevice: HDC; const CreatedBy, Description: String);
var
  RefDC: HDC;
  R: TRect;
  Temp: HDC;
  P: PChar;
begin
  inherited Create;
  FMetafile := AMetafile;

  if ReferenceDevice = 0 then RefDC := GetDC(0)
  else RefDC := ReferenceDevice;

  try
    if FMetafile.MMWidth = 0 then begin
      if FMetafile.Width = 0 then //if no width get RefDC height
        FMetafile.MMWidth := GetDeviceCaps(RefDC, HORZSIZE)*100
      else FMetafile.MMWidth := MulDiv(FMetafile.Width, //else convert
              GetDeviceCaps(RefDC, HORZSIZE)*100, GetDeviceCaps(RefDC, HORZRES));
    end;

    if FMetafile.MMHeight = 0 then begin
      if FMetafile.Height = 0 then //if no height get RefDC height
        FMetafile.MMHeight := GetDeviceCaps(RefDC, VERTSIZE)*100
      else FMetafile.MMHeight := MulDiv(FMetafile.Height, //else convert
              GetDeviceCaps(RefDC, VERTSIZE)*100, GetDeviceCaps(RefDC, VERTRES));
    end;

    R := Rect(0,0,FMetafile.MMWidth,FMetafile.MMHeight);
    //lpDescription stores both author and description
    if (Length(CreatedBy) > 0) or (Length(Description) > 0) then
      P := PChar(CreatedBy+#0+Description+#0#0)
    else
      P := nil;
    Temp := CreateEnhMetafile(RefDC, nil, @R, P);
    if Temp = 0 then raise EOutOfResources.Create('Out of Resources');;
    Handle := Temp;
  finally
    if ReferenceDevice = 0 then ReleaseDC(0, RefDC);
  end;

end;

destructor TMetafileCanvas.Destroy;
begin
  FMetafile.Handle := CloseEnhMetafile(Handle);
  inherited Destroy;
end;

{$endif}

// TMetaFilePrinter

procedure TMetaFilePrinter.CreatePage(var NextPage: TObject; var NextCanvas: TCanvas);
var
  MetaFile: TMetaFile;
begin
  inherited;
  MetaFile := TMetaFile.Create;
  NextPage := MetaFile;

{$IFNDEF NoGDIPlus}
{$ifndef LCL}
  if GDIPlusActive then
    NextCanvas := TMetaFileCanvas.Create(MetaFile, Printer.Handle)
  else
{$endif LCL}
{$ENDIF NoGDIPlus}
    NextCanvas := TMetaFileCanvas.Create(MetaFile, 0);
end;

function TMetaFilePrinter.GetMetaFile(I: integer): TMetaFile;
begin
  Result := Pages[I] as TMetaFile;
end;
{$endif NoMetaFile}

end.
