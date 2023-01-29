{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
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

unit HtmlFonts;

interface

uses
{$ifdef UseVCLStyles}
  System.Types,
  System.UITypes,
  Vcl.Themes,
{$endif}
{$ifdef LCL}
  LclIntf, LclType,
{$else}
  {$ifdef MSWindows}
    Windows,
  {$endif}
{$endif}
  SysUtils,
  Graphics, Classes, Forms, Contnrs, Variants,
{$ifdef UseGenerics}
  System.Generics.Collections,
{$endif}
  //
  HtmlGlobals,
  StyleTypes;

type
  ThtFontInfo = record
    iName: ThtString;
    iSize: Double;
    iStyle: TFontStyles;
    iColor: TColor;
    ibgColor: TColor;
    iCharSet: TFontCharSet;
    iCharExtra: Variant;
    iWeight: Integer;
  end;

  // link state
  FIIndex = (
    LFont,  // initial link
    VFont,  // visited link
    HLFont, // hovered link
    HVFont  // hovered visited link
  );

  TFontInfoArray = class
  public
    Ar: array[LFont..HVFont] of ThtFontInfo;
    procedure Assign(Source: TFontInfoArray);
  end;

  ThtFont = class(TFont)
  public
    bgColor: TColor;
    tmHeight: Integer;
    tmDescent: Integer;
    tmExternalLeading: Integer;
    tmAveCharWidth: Integer;
    tmMaxCharWidth: Integer;
    tmCharset: Integer;
    CharExtra: Integer;
    EmSize: Integer;
    ExSize: Integer;
    constructor Create; {$ifdef LCL} override; {$endif}
    procedure Assign(const Info: ThtFontInfo; APixelsPerInch: Integer); reintroduce; overload;
    procedure Assign(Source: TPersistent); overload; override;
    procedure AssignToCanvas(Canvas: TCanvas);
  end;

{$ifdef UseGenerics}
  ThtFontList = class(TObjectList<ThtFont>);
{$else}
  ThtFontList = class(TObjectList);
{$endif}

  ThtFontCache = class
  private
    FFontsByName: ThtStringList;
    procedure Add(Font: ThtFont);
    function Find(const FontInfo: ThtFontInfo; PixelsPerInch: Integer): ThtFont;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetFontLike(var Font: ThtFontInfo; PixelsPerInch: Integer): ThtFont;
  end;

function AllMyFonts: ThtFontCache;

implementation

var
  VFontCache: ThtFontCache;

//-- BG ---------------------------------------------------------- 02.05.2011 --
function AllMyFonts: ThtFontCache;
// do not inline as it would required to move local var to interface section. {$ifndef UseInline} inline; {$endif}
begin
  if VFontCache = nil then
    VFontCache := ThtFontCache.Create;
  Result := VFontCache;
end;

{ TFontInfoArray }

procedure TFontInfoArray.Assign(Source: TFontInfoArray);
var
  I: FIIndex;
begin
  for I := LFont to HVFont do
    Ar[I] := Source.Ar[I];
end;

{----------------TMyFont.Assign}

procedure ThtFont.Assign(Source: TPersistent);
var
  SourceFont: ThtFont absolute Source;
begin
  if Source is ThtFont then
  begin
    PixelsPerInch := SourceFont.PixelsPerInch;
    bgColor := SourceFont.bgColor;
    tmHeight := SourceFont.tmHeight;
    tmDescent := SourceFont.tmDescent;
    tmExternalLeading := SourceFont.tmExternalLeading;
    tmAveCharWidth := SourceFont.tmAveCharWidth;
    tmMaxCharWidth := SourceFont.tmMaxCharWidth;
    tmCharset := SourceFont.tmCharset;
    CharExtra := SourceFont.CharExtra;
    EmSize := SourceFont.EmSize;
    ExSize := SourceFont.ExSize;
  end;
  inherited Assign(Source);
end;

//-- BG ---------------------------------------------------------- 12.03.2011 --
procedure ThtFont.Assign(const Info: ThtFontInfo; APixelsPerInch: Integer);
begin
  Name := htStringToString(Info.iName);
  {$ifdef LCL}
    PixelsPerInch := APixelsPerInch;
  {$endif}
  Height := -Round(Info.iSize * (APixelsPerInch / 72.0));
  Style := Info.iStyle;
  bgColor := Info.ibgColor;
  Color := Info.iColor;
  CharSet := Info.iCharSet;
end;

procedure ThtFont.AssignToCanvas(Canvas: TCanvas);
begin
  Canvas.Font := Self;
{$ifdef Windows}
  SetTextCharacterExtra(Canvas.Handle, CharExtra);
{$endif}
end;

constructor ThtFont.Create;
begin
  inherited;
  Charset := DEFAULT_CHARSET;
end;

{ TMyFontCache }

//-- BG ---------------------------------------------------------- 30.01.2011 --
procedure ThtFontCache.Add(Font: ThtFont);
var
  I: Integer;
  FontName: ThtString;
begin
  FontName := htLowerCase(htString(Font.Name));
  I := -1;
  if not FFontsByName.Find(FontName, I) then
  begin
    I := FFontsByName.Add(FontName);
    FFontsByName.Objects[I] := ThtFontList.Create(True);
  end;
  ThtFontList(FFontsByName.Objects[I]).Add(Font);
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
constructor ThtFontCache.Create;
begin
  inherited;
  FFontsByName := ThtStringList.Create;
  FFontsByName.Sorted := True;
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
destructor ThtFontCache.Destroy;
begin
  Clear;
  FFontsByName.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 19.09.2022 --
procedure ThtFontCache.Clear;
var
  I: Integer;
begin
  for I := 0 to FFontsByName.Count - 1 do
    FFontsByName.Objects[I].Free;
  FFontsByName.Clear;
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
function ThtFontCache.Find(const FontInfo: ThtFontInfo; PixelsPerInch: Integer): ThtFont;
var
  iHeight: Integer;

  function SameFonts(F1: ThtFont): Boolean;
  begin
    if F1.Height = iHeight then
      if F1.Style = FontInfo.iStyle then
        if F1.Charset = FontInfo.iCharset then
        begin
          Result := True;
          exit;
        end;
    Result := False;
  end;

var
  I: Integer;
  Fonts: ThtFontList;
begin
  I := -1;
  if FFontsByName.Find(htLowerCase(FontInfo.iName), I) then
  begin
    iHeight := -Round(FontInfo.iSize * PixelsPerInch / 72.0);
    Fonts := ThtFontList(FFontsByName.Objects[I]);
    for I := 0 to Fonts.Count - 1 do
    begin
      Result := ThtFont(Fonts[I]);
      if SameFonts(Result) then
        exit;
    end;
  end;
  Result := nil;
end;

//-- BG ---------------------------------------------------------- 30.01.2011 --
function ThtFontCache.GetFontLike(var Font: ThtFontInfo; PixelsPerInch: Integer): ThtFont;
var
  SameFont: ThtFont;
  Save: HFONT;
  SaveCharSet: TFontCharSet;
  tm: TTextmetric;
  DC: HDC;
  V: Variant;
  GotTextMetrics: Boolean;
begin
  SameFont := Find(Font, PixelsPerInch);
  if SameFont = nil then
  begin
    SameFont := ThtFont.Create;
    SameFont.Assign(Font, PixelsPerInch);
    Add(SameFont);

    GotTextMetrics := False;
    if SameFont.Height <> 0 then
    begin
      DC := GetDC(0);
      Save := SelectObject(DC, SameFont.Handle);
      try
        // If this is a Symbol charset, then keep it that way.
        // To check the font's real charset, use Default_Charset
        SaveCharSet := SameFont.CharSet;
        SameFont.CharSet := DEFAULT_CHARSET;
        GotTextMetrics := GetTextMetrics(DC, tm);
        if GotTextMetrics then
        begin
          if tm.tmCharset = Symbol_Charset then
            SameFont.Charset := Symbol_CharSet
          else
            SameFont.Charset := SaveCharSet;

          {now get the info on the finalized font}
          if SameFont.Charset <> Default_Charset then {else already have the textmetrics}
            GetTextMetrics(DC, tm);
        end
        else
          SameFont.Charset := SaveCharSet;
      finally
        SelectObject(DC, Save);
        ReleaseDC(0, DC);
      end;
    end;

    if GotTextMetrics then
    begin
      {calculate EmSize with current font rather than inherited}
      SameFont.EmSize := tm.tmHeight - tm.tmInternalLeading;
      SameFont.ExSize := SameFont.EmSize div 2; {apparently correlates with what browsers are doing}
      SameFont.tmHeight := tm.tmHeight;
      SameFont.tmDescent := tm.tmDescent;
      SameFont.tmExternalLeading := tm.tmExternalLeading;
      SameFont.tmMaxCharWidth := tm.tmMaxCharWidth;
      SameFont.tmAveCharWidth := tm.tmAveCharWidth;
      SameFont.tmCharset := tm.tmCharset;
    end;
  end;

  Result := ThtFont.Create;
  Result.Assign(SameFont);
  Result.bgColor := Font.ibgColor;
  Result.Color := Font.iColor;

  V := Font.iCharExtra;
  if VarType(V) in VarInt then
    Result.CharExtra := V
  else if VarIsStr(V) then
    if V <> 'normal' then
    begin
      Result.CharExtra := Round(StrToLength(V, False, Result.EmSize, Result.EmSize, 0, PixelsPerInch));
      Font.iCharExtra := Result.CharExtra;
    end;
end;

initialization
finalization
  FreeAndNil(VFontCache);
end.
