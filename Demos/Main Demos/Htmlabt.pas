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

unit Htmlabt;

{$include ..\..\source\htmlcons.inc}

interface

uses
{$ifdef LCL}
  LCLVersion, LCLIntf, InterfaceBase,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons, ExtCtrls,
  HtmlGlobals, Htmlview, HTMLUn2, StdCtrls;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Viewer: THTMLViewer;
  public
    constructor CreateIt(Owner: TComponent; const ProgName, CompName: string; const Remarks: string = ''; const Styles: string = ''); overload;
    constructor CreateIt(Owner: TComponent; const Message: ThtString); overload;
  end;

implementation

{$if lcl_fullversion >= 1080000}
uses
  LCLPlatformDef;
{$ifend}

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

function ConfigInfo: String;
begin
  Result := '<ul><li>Compiled with ' +
{$ifdef Ver90}
    'Delphi 2'
{$endif}
{$ifdef Ver93}
    'C++Builder 1'
{$endif}
{$ifdef Ver100}
    'Delphi 3'
{$endif}
{$ifdef Ver110}
    'C++Builder 3'
{$endif}
{$ifdef Ver120}
    'Delphi 4'
{$endif}
{$ifdef Ver125}
    'C++Builder 4'
{$endif}
{$ifdef Ver130}
  {$ifdef BCB}
    'C++Builder 5'
  {$else}
    'Delphi 5'
  {$endif}
{$endif}
{$ifdef Ver140}
    'Delphi 6'
{$endif}
{$ifdef Ver150}
    'Delphi 7'
{$endif}
{$ifdef Ver170}
    'Delphi 2005'
{$endif}
{$ifdef Ver185}
    'Delphi 2007'
{$else}
  {$ifdef Ver180}
      'Delphi 2006'
  {$endif}
{$endif}
{$ifdef Ver200}
    'Delphi 2009'
{$endif}
{$ifdef Ver210}
    'Delphi 2010'
{$endif}
{$ifdef Ver220}
    'Delphi XE'
{$endif}
{$ifdef Ver230}
    'Delphi XE2'
{$endif}
{$ifdef Ver240}
    'Delphi XE3'
{$endif}
{$ifdef Ver250}
    'Delphi XE4'
{$endif}
{$ifdef Ver260}
    'Delphi XE5'
{$endif}
{$ifdef Ver270}
    'Delphi XE6'
{$endif}
{$ifdef Ver280}
    'Delphi XE7'
{$endif}
{$IFDEF Ver290}
    'Delphi XE8'
{$ENDIF}
{$IFDEF Ver300}
    'Delphi 10 Seattle'
{$ENDIF}
{$IFDEF Ver310}
    'Delphi 10.1 Berlin'
{$ENDIF}
{$IFDEF Ver320}
    'Delphi 10.2 Tokyo'
{$ENDIF}
{$IFDEF Ver330}
    'Delphi 10.3 Rio'
{$ENDIF}
{$IFDEF Ver340}
    'Delphi 10.4 Sydney'
{$ENDIF}
{$IFDEF Ver350}
    'Delphi 11 Alexandria'
{$ENDIF}
{$ifdef LCL}
    'Lazarus ' + lcl_version
{$endif}
    ;

{$ifdef win64}
  Result := Result + '<li>Compiled for Win64</li>';
{$endif}
{$ifdef win32}
  Result := Result + '<li>Compiled for Win32</li>';
{$endif}
{$ifdef wince}
  Result := Result + '<li>Compiled for WinCE</li>';
{$endif}
{$ifdef unix}
  Result := Result + '<li>Compiled for Unix';
  {$ifdef LCL}
    Result := Result + ' (' + LCLPlatformDirNames[WidgetSet.LCLPlatform] +')';
  {$endif}
  Result := Result + '</li>';
{$endif}
{$ifdef UseTNT}
  Result := Result + '<li>Using TNT unicode controls.';
{$else}
  {$ifdef UseElPack}
    Result := Result + '<li>Using ElPack unicode controls.';
  {$else}
    {$ifdef UNICODE}
      {$ifdef LCL}
        Result := Result + '<li>Using LCL unicode character controls.';
      {$else}
        Result := Result + '<li>Using VCL unicode character controls.';
      {$endif}
    {$else}
      {$ifdef LCL}
        Result := Result + '<li>Using LCL multi byte character controls.';
      {$else}
        Result := Result + '<li>Using VCL single byte character controls.';
      {$endif}
    {$endif}
  {$endif}
{$endif}

  Result := Result + '</ul>';
end;


constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName, Remarks, Styles: string);
var
  Head: string;
  Body: string;
begin
  inherited Create(Owner);
  inherited Loaded;

{$ifdef HasGestures}
  Viewer.Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia];
  Viewer.Touch.InteractiveGestures := [igPan];
{$endif}

  Viewer.DefFontName := FontSans;
  Viewer.DefFontSize := 9;
  Viewer.DefFontColor := clNavy;
  if Length(Styles) > 0 then
    Head :=
      '<head>' +
      '<style>' +
      Styles +
      '</style>' +
      '</head>';

  Body :=
    '<body text="000080">' +
    '<center>' +
    '<h1>' + ProgName + '</h1>';

  if Length(CompName) > 0 then
    Body := Body +
      '<font color="Maroon">A demo program for the <b>' + CompName + '</b> component</font>' +
      '<h3>Version ' + VersionNo + '</h3>';

  Body := Body +
    Remarks +
    '</center>' +
    ConfigInfo +
    '</body>';
  Viewer.Text := '<html>' + Head + Body + '</html>';
  if Viewer.ClientHeight < Viewer.MaxVertical then
    Height := Height + Viewer.MaxVertical - Viewer.ClientHeight;
end;


constructor TAboutBox.CreateIt(Owner: TComponent;
  const Message: ThtString);
begin
  inherited Create(Owner);
  inherited Loaded;
  if Owner is TCustomForm then
    Caption := TCustomForm(Owner).Caption;
  Viewer.DefFontName := FontSans;
  Viewer.DefFontSize := 12;
  Viewer.DefFontColor := clBlack;
  Viewer.Text := '<body>' + Message + '</body>';
end;

end.
