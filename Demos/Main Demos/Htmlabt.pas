{
Version   11.2
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2010-2012 by Bernd Gabriel

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
  LCLVersion,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons, ExtCtrls,
  HtmlGlobals, Htmlview, StdCtrls, HTMLUn2;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Viewer: THTMLViewer;
  public
    constructor CreateIt(Owner: TComponent; const ProgName, CompName: string); overload;
    constructor CreateIt(Owner: TComponent; const Message: ThtString); overload;
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

function ConfigInfo: String;
begin
  Result := '<ul><li>compiled with ' +
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
{$ifdef LCL}
    'Lazarus ' + lcl_version
{$endif}
    ;

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
        Result := Result + '<li>Using LCL single byte character controls.';
      {$else}
        Result := Result + '<li>Using VCL single byte character controls.';
      {$endif}
    {$endif}
  {$endif}
{$endif}

  Result := Result + '</ul>';
end;


constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
var
  S: String;
begin
  inherited Create(Owner);
  inherited Loaded;
  Viewer.DefFontName := 'MS Sans Serif';
  Viewer.DefFontSize := 9;
  Viewer.DefFontColor := clNavy;
  S :='<body text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
    '<h3>Version '+ VersionNo +'</h3>'+
    '</center>'+
    ConfigInfo +
    '</body>';
  Viewer.LoadFromString(S);
end;


constructor TAboutBox.CreateIt(Owner: TComponent;
  const Message: ThtString);
begin
  inherited Create(Owner);
  inherited Loaded;
  if Owner is TCustomForm then
    Caption := TCustomForm(Owner).Caption;
  Viewer.DefFontName := 'Verdana';
  Viewer.DefFontSize := 12;
  Viewer.DefFontColor := clBlack;
  Viewer.LoadFromString('<body>' + Message + '</body>');
end;

end.
