unit HTMLAbt;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls, HTMLUn2;

{$include htmlcons.inc}

type
  TAboutBox = class(TForm)
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    Viewer: THTMLViewer;
  private
    { Private declarations }
  public
    { Public declarations }
    constructor CreateIt(Owner: TComponent; const ProgName, CompName: string);
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}


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
  S: string;
begin
inherited Create(Owner);
Viewer.DefFontName := 'MS Sans Serif';
Viewer.DefFontSize := 9;
Viewer.DefFontColor := clNavy;
S :='<body bgcolor="ffffeb" text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
    '<p>Version '+VersionNo+
    '</center>'+
     ConfigInfo +
    '</body>';
Viewer.LoadFromString(S);
end;

end.
