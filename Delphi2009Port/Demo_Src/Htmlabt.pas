unit HTMLAbt;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls;

const
  Version = '9.45';

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

constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
var
  S: string[210];
begin
inherited Create(Owner);
Viewer.DefFontName := 'MS Sans Serif';
Viewer.DefFontSize := 9;
Viewer.DefFontColor := clNavy;
S :='<body bgcolor="ffffeb" text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
    '<h3>Version '+Version+' compiled with Delphi '+
{$ifdef Windows}
    '1</h3>'+
{$endif}
{$ifdef Ver90}
    '2</h3>'+
{$endif}
{$ifdef Ver100}
    '3</h3>'+
{$endif}
{$ifdef Ver120}
    '4</h3>'+
{$endif}
{$ifdef Ver130}
    '5</h3>'+
{$endif}
{$ifdef Ver140}
    '6</h3>'+
{$endif}
{$ifdef Ver150}
    '7</h3>'+
{$endif}
{$ifdef Ver170}
    '2005</h3>'+
{$endif}
{$ifdef Ver180}
    '2006</h3>'+
{$endif}
    '</center>'+
    '</body>';
Viewer.LoadFromBuffer(@S[1], Length(S), '');
end;

end.
