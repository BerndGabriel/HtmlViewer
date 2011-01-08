unit HTMLAbt;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls, HTMLUn2;

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
    '<p>Version '+Version+ 
    '</center>'+
    '</body>';
Viewer.LoadFromBuffer(@S[1], Length(S));
end;

end.
