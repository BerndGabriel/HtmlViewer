unit HTMLAbt;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, StdCtrls, Buttons, Htmlview, ExtCtrls, HTMLUn2;

{$INCLUDE htmlcons.inc}

type
    TAboutBox = class(TForm)
        BitBtn1 : TBitBtn;
        Panel1 : TPanel;
        Viewer : THTMLViewer;
    private
        { Private declarations }
    public
        { Public declarations }
        constructor CreateIt(Owner : TComponent;
            const ProgName, CompName : String);
    end;

var
    AboutBox : TAboutBox;

implementation

{$R *.DFM}

function ConfigInfo : String;
begin
    Result := '<ul><li>compiled with ' +
{$IFDEF Ver90}
        'Delphi 2'
{$ENDIF}
{$IFDEF Ver93}
        'C++Builder 1'
{$ENDIF}
{$IFDEF Ver100}
        'Delphi 3'
{$ENDIF}
{$IFDEF Ver110}
        'C++Builder 3'
{$ENDIF}
{$IFDEF Ver120}
        'Delphi 4'
{$ENDIF}
{$IFDEF Ver125}
        'C++Builder 4'
{$ENDIF}
{$IFDEF Ver130}
{$IFDEF BCB}
        'C++Builder 5'
{$ELSE}
        'Delphi 5'
{$ENDIF}
{$ENDIF}
{$IFDEF Ver140}
        'Delphi 6'
{$ENDIF}
{$IFDEF Ver150}
        'Delphi 7'
{$ENDIF}
{$IFDEF Ver170}
        'Delphi 2005'
{$ENDIF}
{$IFDEF Ver185}
        'Delphi 2007'
{$ELSE}
{$IFDEF Ver180}
        'Delphi 2006'
{$ENDIF}
{$ENDIF}
{$IFDEF Ver200}
        'Delphi 2009'
{$ENDIF}
{$IFDEF Ver210}
        'Delphi 2010'
{$ENDIF}
{$IFDEF Ver220}
        'Delphi XE'
{$ENDIF}
{$IFDEF Ver230}
        'Delphi XE2'
{$ENDIF}
{$IFDEF LCL}
        'Lazarus ' + lcl_version
{$ENDIF}
        ;

{$IFDEF UseTNT}
    Result := Result + '<li>Using TNT unicode controls.';
{$ELSE}
{$IFDEF UseElPack}
    Result := Result + '<li>Using ElPack unicode controls.';
{$ELSE}
{$IFDEF UNICODE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL unicode character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL unicode character controls.';
{$ENDIF}
{$ELSE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL single byte character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL single byte character controls.';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
    Result := Result + '</ul>';
end;

constructor TAboutBox.CreateIt(Owner : TComponent;
    const ProgName, CompName : String);
var
    S : String;
begin
    inherited Create(Owner);
    Viewer.DefFontName  := 'MS Sans Serif';
    Viewer.DefFontSize  := 9;
    Viewer.DefFontColor := clNavy;
    S := '<body bgcolor="ffffeb" text="000080">' + '<center>' + '<h1>' +
        ProgName + '</h1>' + '<font color="Maroon">A demo program for the ' +
        CompName + ' component</font>' + '<p>Version ' + VersionNo + '</center>'
        + ConfigInfo + '</body>';
    Viewer.LoadFromString(S);
end;

end.
