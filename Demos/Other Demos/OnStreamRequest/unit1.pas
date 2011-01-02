
{Demo to illustrate the use of the OnStreamRequest and OnImageRequest events
 with TFrameViewer.

 For the purpose of this demo, the streams are obtained from existing files,
 the demo.htm files in the C:\THTML(Demo) directory (assiming the default
 installation is used).

 Usage:

 Enter the Base path where the demo.htm files are located in the TEdit control
 and press "Load Demo.htm".
}

unit Unit1;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FramView, ReadHTML;

type
  TForm1 = class(TForm)
    FrameViewer: TFrameViewer;
    Panel1: TPanel;
    LoadButton: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure LoadButtonClick(Sender: TObject);
    procedure FrameViewerStreamRequest(Sender: TObject; const SRC: String;
      var Stream: TStream);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FrameViewerImageRequest(Sender: TObject; const SRC: String;
      var Stream: TMemoryStream);
  private
    { Private declarations }
    AStream: TMemoryStream;
    Base: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
{create general purpose Stream}
AStream := TMemoryStream.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
AStream.Free;
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
{read the Base}
Base := Edit1.Text;
{start with the "Load" method}
FrameViewer.Load('Demo.htm');
end;

procedure TForm1.FrameViewerStreamRequest(Sender: TObject;
  const SRC: String; var Stream: TStream);
{this is the OnStreamRequest handler}
var
  Name: string;
  S: AnsiString;
begin
if Pos(':', Src) = 0 then  {don't add Base if filename already complete}
  Name := Base+Src
else Name := Src;
try
  AStream.LoadFromFile(Name);
except
  S := 'Cannot load '+Name;
  AStream.Clear;
  AStream.Write(S[1], Length(S));
  end;
Stream := AStream;
end;

procedure TForm1.FrameViewerImageRequest(Sender: TObject;
  const SRC: String; var Stream: TMemoryStream);
{this is the OnImageRequest handler}  
var
  Name: string;
begin
if Pos(':', Src) = 0 then    {don't add Base if filename already complete}
  Name := Base+Src
else Name := Src;
try
  AStream.LoadFromFile(Base+Src);
  Stream := AStream;
except
  Stream := Nil;
  end;
end;

end.
