unit AVIUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Htmlview, ExtCtrls, mmSystem, MPlayer, 
  htmlsubs, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Viewer: THTMLViewer;
    MediaPlayer: TMediaPlayer;
    LoadButton: TButton;
    PlayButton: TButton;
    procedure ViewerPanelCreate(Sender: TObject; const AName, AType,
      SRC: String; Panel: ThvPanel);
    procedure ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
    procedure LoadButtonClick(Sender: TObject);
    procedure PlayButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    APanel: ThvPanel;
    Memo: TMemo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ViewerPanelCreate(Sender: TObject; const AName, AType,
  SRC: String; Panel: ThvPanel);
{the OnPanelCreate event}
begin
with Panel do
  begin
  color := clBtnFace;
  BorderStyle := bsSingle;
  if CompareText(AName, 'AviPanel') = 0 then
    begin
    APanel := Panel;
    Memo := TMemo.Create(APanel);
    Memo.Align := alClient;
    Memo.Hide;
    Memo.Parent := APanel;
    PlayButton.Enabled := True;
    end;
  end;
end;

procedure TForm1.ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
{the OnPanelDestroy event}
begin
MediaPlayer.Wait := True;
MediaPlayer.Close;   {in case it's playing}
APanel := Nil;
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
Viewer.LoadFromFile('AviPanel.htm');
end;

procedure TForm1.PlayButtonClick(Sender: TObject);
begin
Memo.Hide;
If Assigned(APanel) then
  With MediaPlayer do
    if FileExists('speedis.avi') then
      begin
      Filename := 'speedis.avi';
      DeviceType := dtAutoSelect;
      Display := APanel; //Set display device to the Panel
      Open;
      Play;
      end
    else
      begin
      Memo.Text := 'You forgot to get the "speedis.avi" file!';
      Memo.Font.Size := 12;
      Memo.Show;
      end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
MediaPlayer.Wait := True;
MediaPlayer.Close;  
CanClose := True;
end;

end.
