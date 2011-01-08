unit PanelUnit;

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
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, Htmlview, HTMLSubs;  {ThvPanel defined in htmlsubs}

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Viewer: THTMLViewer;
    LoadButton: TButton;
    PrintButton: TButton;
    procedure LoadButtonClick(Sender: TObject);
    procedure ViewerPanelCreate(Sender: TObject; const AName, AType,
      SRC: String; Panel: ThvPanel);
    procedure ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
    procedure ViewerPanelPrint(Sender: TObject; Panel: ThvPanel;
      const Bitmap: TBitmap);
    procedure PrintButtonClick(Sender: TObject);
  private
    { Private declarations }
    LeftPanel, RightPanel: ThvPanel;
    LeftButton, RightButton: TButton;
    procedure PanelButtonClick(Sender: TObject);
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

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
Viewer.LoadFromFile('Panel.htm');
PrintButton.Enabled := True;
end;

procedure TForm1.ViewerPanelCreate(Sender: TObject; const AName, AType,
  SRC: String; Panel: ThvPanel);
begin
Panel.Color := clBtnFace;
Panel.BorderStyle := bsSingle;
with Panel do
  if CompareText(Name, 'Panel1') = 0 then
    begin
    LeftPanel := Panel;     
    LeftButton := TButton.Create(Panel);
    with LeftButton do
      begin
      Caption := 'Try Me';
      OnClick := PanelButtonClick;
      Top := 100;
      Left := 15;
      LeftButton.Parent := LeftPanel;
      end;
    end
  else
    begin
    RightPanel := Panel;
    RightButton := TButton.Create(Panel);
    with RightButton do
      begin
      Caption := 'Try Me';
      OnClick := PanelButtonClick;
      Top := 100;
      Left := 15;
      RightButton.Parent := RightPanel;
      Hide;
      end;
    end
end;

procedure TForm1.ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
begin
{not needed}
end;

procedure TForm1.PanelButtonClick(Sender: TObject);
begin
if Sender = LeftButton then
  begin
  LeftPanel.Color := $2FFc0c0;
  LeftPanel.Caption := 'We''re Blue';
  RightPanel.Color := $2FFc0c0;
  RightPanel.Caption := 'We''re Blue';
  LeftButton.Hide;
  RightButton.Show;
  end
else
  begin
  LeftPanel.Color := $2c0c0FF;
  LeftPanel.Caption := 'We''re Red';
  RightPanel.Color := $2c0c0FF;
  RightPanel.Caption := 'We''re Red';
  LeftButton.Show;
  RightButton.Hide;
  end;
end;

procedure TForm1.ViewerPanelPrint(Sender: TObject; Panel: ThvPanel;
  const Bitmap: TBitmap);
{OnPanelPrint handler}  
begin
Bitmap.Canvas.Lock;
Panel.PaintTo(Bitmap.Canvas.handle, 0, 0);
Bitmap.Canvas.UnLock;
end;

procedure TForm1.PrintButtonClick(Sender: TObject);
begin
{$ifdef MsWindows}
Viewer.Print(1, 1);
{$endif}
end;

end.
