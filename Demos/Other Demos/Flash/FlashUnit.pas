
{
Demo program to show how to use the OnObjectTag event to display Flash .swf files.

Most computers will have the Flash.ocx already installed.  To set it up in
Delphi:

1. Click on Component|Import ActiveX Control...
2. Highlight the item "Shockwave Flash (Version 1.0)"
3. The Classname listed should be "TShockwave Flash".
4. Click "Create Unit".  (It is not necessary to install the component.)

This will create a unit named ShockwaveFlashObjects_TLB.pas.
}



unit FlashUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFNDEF FPC}
  ShockwaveFlashObjects_TLB, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, Htmlview, HTMLSubs;  {ThvPanel defined in htmlsubs}

type
  TForm1 = class(TForm)
    Viewer: THTMLViewer;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open: TMenuItem;
    Print1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure ViewerObjectTag(Sender: TObject; Panel: ThvPanel;
      const Attributes, Params: TStringList; var WantPanel: Boolean);
    procedure ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
    procedure ViewerPanelPrint(Sender: TObject; Panel: ThvPanel;
      const Bitmap: TBitmap);
    procedure PrintClick(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.PrintClick(Sender: TObject);
begin
{$ifdef MsWindows}
Viewer.Print(1, 1);
{$endif}
end;

procedure TForm1.ViewerObjectTag(Sender: TObject; Panel: ThvPanel;
  const Attributes, Params: TStringList; var WantPanel: Boolean);
var
  Movie, S: string;
  Flash: TShockwaveFlash;
begin
WantPanel := False;
if CompareText(Attributes.Values['classid'],
       'clsid:d27cdb6e-ae6d-11cf-96b8-444553540000') = 0 then
  try
    Movie := Viewer.HTMLExpandFileName(Params.Values['movie']);
    if FileExistsUTF8(Movie) { *Converted from FileExists*  } then
      begin
      Flash := TShockwaveFlash.Create(Panel);
      {set Height and Width before loading Movie}
      Flash.Width := Panel.ClientWidth;
      Flash.Height := Panel.ClientHeight;
      Flash.Align := alClient;       
      Flash.Parent := Panel;
      Flash.Movie := Movie;
      if Params.Values['Quality'] <> '' then
        Flash.Quality2 := Params.Values['Quality'];
      if Params.Values['bgColor'] <> '' then
        Flash.bgColor := Params.Values['bgColor'];

      S := Lowercase(Params.Values['loop']);
      if S = 'true' then   {check default on this}
        Flash.Loop := True
      else if S = 'false' then
        Flash.Loop := False;

      S := Lowercase(Params.Values['playing']);
      if S = 'true' then   {check default on this}
        Flash.Playing := True
      else if S = 'false' then
        Flash.Playing := False;

      WantPanel := True;
      end;
  except
    end;
end;

procedure TForm1.ViewerPanelDestroy(Sender: TObject; Panel: ThvPanel);
begin
{not needed}
end;

procedure TForm1.ViewerPanelPrint(Sender: TObject; Panel: ThvPanel;
  const Bitmap: TBitmap);
{OnPanelPrint handler}
begin
Bitmap.Canvas.Lock;
Panel.PaintTo(Bitmap.Canvas.handle, 0, 0);
Bitmap.Canvas.UnLock;
end;

procedure TForm1.OpenFileClick(Sender: TObject);
begin
if Viewer.CurrentFile <> '' then
  OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile)
else OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
if OpenDialog.Execute then
  begin
  Update;  
  Viewer.LoadFromFile(OpenDialog.Filename);
  Caption := Viewer.DocumentTitle;
  Print1.Enabled := True;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
Close;
end;

end.
