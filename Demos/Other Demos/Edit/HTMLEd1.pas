{
 The purpose of this demo is to illustrate the usage of the new FindDisplayPos,
 FindSourcePos, and DisplayPosToXY methods and SelStart, SelLength properties.
 These new properties and methods relate character positions in the HTML source
 with those in the displayed docuement.

 This demo is a simple two window editor.  Text entry and modifications are made
 in the TRichEdit window but appear immediately in the ThtmlViewer window.
 Positions marked in the ThtmlViewer window are also hilighted in the TRichEdit
 window.

 Since display updates are accomplished by reloading the document, this scheme
 obviously works best with a fast computer and simple documents.

 This demo will not run in Delphi 2 as there is no TSplitter
}

unit HTMLEd1;

{$include ..\..\..\source\htmlcons.inc}

interface

uses
{$IFNDEF FPC}
  ShellAPI, Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, FileUtil,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls,
  HtmlGlobals,
  Htmlview, StyleUn, HTMLUn2;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    New1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    ExitItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    NewHTML: TMenuItem;
    Panel: TPanel;
    Splitter: TSplitter;
    RichEdit: TRichEdit;
    Panel2: TPanel;
    Viewer: THTMLViewer;
    Panel3: TPanel;
    procedure Open1Click(Sender: TObject);
    procedure RichEdChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichEditSelectionChange(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonClick(Sender: TObject);
{$ifdef UNICODE}
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
    procedure ViewerHotSpotCovered(Sender: TObject; const SRC: string);
{$else}
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: WideString; var Handled: Boolean);
    procedure ViewerHotSpotCovered(Sender: TObject; const SRC: WideString);
{$endif}
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
  private
    ViewerOK, RichOK: boolean;
    SizeRatio: double;
    procedure CheckFileSave;
{$ifdef MsWindows}
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
{$endif}
  public
    CurrentFile: String;
    procedure LoadFile(Filename: String);
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
  InitText = '<html>'^m^j'<head>'^m^j'<style>'^m^j^m^j'</style>'^m^j'</head>'^m^j+
      '<body>'^m^j^m^j^m^j'</body>'^m^j'</html>'^m^j;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$ifdef MsWindows}
  DragAcceptFiles(Handle, True);
{$endif}
  RichEdit.Modified := False;
  SizeRatio := 0.5;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  CheckFileSave;
  if CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(CurrentFile)
  else
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
    LoadFile(OpenDialog.Filename);
end;

procedure TForm1.Saveas1Click(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(CurrentFile);
  SaveDialog1.Filename := ExtractFilename(CurrentFile);
  if SaveDialog1.Execute then
  begin
    RichEdit.Lines.SaveToFile(SaveDialog1.FileName);
    CurrentFile := SaveDialog1.FileName;
    Caption := CurrentFile;
    RichEdit.Modified := False;
  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if CurrentFile = '' then
    SaveAs1Click(Sender)
  else
  begin
    RichEdit.Lines.SaveToFile(CurrentFile);
    RichEdit.Modified := False;
  end;
end;

procedure TForm1.RichEdChange(Sender: TObject);
{TRichEdit OnChange handler}
var
  Position: integer;
begin
  if RichOK then
  begin
    Position := Viewer.Position;
    Viewer.LoadFromString(RichEdit.Text);
    ViewerOK := True;
    Viewer.Position := Position;
    RichEditSelectionChange(Nil);
  end;
end;

procedure TForm1.RichEditSelectionChange(Sender: TObject);
{TRichEdit OnSelectionChange handler}
var
  Pos1, Pos2, X, Y, VPos, SStr, SLen: integer;
begin
  if ViewerOK then
  begin
    SStr := RichEdit.SelStart;
    SLen := RichEdit.SelLength;
    if SStr+SLen > Length(RichEdit.Text) then
       SLen := Length(RichEdit.Text)-SStr;
    Pos1 := Viewer.FindDisplayPos(SStr, False);
    if Pos1 < 0 then  {means it's past end}
      Pos1 := Viewer.FindDisplayPos(SStr, True);
    if SLen <> 0 then
    begin
      Pos2 := Viewer.FindDisplayPos(SStr+SLen, False);
      if Pos2 < 0 then
        Pos2 := Viewer.FindDisplayPos(SStr+SLen-1, False); {fix for above}
    end
    else
      Pos2 := Pos1;
    if (Pos1 >= 0) and Viewer.DisplayPosToXY(Pos1, X, Y) then
    begin
      {see if Viewer is positioned properly}
      VPos := Viewer.VScrollBarPosition;
      if (Y < VPos) or
             (Y > VPos +Viewer.ClientHeight-20) then
        Viewer.VScrollBarPosition := (Y - Viewer.ClientHeight div 2);
      Viewer.SelStart := Pos1;
      Viewer.SelLength := Pos2-Pos1;
    end;
  end;
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pos, Pos2, VSelLength: integer;
begin
  if not ViewerOK then Exit;
  ViewerOK := False;
  try
    VSelLength := Viewer.SelLength;
    if VSelLength >= 0 then
      Pos := Viewer.FindSourcePos(Viewer.SelStart)
    else Pos := Viewer.FindSourcePos(Viewer.SelStart+VSelLength);
    if Pos >= 0 then
    begin
      RichEdit.SelStart := Pos;
      if VSelLength = 0 then
        RichEdit.SelLength := 0
      else if VSelLength > 0 then
      begin
        Pos2 := Viewer.FindSourcePos(Viewer.SelStart+VSelLength-1)+1;
        RichEdit.SelLength := Pos2-Pos;
      end
      else
      begin
        Pos2 := Viewer.FindSourcePos(Viewer.SelStart-1)+1;
        RichEdit.SelLength := Pos2-Pos;
      end;
      RichEdit.SetFocus;
{$ifdef MsWindows}
      PostMessage(RichEdit.handle, em_scrollcaret, 0, 0);   {8.03}
{$endif}
    end;
  finally
    ViewerOK := True;
  end;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  if RichEdit.Modified then
    SaveAs1Click(Nil);
  RichEdit.Text := '';
  Viewer.Clear;
  ViewerOK := True;
  RichOK := True;
  if CurrentFile <> '' then
    CurrentFile := ExtractFilePath(CurrentFile)
  else
    CurrentFile := ExtractFilePath(ParamStr(0));
  CurrentFile := CurrentFile+'Untitled.htm';
  Caption := CurrentFile;
  if Sender = NewHTML then
  begin
    RichEdit.Text := InitText;
    RichOK := True;
    RichEdit.Modified := False;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
  if ParamCount >= 1 then
  begin            {Parameter is file to load}
    S := CmdLine;
    I := Pos('" ', S);
    if I > 0 then
      Delete(S, 1, I+1)     {delete EXE name in quotes}
    else
      Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
    I := Pos('"', S);
    while I > 0 do     {remove any quotes from parameter}
    begin
      Delete(S, I, 1);
      I := Pos('"', S);
    end;
    LoadFile(S);
  end
  else
    New1Click(Sender);
end;

procedure TForm1.EditCut(Sender: TObject);
begin
  RichEdit.CutToClipboard;
end;

procedure TForm1.EditCopy(Sender: TObject);
begin
  RichEdit.CopyToClipboard;
end;

procedure TForm1.EditPaste(Sender: TObject);
begin
  RichEdit.PasteFromClipboard;
end;

procedure TForm1.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not RichEdit.Modified then Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [CurrentFile]), mtConfirmation, mbYesNoCancel, 0);
  case SaveResp of
    idYes: Save1Click(Self);
    idNo: {Nothing};
    idCancel: Abort;
  end;
end;

procedure TForm1.ExitItemClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

{$ifdef MsWindows}
procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
      CheckFileSave;
      LoadFile(CFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;
{$endif}

procedure TForm1.ButtonClick(Sender: TObject);
var
  C: char;
begin
  case TButton(Sender).Tag of
    1: C := 'I';
    2: C := 'U';
    3: C := 'B';
  else
    exit;
  end;
  with RichEdit do
  begin
    SelText := '<'+C+'>'+SelText+'</'+C+'>';
    SetFocus;
  end;
end;

procedure TForm1.ViewerHotSpotClick(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
{HotspotClick handler}
var
  I: integer;
  ID: string;
begin
  I := Pos('IDEXPAND_', Uppercase(SRC));
  if I=1 then
  begin
    ID := Copy(SRC, 10, Length(SRC)-9);
    if Viewer.IDDisplay[ID+'Minus'] = High(TPropDisplay) then
      Viewer.IDDisplay[ID+'Minus'] := Low(TPropDisplay)
    else
      Viewer.IDDisplay[ID+'Minus'] := Succ(Viewer.IDDisplay[ID+'Minus']);
    Viewer.IDDisplay[ID+'Plus'] := Viewer.IDDisplay[ID+'Minus'];
//    Viewer.IDDisplay[ID+'Plus'] := not Viewer.IDDisplay[ID+'Plus'];
//    Viewer.IDDisplay[ID+'Minus'] := not Viewer.IDDisplay[ID+'Minus'];
    Viewer.Reformat;
    Handled := True;
    Exit;
  end;

  {allow only local links to work}
  if (Length(SRC) > 0) and (Trim(SRC)[1] = '#') then
    Handled := False
  else
    Handled := True;
end;

procedure TForm1.ViewerHotSpotCovered(Sender: TObject; const SRC: ThtString);
begin
  if SRC = '' then
    Panel3.Caption := ''
  else if Viewer.Target <> '' then
    Panel3.Caption := 'Target: '+Viewer.Target+'     URL: '+SRC
  else
    Panel3.Caption := 'URL: '+SRC
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  RichEdit.Height := Round((Panel.Height-Splitter.Height) * SizeRatio);
end;

procedure TForm1.SplitterMoved(Sender: TObject);
begin
  SizeRatio := RichEdit.Height / (Panel.Height-Splitter.Height);
end;

//-- BG ---------------------------------------------------------- 08.01.2011 --
procedure TForm1.LoadFile(Filename: String);
begin
{$ifdef LCL}
  SetCurrentDirUTF8(ExtractFilePath(Filename));
{$else}
  SetCurrentDir(ExtractFilePath(Filename));
{$endif}
  CurrentFile := Filename;
  Caption := CurrentFile;
  RichEdit.Lines.LoadFromFile(Filename);
  RichEdit.SetFocus;
  RichEdit.Modified := False;
end;

end.
