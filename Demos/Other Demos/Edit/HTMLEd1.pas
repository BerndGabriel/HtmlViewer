{
Version   11.9
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2018 by HtmlViewer Team

The purpose of this demo is to illustrate the usage of FindDisplayPos,
FindSourcePos, and DisplayPosToXY methods and SelStart, SelLength properties.
These properties and methods relate character positions in the HTML source
with those in the displayed docuement.

This demo is a simple two window editor.  Text entry and modifications are made
in the TRichEdit window but appear immediately in the ThtmlViewer window.
Positions marked in the THtmlViewer window are also hilighted in the TRichEdit
window.

Since display updates are accomplished by reloading the document, this scheme
obviously works best with a fast computer and simple documents.
}

unit HTMLEd1;

{$include ..\..\..\source\htmlcons.inc}

interface

uses
{$ifdef FPC}
  LCLVersion, LCLIntf, LCLType, LMessages, FileUtil, ShellAPI, Interfaces,
{$else}
  ShellAPI, Windows,
{$endif}
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls,
  HtmlGlobals, StyleTypes, Htmlview, StyleUn, HTMLUn2, HtmlAbt;

type
{$ifdef FPC}
  TRichEdit = TMemo;
{$endif}

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
    About1: TMenuItem;
    procedure ButtonClick(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure RichEdChange(Sender: TObject);
    procedure RichEditSelectionChange(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$ifdef UNICODE}
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
    procedure ViewerHotSpotCovered(Sender: TObject; const SRC: string);
{$else}
    procedure ViewerHotSpotClick(Sender: TObject; const SRC: WideString; var Handled: Boolean);
    procedure ViewerHotSpotCovered(Sender: TObject; const SRC: WideString);
{$endif}
    procedure FormResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    ViewerOK, RichOK: Boolean;
    SizeRatio: Double;
{$ifdef FPC}
    FSelStart, FSelLength: Integer;
{$endif}
    procedure CheckFileSave;
{$ifdef MsWindows}
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
{$endif}

  public
    CurrentFile: String;
    procedure LoadFile(Filename: String);
{$ifdef FPC}
    procedure UpdateActions; override;
{$endif}
  end;

var
  Form1: TForm1;

implementation

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

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
  Text: string;
begin
  if RichOK then
  begin
    Position := Viewer.Position;
{$ifdef Compiler16_Plus}
    //BG, 19.06.2015: VCL version specific values in RichEdit.SelStart and RichEdit.SelLength
    Text := StringReplace(RichEdit.Text, #$0D#$0A, #$0A, [rfReplaceAll]);
{$else}
    Text := RichEdit.Text;
{$endif}
{$ifdef LCL}
    Viewer.Text := UTF8Decode(Text);
{$else}
    Viewer.Text := Text;
{$endif}
    ViewerOK := True;
    Viewer.Position := Position;
    RichEditSelectionChange(Nil);
  end;
end;

procedure TForm1.RichEditSelectionChange(Sender: TObject);
{TRichEdit OnSelectionChange handler}
var
  Pos1, Pos2, X, Y, VPos, SStr, SLen: integer;
  Text: string;
  CharSize: Integer;
begin
   if ViewerOK then
  begin
    SStr := RichEdit.SelStart;
    SLen := RichEdit.SelLength;
    Text := RichEdit.Text;
    if SStr+SLen > Length(Text) then
       SLen := Length(Text)-SStr;
    CharSize := sizeof(ThtChar);
    Pos1 := Viewer.FindDisplayPos(SStr*CharSize, False);
    if Pos1 < 0 then  {means it's past end}
      Pos1 := Viewer.FindDisplayPos(SStr*CharSize, True);
    if SLen <> 0 then
    begin
      Pos2 := Viewer.FindDisplayPos((SStr+SLen)*CharSize, False);
      if Pos2 < 0 then
        Pos2 := Viewer.FindDisplayPos((SStr+SLen-1)*CharSize, False); {fix for above}
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
    Panel3.Caption := Format('Edit.SelStart %d, Edit.SelLength %d, Viewer.SelStart %d, Viewer.SelLength %d', [SStr, SLen, Pos1, Pos2-Pos1]);
  end;
end;

procedure TForm1.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pos, Pos2, VSelLength, VSelStart: integer;
begin
  if not ViewerOK then Exit;
  ViewerOK := False;
  try
    VSelStart  := Viewer.SelStart;
    VSelLength := Viewer.SelLength;
    if VSelLength < 0 then
    begin
      VSelLength := -VSelLength;
      Dec(VSelStart, VSelLength);
    end;

    Pos := Viewer.FindSourcePos(VSelStart);
    if Pos >= 0 then
    begin
      if VSelLength = 0 then
        Pos2 := Pos
      else
        Pos2 := Viewer.FindSourcePos(VSelStart + VSelLength);
      RichEdit.SelStart := Pos div 2;
      RichEdit.SelLength := (Pos2-Pos) div 2;
{$ifdef FPC}
      FSelStart := RichEdit.SelStart;
      FSelLength := RichEdit.SelLength;
{$endif}
      RichEdit.SetFocus;
{$ifdef MsWindows}
      PostMessage(RichEdit.handle, EM_SCROLLCARET, 0, 0);   {8.03}
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

//-- BG ---------------------------------------------------------- 06.01.2015 --
procedure TForm1.About1Click(Sender: TObject);
begin
  with TAboutBox.CreateIt(Self, 'HtmlEditor', 'THtmlViewer') do
  try
    ShowModal;
  finally
    Free;
  end;
end;

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
    if Viewer.IDDisplay[ID+'Minus'] = High(ThtDisplayStyle) then
      Viewer.IDDisplay[ID+'Minus'] := Low(ThtDisplayStyle)
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
{$if (lcl_fullversion > 0) and (lcl_fullversion < 1060400)}
  SetCurrentDirUTF8(ExtractFilePath(Filename));
{$else}
  SetCurrentDir(ExtractFilePath(Filename));
{$ifend}
  CurrentFile := Filename;
  Caption := CurrentFile;
  RichEdit.Lines.LoadFromFile(Filename);
  RichEdit.SetFocus;
  RichEdit.Modified := False;
end;

{$ifdef LCL}
//-- BG ---------------------------------------------------------- 06.01.2015 --
procedure TForm1.UpdateActions;
begin
  if (RichEdit.SelStart <> FSelStart) or (RichEdit.SelLength <> FSelLength) then
  begin
    FSelStart := RichEdit.SelStart;
    FSelLength := RichEdit.SelLength;
    RichEditSelectionChange(RichEdit);
  end;
end;
{$endif}

end.
