{
Version   11.2
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2010 by HtmlViewer Team
Copyright (c) 2011-2012 by Bernd Gabriel

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

unit FDemUnit;

{$include ..\..\source\htmlcons.inc}

{A program to demonstrate the TFrameViewer component}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Clipbrd, ComCtrls, StdCtrls, Fontdlg,
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs, FPImage, HtmlMisc, WideStringsLcl,
{$else}
  Windows, ShellAPI,
  {$if CompilerVersion >= 15}
    XpMan,
  {$ifend}
  {$ifdef Compiler18_Plus}
    WideStrings,
  {$else}
    TntWideStrings,
    TntClasses,
  {$endif}
{$endif}
{$ifndef MultiMediaMissing}
  MPlayer, MMSystem,
{$endif}
{$ifndef MetaFileMissing}
  MetaFilePrinter,
{$endif}
  PreviewForm,
{$ifdef UseTNT}
  TntForms,
  TntStdCtrls,
  SubmitTnt,
{$else UseTNT}
  Submit,
{$endif UseTNT}
  HtmlGlobals,
  HtmlBuffer,
  URLSubs,
  StyleUn,
  ReadHTML,
  HTMLSubs,
  HTMLUn2,
  Htmlview,
  FramView,
  DemoSubs,
  Htmlabt,
  PrintStatusForm,
  ImgForm;

const
  MaxHistories = 6;  {size of History list}

type
// Delphi 6 form editor fails with conditionals in form declaration:
{$ifdef UseTNT}
    TBaseForm = TTntForm;
{$else}
    TBaseForm = TForm;
{$endif}

  { TForm1 }

  TForm1 = class(TBaseForm)
    About1: TMenuItem;
    BackButton: TButton;
    CopyImagetoclipboard: TMenuItem;
    CopyItem: TMenuItem;
    Edit1: TMenuItem;
    Edit2: TEdit;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Find1: TMenuItem;
    FindDialog: TFindDialog;
    Fonts: TMenuItem;
    FrameViewer: TFrameViewer;
    FwdButton: TButton;
    HistoryMenuItem: TMenuItem;
    InfoPanel: TPanel;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Open: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenInNewWindow: TMenuItem;
    Options1: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    PopupMenu: TPopupMenu;
    Print1: TMenuItem;
    PrinterSetup: TMenuItem;
    PrintPreview: TMenuItem;
    ProgressBar: TProgressBar;
    ReloadButton: TButton;
    SelectAllItem: TMenuItem;
    SetPrintScale: TMenuItem;
    Showimages: TMenuItem;
    Timer1: TTimer;
    ViewImage: TMenuItem;
{$ifdef MsWindows}
  {$ifndef LCL}
    MediaPlayer: TMediaPlayer;
  {$endif}
{$endif}
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    procedure About1Click(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure CopyImagetoclipboardClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FontsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FrameViewerProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Integer);
    procedure FrameViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure FwdButtonClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure PrintPreviewClick(Sender: TObject);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure ReloadClick(Sender: TObject);
    procedure SelectAllItemClick(Sender: TObject);
    procedure SetPrintScaleClick(Sender: TObject);
    procedure ShowimagesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewImageClick(Sender: TObject);
{$ifdef UNICODE}
    procedure FrameViewerInclude(Sender: TObject; const Command: String; Params: TStrings; out IncludedDocument: TBuffer);
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: string);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: String);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: String; var Handled: Boolean);
    procedure SoundRequest(Sender: TObject; const SRC: String; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: String; Results: TStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: String);
{$else}
    procedure FrameViewerInclude(Sender: TObject; const Command: WideString; Params: TWideStrings; out IncludedDocument: TBuffer);
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: WideString);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure SoundRequest(Sender: TObject; const SRC: WideString; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: WideString; Results: TWideStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: WideString);
{$endif}
  private
    { Private declarations }
    Histories: array[0..MaxHistories-1] of TMenuItem;
    FoundObject: TImageObj;
    NewWindowFile: string;
{$ifdef MsWindows}
  {$ifdef LCL}
  {$else}
    MediaCount: integer;
    ThePlayer: TOBject;
  {$endif}
{$endif}
    TimerCount: integer;
    OldTitle: ThtString;
    HintWindow: ThtHintWindow;
    HintVisible: boolean;
    TitleViewer: THtmlViewer;
{$ifdef UseTNT}
    TntLabel: TTntLabel;
{$endif}
    procedure UpdateCaption;
    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure CloseAll;
  public
  end;

var
  Form1: TForm1;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$if CompilerVersion < 15}
    {$R manifest.res}
  {$ifend}
{$endif}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: integer;
begin
  Left := Left div 2;
  Top := Top div 2;
  Width := (Screen.Width * 8) div 10;
  Height := (Screen.Height * 6) div 8;


  FrameViewer.HistoryMaxCount := MaxHistories;  {defines size of history list}

  for I := 0 to MaxHistories-1 do
  begin      {create the MenuItems for the history list}
    Histories[I] := TMenuItem.Create(HistoryMenuItem);
    HistoryMenuItem.Insert(I, Histories[I]);
    with Histories[I] do
    begin
      OnClick := HistoryClick;
      Caption := 'XX';
      Tag := I;
    end;
  end;
{$ifdef LCL}
{$else}
  DragAcceptFiles(Handle, True);
{$endif}
  HintWindow := ThtHintWindow.Create(Self);
  HintWindow.Color := $CCFFFF;

{$ifdef UseTNT}
  TntLabel := TTntLabel.Create(Self);
  TntLabel.Align := alClient;
  TntLabel.Layout := tlCenter;
  TntLabel.Parent := InfoPanel;
{$endif}
  if sizeof(char) = 1 then
    Edit2.Text := 'Program uses single byte characters.'
  else
    Edit2.Text := 'Program uses unicode characters.';
  UpdateCaption;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
if (ParamCount >= 1) then
  begin            {Parameter is file to load}
  S := CmdLine;
  I := Pos('" ', S);
  if I > 0 then
    Delete(S, 1, I+1)  {delete EXE name in quotes}
  else Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
  I := Pos('"', S);
  while I > 0 do     {remove any quotes from parameter}
    begin
    Delete(S, I, 1);
    I := Pos('"', S);
    end;
  FrameViewer.LoadFromFile(HtmlToDos(Trim(S)));
  end
else if FileExists(ExtractFilePath(ParamStr(0))+'demo.htm') then
  FrameViewer.LoadFromFile(ExtractFilePath(ParamStr(0))+'demo.htm');
end;

procedure TForm1.OpenClick(Sender: TObject);
begin
  if FrameViewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(FrameViewer.CurrentFile)
  else
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
  begin
    FrameViewer.LoadFromFile(OpenDialog.Filename);
    UpdateCaption();
  end;
end;

procedure TForm1.HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: boolean);
{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
{$ifndef MultiMediaMissing}
const
  snd_Async = $0001;  { play asynchronously }
{$endif}
var
  PC: array[0..255] of {$ifdef UNICODE} WideChar {$else} AnsiChar {$endif};
  S, Params: ThtString;
  Ext: string;
  I, J, K: integer;
  Viewer: ThtmlViewer;
  ID: string;

begin
Handled := False;

{The following looks for a link of the form, "IDExpand_XXX".  This is interpreted
 as meaning a block with an ID="XXXPlus" or ID="XXXMinus" attribute should
 have its Display property toggled.
} 
I := Pos('IDEXPAND_', Uppercase(URL));
if I=1 then
  begin
  Viewer := FrameViewer.ActiveViewer;
  if Assigned(Viewer) then
    begin
    ID := Copy(URL, 10, Length(URL)-9);
    if Viewer.IDDisplay[ID+'Minus'] = High(TPropDisplay) then
      Viewer.IDDisplay[ID+'Minus'] := Low(TPropDisplay)
    else
      Viewer.IDDisplay[ID+'Minus'] := Succ(Viewer.IDDisplay[ID+'Minus']);
    Viewer.IDDisplay[ID+'Plus'] := Viewer.IDDisplay[ID+'Minus'];
    Viewer.Reformat;
    end;
  Handled := True;
  Exit;
  end;

{check for various file types}
I := Pos(':', URL);
J := Pos('FILE:', UpperCase(URL));
if (I <= 2) or (J > 0) then
  begin                      {apparently the URL is a filename}
  S := URL;
  K := Pos(' ', S);     {look for parameters}
  if K = 0 then K := Pos('?', S);  {could be '?x,y' , etc}
  if K > 0 then
    begin
    Params := Copy(S, K+1, 255); {save any parameters}
    setLength(S, K-1);            {truncate S}
    end
  else Params := '';
  S := (Sender as TFrameViewer).HTMLExpandFileName(S);
  Ext := Uppercase(ExtractFileExt(S));
  if Ext = '.WAV' then
    begin
    Handled := True;
{$ifndef MultiMediaMissing}
    sndPlaySound(StrPCopy(PC, S), snd_ASync);
{$endif}
    end
  else if Ext = '.EXE' then
    begin
    Handled := True;
    StartProcess(S + ' ' + Params, SW_SHOW);
    end
  else if (Ext = '.MID') or (Ext = '.AVI')  then
    begin
    Handled := True;
    StartProcess('MPlayer.exe /play /close ' + S, SW_SHOW);
    end;
  {else ignore other extensions}
  Edit2.Text := URL;
  Exit;
  end;
I := Pos('MAILTO:', UpperCase(URL));
J := Pos('HTTP://', UpperCase(URL));
if (I > 0) or (J > 0) then
  begin
  {Note: ShellExecute causes problems when run from Delphi 4 IDE}
{$ifdef LCL}
  OpenDocument(StrPCopy(PC, URL));
{$else}
  ShellExecute(Handle, nil, StrPCopy(PC, URL), nil, nil, SW_SHOWNORMAL);
{$endif}
  Handled := True;
  Exit;
  end;
Edit2.Text := URL;   {other protocall}
end;

procedure TForm1.HotSpotTargetCovered(Sender: TObject; const Target, URL: ThtString);
{mouse moved over or away from a hot spot.  Change the status line}
var
  Text: ThtString;
begin
  if URL = '' then
    Text := ''
  else if Target <> '' then
    Text := 'Target: '+Target+'  URL: '+URL
  else
    Text := 'URL: '+URL;

{$ifdef UseTNT}
  TntLabel.Caption := Text;
{$else}
  InfoPanel.Caption := Text;
{$endif}
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Find1Click(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TForm1.ReloadClick(Sender: TObject);
{the Reload button was clicked}
begin
with FrameViewer do
  begin
  ReloadButton.Enabled := False;
  Reload;   {load again}
  ReloadButton.Enabled := CurrentFile <> '';
  FrameViewer.SetFocus;
  end;
end;

procedure TForm1.CopyItemClick(Sender: TObject);
begin
FrameViewer.CopyToClipboard;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
with FrameViewer do
  begin
  CopyItem.Enabled := SelLength <> 0;
  SelectAllItem.Enabled := (ActiveViewer <> Nil) and (ActiveViewer.CurrentFile <> '');
  Find1.Enabled := SelectAllItem.Enabled;
  end;
end;

procedure TForm1.SelectAllItemClick(Sender: TObject);
begin
FrameViewer.SelectAll;
end;

procedure TForm1.FindDialogFind(Sender: TObject);
begin
with FindDialog do
  begin
  if not FrameViewer.FindEx(FindText, frMatchCase in Options, not (frDown in Options)) then
    MessageDlg('No further occurances of "'+FindText+'"', mtInformation, [mbOK], 0);
  end;
end;

procedure TForm1.ShowimagesClick(Sender: TObject);
begin
With FrameViewer do
  begin
  ViewImages := not ViewImages;
  ShowImages.Checked := ViewImages;
  end;
end;

procedure TForm1.HistoryChange(Sender: TObject);
{This event occurs when something changes history list}
var
  I: integer;
  Cap: string;
begin
  with FrameViewer do
  begin
    {check to see which buttons are to be enabled}
    FwdButton.Enabled := FwdButtonEnabled;
    BackButton.Enabled := BackButtonEnabled;

    {Enable and caption the appropriate history menuitems}
    HistoryMenuItem.Visible := History.Count > 0;
    for I := 0 to MaxHistories-1 do
      with Histories[I] do
        if I < History.Count then
        begin
          Cap := History.Strings[I];
          if TitleHistory[I] <> '' then
            Cap := Cap + '--' + TitleHistory[I];
          Caption := Cap;    {Cap limits string to 80 char}
          Visible := True;
          Checked := I = HistoryIndex;
        end
        else
          Histories[I].Visible := False;
    UpdateCaption();    {keep the caption updated}
    FrameViewer.SetFocus;
  end;
end;

procedure TForm1.HistoryClick(Sender: TObject);
{A history list menuitem got clicked on}
begin
  {Changing the HistoryIndex loads and positions the appropriate document}
  FrameViewer.HistoryIndex := (Sender as TMenuItem).Tag;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  with TAboutBox.CreateIt(Self, 'FrameDem', 'TFrameViewer') do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TForm1.FontsClick(Sender: TObject);
var
  FontForm: TFontForm;
begin
  FontForm := TFontForm.Create(Self);
  try
    with FontForm do
    begin
      FontName := FrameViewer.DefFontName;
      FontColor := FrameViewer.DefFontColor;
      FontSize := FrameViewer.DefFontSize;
      HotSpotColor := FrameViewer.DefHotSpotColor;
      Background := FrameViewer.DefBackground;
      if ShowModal = mrOK then
      begin
        FrameViewer.DefFontName := FontName;
        FrameViewer.DefFontColor := FontColor;
        FrameViewer.DefFontSize := FontSize;
        FrameViewer.DefHotSpotColor := HotSpotColor;
        FrameViewer.DefBackground := Background;
        ReloadClick(Self);    {reload to see how it looks}
      end;
    end;
  finally
    FontForm.Free;
  end;
end;

procedure TForm1.Print1Click(Sender: TObject);
var
  Viewer: THtmlViewer;
begin
  Viewer := FrameViewer.ActiveViewer;
  if Viewer <> nil then
    PrintWithDialog(Self, PrintDialog, Viewer);
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  Print1.Enabled := FrameViewer.ActiveViewer <> Nil;
  PrintPreview.Enabled := Print1.Enabled;
end;

procedure TForm1.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: ThtString; Results: ThtStringList);
begin
  with SubmitForm do
  begin
    ActionText.Text := AnAction;
    MethodText.Text := Method;
    ResultBox.Items.Text := Results.Text;
    Results.Free;
    Show;
  end;
end;

procedure TForm1.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
var
  Enabled: Boolean;
begin
  Enabled := not ProcessingOn;
  if ProcessingOn then
    CloseAll;    {in case hint window is open}
  FwdButton.Enabled := Enabled and FrameViewer.FwdButtonEnabled;
  BackButton.Enabled := Enabled and FrameViewer.BackButtonEnabled;
  ReloadButton.Enabled := Enabled and (FrameViewer.CurrentFile <> '');
  Print1.Enabled := Enabled and (FrameViewer.CurrentFile <> '') and (FrameViewer.ActiveViewer <> Nil);
  PrintPreview.Enabled := Print1.Enabled;
  Find1.Enabled := Print1.Enabled;
  SelectAllItem.Enabled := Print1.Enabled;
  Open.Enabled := Enabled;
end;

procedure TForm1.FwdButtonClick(Sender: TObject);
begin
FrameViewer.GoFwd;
end;

procedure TForm1.BackButtonClick(Sender: TObject);
begin
FrameViewer.GoBack;
end;

procedure TForm1.WindowRequest(Sender: TObject; const Target, URL: ThtString);
var
  S, Dest: string;
  I: integer;
  PC: array[0..1023] of char;
begin
S := URL;
I := Pos('#', S);
if I >= 1 then
  begin
  Dest := System.Copy(S, I, 255);  {local destination}
  S := System.Copy(S, 1, I-1);     {the file name}
  end
else
  Dest := '';    {no local destination}
S := FrameViewer.HTMLExpandFileName(S);
if FileExists(S) then
   StartProcess(StrPCopy(PC, '"' + ParamStr(0)+'" "'+S+Dest+'"'), sw_Show);
end;

procedure TForm1.wmDropFiles(var Message: TMessage);
{$ifdef LCL}
begin
{$else}
var
  S: string;
begin
  SetLength(S, 1024);
  SetLength(S, DragQueryFile(Message.WParam, 0, @S[1], 1024));
  DragFinish(Message.WParam);
  if Length(S) > 0 then
    FrameViewer.LoadFromFile(S);
{$endif}
  Message.Result := 0;
end;

procedure TForm1.CopyImagetoclipboardClick(Sender: TObject);
begin
Clipboard.Assign(FoundObject.Bitmap);
end;

procedure TForm1.ViewImageClick(Sender: TObject);
var
  AForm: TImageForm;
begin
AForm := TImageForm.Create(Self);
with AForm do
  begin
  Bitmap := FoundObject.Bitmap;
  Caption := '';
  Show;
  end;
end;

procedure TForm1.MediaPlayerNotify(Sender: TObject);
begin
{$ifndef MultiMediaMissing}
  try
    With MediaPlayer do
      if NotifyValue = nvSuccessful then
        begin
        if MediaCount > 0 then
          begin
          Play;
          Dec(MediaCount);
          end
        else
          Begin
          Close;
          ThePlayer := Nil;
          end;
        end;
  except
  end;
{$endif}
end;

procedure TForm1.SoundRequest(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: Boolean);
begin
{$ifndef MultiMediaMissing}
  try
    with MediaPlayer do
      if Terminate then
        begin
        if (Sender = ThePlayer) then
          begin
          Close;
          ThePlayer := Nil;
          end;
        end
      else if ThePlayer = Nil then
        begin
        if Sender is THtmlViewer then
          Filename := ThtmlViewer(Sender).HTMLExpandFilename(SRC)
        else Filename := (Sender as TFrameViewer).HTMLExpandFilename(SRC);
        Notify := True;
        Open;
        ThePlayer := Sender;
        if Loop < 0 then MediaCount := 9999
          else if Loop = 0 then MediaCount := 1
          else MediaCount := Loop;
        end;
  except
  end;
{$endif}
end;

procedure TForm1.FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: ThtString);
var
  S: ThtString;
  CB: TCheckBoxFormControlObj absolute Obj;
  RB: TRadioButtonFormControlObj absolute Obj;
begin
  if OnClick = 'display' then
  begin
    if Obj is TCheckBoxFormControlObj then
    begin
      S := CB.Value + ' is ';
      if CB.Checked then
        S := S + 'checked'
      else
        S := S + 'unchecked';
      MessageDlg(S, mtCustom, [mbOK], 0);
    end
    else if Obj is TRadioButtonFormControlObj then
    begin
      S := RB.Value + ' is checked';
      MessageDlg(S, mtCustom, [mbOK], 0);
    end;
  end
  else if OnClick <> '' then
    with TAboutBox.CreateIt(Self, OnClick) do
      try
        ShowModal;
      finally
        Free;
      end;
    //MessageDlg(OnClick, mtCustom, [mbOK], 0);
end;

procedure TForm1.FrameViewerInclude(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer);
{OnInclude handler}
var
  Filename: ThtString;
  I: integer;
  Stream: TFileStream;
begin
  if CompareText(Command, 'Date') = 0 then
    IncludedDocument := TBuffer.Create(DateToStr(Date)) { <!--#date --> }
  else if CompareText(Command, 'Time') = 0 then
    IncludedDocument := TBuffer.Create(TimeToStr(Time))   { <!--#time -->  }
  else if CompareText(Command, 'Include') = 0 then
  begin   {an include file <!--#include FILE="filename" -->  }
    if (Params.count >= 1) then
    begin
      I := Pos('file=', Lowercase(Params[0]));
      if I > 0 then
      begin
        Filename := copy(Params[0], 6, Length(Params[0])-5);
        try
          if FileExists(Filename) then
          begin
            Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
            IncludedDocument := TBuffer.Create(Stream);
          end
        except
        end;
      end;
    end;
  end;
end;

procedure TForm1.FrameViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  Pt: TPoint;
  S, Dest: string;
  I: integer;
  Viewer: ThtmlViewer;
  HintWindow: ThtHintWindow;
  ARect: TRect;
begin
Viewer := Sender as ThtmlViewer;
with Parameters do
  begin
  FoundObject := Image;
  ViewImage.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);
  CopyImageToClipboard.Enabled := (FoundObject <> Nil) and (FoundObject.Bitmap <> Nil);

  if URL <> '' then
    begin
    S := URL;
    I := Pos('#', S);
    if I >= 1 then
      begin
      Dest := System.Copy(S, I, 255);  {local destination}
      S := System.Copy(S, 1, I-1);     {the file name}
      end
    else
      Dest := '';    {no local destination}
    if S = '' then S := Viewer.CurrentFile
      else S := Viewer.HTMLExpandFileName(S);
    NewWindowFile := S+Dest;
    OpenInNewWindow.Enabled := FileExists(S);
    end
  else OpenInNewWindow.Enabled := False;

  GetCursorPos(Pt);
  if Length(CLickWord) > 0 then
    begin
    HintWindow := ThtHintWindow.Create(Self);   
    try
      ARect := Rect(0,0,0,0);
      DrawTextW(HintWindow.Canvas.Handle, @ClickWord[1], Length(ClickWord), ARect, DT_CALCRECT);
      with ARect do
        HintWindow.ActivateHint(Rect(Pt.X+20, Pt.Y-(Bottom-Top)-15, Pt.x+30+Right, Pt.Y-15), ClickWord);
      PopupMenu.Popup(Pt.X, Pt.Y);
    finally
      HintWindow.Free;
      end;
    end
  else PopupMenu.Popup(Pt.X, Pt.Y);
  end;
end;
  
procedure TForm1.OpenInNewWindowClick(Sender: TObject);
var
  PC: array[0..255] of char;
begin
  StartProcess(StrPCopy(PC, ParamStr(0)+' "'+NewWindowFile+'"'), sw_Show);
end;

procedure TForm1.PrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TForm1.PrintPreviewClick(Sender: TObject);
var
  pf: TPreviewForm;
  Viewer: ThtmlViewer;
  Abort: boolean;
begin
  Viewer := FrameViewer.ActiveViewer;
  if Viewer <> nil then
  begin
    pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
    try
      if not Abort then
        pf.ShowModal;
    finally
      pf.Free;
    end;
  end;
end;

procedure TForm1.FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
if not Timer1.Enabled and Assigned(ActiveControl) and ActiveControl.Focused
         and (Sender is ThtmlViewer) then  
  begin
  TitleViewer := ThtmlViewer(Sender);
  TitleStr := TitleViewer.TitleAttr;
  if TitleStr = '' then
    OldTitle := ''
  else if TitleStr <> OldTitle then
    begin
    TimerCount := 0;
    Timer1.Enabled := True;
    OldTitle := TitleStr;
    end;
  end;
end;

procedure TForm1.CloseAll;
begin
Timer1.Enabled := False;
HintWindow.ReleaseHandle;
HintVisible := False;
TitleViewer := Nil;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  StartCount = 2; {timer counts before hint window opens}
  EndCount = 20;  {after this many timer counts, hint window closes}
var
  Pt, Pt1: TPoint;
  ARect: TRect;
  TitleStr: ThtString;
begin
  if not Assigned(TitleViewer) then
  begin
    CloseAll;
    Exit;
  end;
  Inc(TimerCount);
  GetCursorPos(Pt);
  try       {in case TitleViewer becomes corrupted}
    Pt1 := TitleViewer.ScreenToClient(Pt);
    TitleStr := TitleViewer.TitleAttr;
    if (TitleStr = '') or not PtInRect(TitleViewer.ClientRect, Pt1)then
    begin
      OldTitle := '';
      CloseAll;
      Exit;
    end;
    if TitleStr <> OldTitle then
    begin
      TimerCount := 0;
      OldTitle := TitleStr;
      HintWindow.ReleaseHandle;
      HintVisible := False;
      Exit;
    end;

    if TimerCount > EndCount then
      CloseAll
    else if (TimerCount >= StartCount) and not HintVisible and (HintWindow <> nil) then
    begin
      ARect := HintWindow.CalcHintRect(300, {$ifdef LCL}Utf8Encode{$endif}(TitleStr), Nil);
      HintWindow.ActivateHint(Rect(Pt.X, Pt.Y + 18, Pt.X + ARect.Right, Pt.Y + 18 + ARect.Bottom), {$ifdef LCL}Utf8Encode{$endif}(TitleStr));
      HintVisible := True;
    end;
except
  CloseAll;
  end;
end;

procedure TForm1.FrameViewerProgress(Sender: TObject;
  Stage: TProgressStage; PercentDone: Integer);
begin
  ProgressBar.Position := PercentDone;
  case Stage of
    psStarting:
      ProgressBar.Visible := True;
    psRunning:;
    psEnding:
      ProgressBar.Visible := False;
  end;
  ProgressBar.Update;
end;

procedure TForm1.SetPrintScaleClick(Sender: TObject);
var
  S: string;
begin
  S := FloatToStr(FrameViewer.PrintScale);
  try
    if InputQuery('PrintScale', 'Enter desired print scale value', S) then
      FrameViewer.PrintScale := StrToFloat(S);
  except
  end;
end;

{HTML for print header and footer}
const
  HFText: ThtString =
    '<html><head><style>'+
    'body  {font: Arial 8pt;}'+
    '</style></head>'+
    '<body marginwidth="0">'+
    '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
    '<tr>'+
    '<td>#left</td><td align="right">#right</td>'+
    '</tr>'+
    '</table></body></html>';

function ReplaceStr(Const S, FromStr, ToStr: ThtString): string;
{replace FromStr with ToStr in string S.
 for Delphi 6, 7, AnsiReplaceStr may be used instead.}
var
  I: integer;
begin
  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Result := S;
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end;
end;

procedure TForm1.ViewerPrintHTMLHeader(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', FrameViewer.DocumentTitle);
  S := ReplaceStr(S, '#right', FrameViewer.CurrentFile);
  HFViewer.LoadFromString(S);
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', DateToStr(Date));
  S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
  HFViewer.LoadFromString(S);
end;

procedure TForm1.UpdateCaption;
var
  Viewer: TFrameViewer;
  Title, Cap: ThtString;
begin
  Viewer := FrameViewer;
  if Viewer.DocumentTitle <> '' then
    Title := Viewer.DocumentTitle
  else if Viewer.URL <> '' then
    Title := Viewer.URL
  else if Viewer.CurrentFile <> '' then
    Title := Viewer.CurrentFile
  else
    Title := '';

  Cap := 'FrameViewer ' + VersionNo + ' Demo';
  if Title <> '' then
    Cap := Cap + ' - ' + Title;
{$ifdef LCL}
  Caption := UTF8Encode(Cap);
{$else}
  Caption := Cap;
{$endif}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseAll;
end;

end.
