{
Version   11.10
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2023 by HtmlViewer Team

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
  ExtCtrls, Menus, Clipbrd, ComCtrls, StdCtrls, Fontdlg, Math,
{$ifdef LCL}
  LclIntf, LclType, LMessages, PrintersDlgs, FPImage, HtmlMisc,
{$else}
  Windows, ShellAPI, MPlayer,
  {$if CompilerVersion >= 15}
    {$ifndef UseVCLStyles}
    XpMan,
    {$endif}
  {$ifend}
  {$ifdef Compiler18_Plus}
    WideStrings,
  {$else}
    TntWideStrings,
    TntClasses,
  {$endif}
{$endif}
{$ifndef MultiMediaMissing}
  MMSystem,
{$endif}
{$ifndef MetaFileMissing}
  MetaFilePrinter,
{$endif}
{$ifdef UseOldPreviewForm}
  PreviewForm,
{$else UseOldPreviewForm}
  BegaZoom,
  BegaHtmlPrintPreviewForm,
{$endif UseOldPreviewForm}
{$ifdef UseTNT}
  TntForms,
  TntStdCtrls,
  SubmitTnt,
{$else UseTNT}
  Submit,
{$endif UseTNT}
  HtmlDemoUtils,
  HtmlGlobals,
  HtmlBuffer,
  URLSubs,
  StyleUn,
  StyleTypes,
  ReadHTML,
  HTMLSubs,
  Htmlsbs1,
  HTMLUn2,
  Htmlview,
  FramView,
  Htmlabt,
  PrintStatusForm,
  ImgForm;

const
  MaxHistories = 6;  {size of History list}
  wm_ShowParam = WM_USER + 100;


type
// Delphi 6 form editor fails with conditionals in form declaration:
{$ifdef UseTNT}
    TBaseForm = TTntForm;
{$else}
    TBaseForm = class(TForm);
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
    mmiParentColor: TMenuItem;
    mmiParentFont: TMenuItem;
    mmiDefaultFont: TMenuItem;
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
{$ifdef VCL}
    MediaPlayer: TMediaPlayer;
{$endif}
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    mmiQuirksMode: TMenuItem;
    mmiQuirksModeDetect: TMenuItem;
    mmiQuirksModeStandards: TMenuItem;
    mmiQuirksModeQuirks: TMenuItem;
    QuirksModePanel: TPanel;
    procedure About1Click(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure CopyImagetoClipboardClick(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure mmiDefaultFontClick(Sender: TObject);
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
  {$ifdef LCL}
    procedure FrameViewerInclude(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer);
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: ThtString);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: ThtString);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: Boolean);
    procedure SoundRequest(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: ThtString; Results: ThtStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: ThtString);
  {$else}
    procedure FrameViewerInclude(Sender: TObject; const Command: WideString; Params: TWideStrings; out IncludedDocument: TBuffer);
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: WideString);
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure SoundRequest(Sender: TObject; const SRC: WideString; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: WideString; Results: TWideStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: WideString);
  {$endif}
{$endif}
    procedure mmiQuirksModeDetectClick(Sender: TObject);
    procedure mmiQuirksModeStandardsClick(Sender: TObject);
    procedure mmiQuirksModeQuirksClick(Sender: TObject);
    procedure mmiParentFontClick(Sender: TObject);
    procedure mmiParentColorClick(Sender: TObject);
  private
    { Private declarations }
    Histories: array[0..MaxHistories-1] of TMenuItem;
    FoundObject: TImageObj;
    NewWindowFile: ThtString;
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
    procedure wmShowParam(var Message: TMessage); message wm_ShowParam;
    procedure CloseAll;
{$ifdef LCL}
{$else}
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
{$endif}
{$if defined(LCL) or defined(Compiler32_Plus)}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
{$ifend}
  protected
    procedure UpdateActions; override;
  public
  end;

var
  Form1: TForm1;

implementation

{$ifdef HasSystemUITypes}
uses
  System.Types,
  System.UITypes;
{$endif}
{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$if CompilerVersion < 15}
    {$R manifest.res}
  {$ifend}
{$endif}

{$if defined(LCL) or defined(Compiler32_Plus)}
procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
var
  Info: string;
begin
  if sizeof(char) = 1 then
    Info := 'Program uses single byte characters.'
  else
    Info := 'Program uses unicode characters.';
  Info := Info + ' New Form.PixelsPerInch = ' + IntToStr(PixelsPerInch);
  Info := Info + ', Monitor.PixelsPerInch = ' + IntToStr(Monitor.PixelsPerInch);
  Edit2.Text := Info;
end;
{$ifend}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  Info: string;
begin
  BoundsRect := htGetNiceFormSize( Monitor, PixelsPerInch );

  FrameViewer.HistoryMaxCount := MaxHistories;  {defines size of history list}
{$ifdef HasGestures}
  FrameViewer.Touch.InteractiveGestureOptions := [{igoPanSingleFingerHorizontal,} igoPanSingleFingerVertical, igoPanInertia];
  FrameViewer.Touch.InteractiveGestures := [igPan];
{$endif}

{$ifdef Compiler32_Plus}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
{$endif}

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
  HintWindow := ThtHintWindow.Create(Self);
  HintWindow.Color := $CCFFFF;

{$ifdef UseTNT}
  TntLabel := TTntLabel.Create(Self);
  TntLabel.Align := alClient;
  TntLabel.Layout := tlCenter;
  TntLabel.Parent := InfoPanel;
{$endif}
  if sizeof(char) = 1 then
    Info := 'Program uses single byte characters.'
  else
    Info := 'Program uses unicode characters.';
{$if defined(LCL) or defined(Compiler22_Plus)}
  Info := Info + ' Form.PixelsPerInch = ' + IntToStr(PixelsPerInch);
  Info := Info + ', Monitor.PixelsPerInch = ' + IntToStr(Monitor.PixelsPerInch);
  Edit2.Text := Info;
{$ifend}
  UpdateCaption;

{$ifdef LCL}
{$else}
  DragAcceptFiles(Handle, True);
  Application.OnMessage := AppMessage;
{$endif}
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PostMessage(Handle, wm_ShowParam, 0, 0);
end;

procedure TForm1.wmShowParam(var Message: TMessage);
var
  S: ThtString;
  I: integer;
begin
  if (ParamCount >= 1) then
  begin            {Parameter is file to load}
    S := CmdLine;
    I := Pos('" ', S);
    if I > 0 then
      Delete(S, 1, I+1)  {delete EXE name in quotes}
    else
      Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
    I := Pos('"', S);
    while I > 0 do     {remove any quotes from parameter}
    begin
      Delete(S, I, 1);
      I := Pos('"', S);
    end;
    S := HtmlToDos(Trim(S));
    FrameViewer.LoadFromFile(S);
  end
  else
  begin
    S := ExtractFilePath(ParamStr(0))+'demo.htm';
    if FileExists(S) then
      FrameViewer.LoadFromFile(S);
  end;
end;

procedure TForm1.OpenClick(Sender: TObject);
begin
  if (FrameViewer.CurrentFile <> '') and (Copy(FrameViewer.CurrentFile, 1, 9) <> 'source://') then
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

procedure TForm1.HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: Boolean);
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
  uURL, S, Params, Ext: ThtString;
  I, J, K: integer;
begin
  Handled := False;

  {check for various file types}
  uURL := htUpperCase(URL);
  I := Pos(':', uURL);
  J := Pos('FILE:', uURL);
  if (I <= 2) or (J > 0) then
  begin                      {apparently the URL is a filename}
    S := URL;
    K := Pos('?', S);  {look for parameters, could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K+1, 255); {save any parameters}
      setLength(S, K-1);            {truncate S}
    end
    else
      Params := '';
    S := (Sender as TFrameViewer).HTMLExpandFileName(S);
    Ext := htUpperCase(ExtractFileExt(S));
    if Ext = '.WAV' then
    begin
      Handled := True;
{$ifndef MultiMediaMissing}
      sndPlaySound(StrPCopy(PC, S), snd_ASync);
{$endif}
    end
    else if Ext = '.EXE' then
      Handled := StartProcess(S, Params)
    else if (Ext = '.MID') or (Ext = '.AVI') then
      Handled := OpenDocument(S);
    {else ignore other extensions}
  end
  else
  begin
    I := Pos('MAILTO:', uURL) + Pos('HTTP://', uURL) + Pos('HTTPS://', uURL);
    if I > 0 then
      Handled := OpenDocument(URL);
  end;

  Edit2.Text := URL;   {other protocol}
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
  ReloadButton.Enabled := False;
  FrameViewer.Reload;   {load again}
  FrameViewer.SetFocus;
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
  Cap: ThtString;
begin
  with FrameViewer do
  begin
    {check to see which buttons are to be enabled}
    FwdButton.Enabled := FwdButtonEnabled;
    BackButton.Enabled := BackButtonEnabled;

    {Enable and caption the appropriate history menuitems}
    HistoryMenuItem.Visible := History.Count > 0;
    for I := 0 to MaxHistories-1 do
      if Histories[I] <> nil then
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

procedure TForm1.mmiDefaultFontClick(Sender: TObject);
var
  FontForm: TFontForm;
begin
  FontForm := TFontForm.Create(Self);
  try
    with FontForm do
    begin
      FontName := ReadFontName(FrameViewer.DefFontName);
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
        FrameViewer.ParentFont := False;
        ReloadClick(Self);    {reload to see how it looks}
      end;
    end;
  finally
    FontForm.Free;
  end;
end;

//-- BG ------------------------------------------------------- 10.12.2022 --
procedure TForm1.mmiParentColorClick(Sender: TObject);
begin
  FrameViewer.ParentColor := not FrameViewer.ParentColor;
end;

//-- BG ------------------------------------------------------- 10.12.2022 --
procedure TForm1.mmiParentFontClick(Sender: TObject);
begin
  FrameViewer.ParentFont := not FrameViewer.ParentFont;
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
  S, Dest: ThtString;
begin
  SplitDest(Url, S, Dest);
  S := FrameViewer.HTMLExpandFileName(S);
  if FileExists(S) then
    try
      StartProcess(ParamStr(0), '"'+S+Dest+'"');
    except
      on e: Exception do
        ShowMessage('failed to open "'+S+Dest+'"'#13#13'Cause: ' + e.Message );
    end;
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

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // Dropping File in LCL
  FrameViewer.LoadFromFile(FileNames[0]);
end;

procedure TForm1.CopyImagetoClipboardClick(Sender: TObject);
begin
  Clipboard.Assign(FoundObject.Graphic);
end;

procedure TForm1.ViewImageClick(Sender: TObject);
var
  AForm: TImageForm;
begin
  AForm := TImageForm.Create(Self);
  with AForm do
  begin
    Image := FoundObject.Image;
    Caption := '';
    Show;
  end;
end;

procedure TForm1.MediaPlayerNotify(Sender: TObject);
begin
{$ifndef LCL}
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
{$ifndef LCL}
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
end;

procedure TForm1.FrameViewerInclude(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer);
{OnInclude handler}
var
  Filename: ThtString;
  I: integer;
  Stream: TFileStream;
begin
  if htCompareText(Command, 'Date') = 0 then
    IncludedDocument := TBuffer.Create(DateToStr(Date)) { <!--#date --> }
  else if htCompareText(Command, 'Time') = 0 then
    IncludedDocument := TBuffer.Create(TimeToStr(Time))   { <!--#time -->  }
  else if htCompareText(Command, 'Include') = 0 then
  begin   {an include file <!--#include FILE="filename" -->  }
    if (Params.count >= 1) then
    begin
      I := Pos('file=', htLowerCase(Params[0]));
      if I > 0 then
      begin
        Filename := Copy(Params[0], 6, Length(Params[0])-5);
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
  S, Dest: ThtString;
  Viewer: ThtmlViewer;
  HintWindow: ThtHintWindow;
  ARect: TRect;
begin
  Viewer := Sender as ThtmlViewer;
  with Parameters do
  begin
    FoundObject := Image;
    ViewImage.Enabled := (FoundObject <> Nil) and (FoundObject.Image <> Nil);
    CopyImageToClipboard.Enabled := (FoundObject <> Nil) and
      (FoundObject.Graphic <> Nil);

    if URL <> '' then
    begin
      SplitDest(URL, S, Dest);
      if Length(S) = 0 then
        S := Viewer.CurrentFile
      else
        S := Viewer.HTMLExpandFileName(S);
      NewWindowFile := S + Dest;
      OpenInNewWindow.Enabled := FileExists(S);
    end
    else
      OpenInNewWindow.Enabled := False;

    GetCursorPos(Pt);
    if Length(CLickWord) > 0 then
    begin
      HintWindow := ThtHintWindow.Create(Self);
      try
        ARect := Rect(0, 0, 0, 0);
        DrawTextW(HintWindow.Canvas.Handle, @CLickWord[1], Length(CLickWord), ARect, DT_CALCRECT);
        with ARect do
          HintWindow.ActivateHint(Rect(Pt.X + 20, Pt.Y - (Bottom - Top) - 15, Pt.X + 30 + Right, Pt.Y - 15), CLickWord);
        PopupMenu.Popup(Pt.X, Pt.Y);
      finally
        HintWindow.Free;
      end;
    end
    else
      PopupMenu.Popup(Pt.X, Pt.Y);
  end;
end;

procedure TForm1.OpenInNewWindowClick(Sender: TObject);
begin
  StartProcess(ParamStr(0), '"' + NewWindowFile + '"');
end;

procedure TForm1.PrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
end;

procedure TForm1.PrintPreviewClick(Sender: TObject);
{$ifndef NoMetaFile}
var
{$ifdef UseOldPreviewForm}
  pf: TPreviewForm;
{$else UseOldPreviewForm}
  pf: TBegaHtmlPrintPreviewForm;
{$endif UseOldPreviewForm}
  Viewer: ThtmlViewer;
  Abort: boolean;
begin
  Viewer := FrameViewer.ActiveViewer;
  if Viewer <> nil then
  begin
{$ifdef UseOldPreviewForm}
    pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
{$else UseOldPreviewForm}
    pf := TBegaHtmlPrintPreviewForm.Create(Self);
    pf.FrameViewer := FrameViewer;
    Abort := False;
{$endif UseOldPreviewForm}
    try
      if not Abort then
        pf.ShowModal;
    finally
      pf.Free;
    end;
  end;
{$else !NoMetaFile}
begin
{$endif !NoMetaFile}
end;

procedure TForm1.FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: ThtString;
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
  if HintWindow <> nil then
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
    'body, td  {font: sans-serif 8pt;}'+
    '</style></head>'+
    '<body marginwidth="0">'+
    '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
    '<tr>'+
    '<td>#left</td><td align="right">#right</td>'+
    '</tr>'+
    '</table></body></html>';

function ReplaceStr(const S, FromStr, ToStr: ThtString): ThtString;
{replace FromStr with ToStr in string S.
 for Delphi 6, 7, AnsiReplaceStr may be used instead.}
var
  I: integer;
begin
  Result := S;
  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end;
end;

procedure TForm1.ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
  NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
  var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', FrameViewer.DocumentTitle);
  S := ReplaceStr(S, '#right', FrameViewer.CurrentFile);
  HFViewer.Text := S;
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
  NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
  var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', DateToStr(Date));
  S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
  HFViewer.Text := S;
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
  else if (Viewer.CurrentFile <> '') and (Copy(Viewer.CurrentFile, 1, 9) <> 'source://') then
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

//-- BG ---------------------------------------------------------- 25.10.2012 --
procedure TForm1.mmiQuirksModeDetectClick(Sender: TObject);
begin
  FrameViewer.QuirksMode := qmDetect;
  ReloadClick(nil);
end;

//-- BG ---------------------------------------------------------- 25.10.2012 --
procedure TForm1.mmiQuirksModeStandardsClick(Sender: TObject);
begin
  FrameViewer.QuirksMode := qmStandards;
  ReloadClick(nil);
end;

//-- BG ---------------------------------------------------------- 25.10.2012 --
procedure TForm1.mmiQuirksModeQuirksClick(Sender: TObject);
begin
  FrameViewer.QuirksMode := qmQuirks;
  ReloadClick(nil);
end;

//-- BG ---------------------------------------------------------- 25.10.2012 --
procedure TForm1.UpdateActions;
var
  Viewer: TViewerBase;
  QuirksModeMenuItem: TMenuItem;
  IsDetectedQuirksMode: Boolean;
  QuirksModePanelCaption: string;
begin
  ReloadButton.Enabled := FrameViewer.CurrentFile <> '';

  // update parent mode menu items
  mmiParentFont.Checked := FrameViewer.ParentFont;
  mmiParentColor.Checked := FrameViewer.ParentColor;

  // update quirks mode panel and quirks mode menu items

  case FrameViewer.QuirksMode of
    qmStandards: QuirksModeMenuItem := mmiQuirksModeStandards;
    qmQuirks: QuirksModeMenuItem := mmiQuirksModeQuirks;
  else
    //qmDetect:
    QuirksModeMenuItem := mmiQuirksModeDetect;
  end;
  QuirksModeMenuItem.Checked := True;

  IsDetectedQuirksMode := False;
  Viewer := FrameViewer.ActiveViewer;
  if Viewer = nil then
  begin
    Viewer := FrameViewer;
    QuirksModePanelCaption := QuirksModeMenuItem.Caption;
    case Viewer.QuirksMode of
      qmStandards:  QuirksModePanel.Color := clBtnFace;
      qmQuirks:     QuirksModePanel.Color := clYellow;
    else
      //qmDetect:
      QuirksModePanel.Color := clLime;
    end;
  end
  else
  begin
    if Viewer.UseQuirksMode then
    begin
      QuirksModePanelCaption := mmiQuirksModeQuirks.Caption;
      QuirksModePanel.Color := clYellow;
    end
    else
    begin
      QuirksModePanelCaption := mmiQuirksModeStandards.Caption;
      QuirksModePanel.Color := clBtnFace;
    end;
    IsDetectedQuirksMode := Viewer.QuirksMode = qmDetect;
  end;

  QuirksModePanelCaption := StringReplace(QuirksModePanelCaption, '&', '', []);
  QuirksModePanel.Caption := QuirksModePanelCaption;
  if IsDetectedQuirksMode then
  begin
    QuirksModePanel.Font.Style := QuirksModePanel.Font.Style + [fsItalic];
    QuirksModePanel.Hint := QuirksModePanelCaption + ' as detected by QuirksMode ''Detect'' selected in Options menu';
  end
  else
  begin
    QuirksModePanel.Font.Style := QuirksModePanel.Font.Style - [fsItalic];
    QuirksModePanel.Hint := 'QuirksMode ''' + QuirksModePanelCaption + ''' as selected in Options menu';
  end;
end;

{$ifdef LCL}
{$else}
//-- BG ---------------------------------------------------------- 16.08.2015 --
procedure TForm1.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  WinCtrl: TWinControl;
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    WinCtrl := FindVCLWindow(Point(Word(Msg.lParam), HiWord(Msg.lParam)));
    if (WinCtrl is TPaintPanel) {$ifndef UseOldPreviewForm} or (WinCtrl is TBegaZoomBox) {$endif UseOldPreviewForm} then
    begin
      // perform mouse wheel scrolling for the control under the mouse:
      WinCtrl.Perform(CM_MOUSEWHEEL, Msg.WParam, Msg.LParam);
      Handled := True;
    end;
  end;
end;
{$endif}
end.
