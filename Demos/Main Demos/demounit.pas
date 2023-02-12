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

unit DemoUnit;

{$include ..\..\source\htmlcons.inc}

{ A program to demonstrate the THtmlViewer component }

interface

uses
{$ifdef HasSystemUITypes}
  System.Types,
  System.UITypes,
{$endif}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Clipbrd, ComCtrls, StdCtrls, Fontdlg, Math,
{$ifdef LCL}
  Types,
  LclIntf, LclType, PrintersDlgs, FPImage, HtmlMisc,
{$else}
  Windows, ShellAPI, MPlayer,
  {$IF CompilerVersion >= 15}
    {$IFNDEF TScrollStyleInSystemUITypes}
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
  HtmlImages,
  HTMLSubs,
  Htmlsbs1,
  Htmlabt,
  PrintStatusForm,
  ImgForm,
  HTMLUn2,
  UrlSubs,
  StyleUn,
  HtmlView;

const
  MaxHistories = 6; { size of History list }

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
    CopyImageToClipboard: TMenuItem;
    CopyItem: TMenuItem;
    Edit1: TEdit;
    Edit2: TMenuItem;
    Exit1: TMenuItem;
    File1: TMenuItem;
    Find1: TMenuItem;
    FindDialog: TFindDialog;
    Fonts: TMenuItem;
    FwdButton: TButton;
    HistoryMenuItem: TMenuItem;
    MainMenu: TMainMenu;
    MetaTimer: TTimer;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Open: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenImageFile: TMenuItem;
    OpenInNewWindow: TMenuItem;
    OpenTextFile: TMenuItem;
    options1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu: TPopupMenu;
    Print1: TMenuItem;
    PrinterSetup: TMenuItem;
    Printpreview: TMenuItem;
    ProgressBar: TProgressBar;
    ReloadButton: TButton;
    RepaintButton: TButton;
    SelectAllItem: TMenuItem;
    ShowImages: TMenuItem;
    Timer1: TTimer;
    Viewer: THTMLViewer;
    Viewimage: TMenuItem;
{$ifdef MsWindows}
{$ifndef LCL}
    MediaPlayer: TMediaPlayer;
{$endif}
{$endif}
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
    procedure About1Click(Sender: TObject);
    procedure CopyImageToClipboardClick(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FontsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure FwdBackClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure MetaTimerTimer(Sender: TObject);
    procedure OpenFileClick(Sender: TObject);
    procedure OpenImageFileClick(Sender: TObject);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure OpenTextFileClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure PrintPreviewClick(Sender: TObject);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure ReloadButtonClick(Sender: TObject);
    procedure RepaintButtonClick(Sender: TObject);
    procedure RightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure SelectAllItemClick(Sender: TObject);
    procedure ShowImagesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer; NumPage: Integer; LastPage: Boolean; var XL, XR: Integer; var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer; NumPage: Integer; LastPage: Boolean; var XL, XR: Integer; var StopPrinting: Boolean);
    procedure ViewerProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Integer);
    procedure ViewimageClick(Sender: TObject);
{$ifdef LCL}
    procedure HotSpotChange(Sender: TObject; const URL: ThtString);
    procedure HotSpotClick(Sender: TObject; const URL: ThtString; var Handled: Boolean);
    procedure MetaRefreshEvent(Sender: TObject; Delay: Integer; const URL: ThtString);
    procedure ObjectClick(Sender, Obj: TObject; const OnClick: ThtString);
    procedure SoundRequest(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; Const AnAction, Target, EncType, Method: ThtString; Results: ThtStringList);
    procedure ViewerInclude(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer);
    procedure ViewerScript(Sender: TObject; const Name, ContentType, SRC, Script: ThtString);
{$else}
  {$ifdef UNICODE}
    procedure HotSpotChange(Sender: TObject; const URL: string);
    procedure HotSpotClick(Sender: TObject; const URL: string; var Handled: Boolean);
    procedure MetaRefreshEvent(Sender: TObject; Delay: Integer; const URL: String);
    procedure ObjectClick(Sender, Obj: TObject; const OnClick: String);
    procedure SoundRequest(Sender: TObject; const SRC: String; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; Const AnAction, Target, EncType, Method: String; Results: TStringList);
    procedure ViewerInclude(Sender: TObject; const Command: String; Params: TStrings; out IncludedDocument: TBuffer);
    procedure ViewerScript(Sender: TObject; const Name, ContentType, SRC, Script: string);
  {$else}
    procedure HotSpotChange(Sender: TObject; const URL: WideString);
    procedure HotSpotClick(Sender: TObject; const URL: WideString; var Handled: Boolean);
    procedure MetaRefreshEvent(Sender: TObject; Delay: Integer; const URL: WideString);
    procedure ObjectClick(Sender, Obj: TObject; const OnClick: WideString);
    procedure SoundRequest(Sender: TObject; const SRC: WideString; Loop: Integer; Terminate: Boolean);
    procedure SubmitEvent(Sender: TObject; Const AnAction, Target, EncType, Method: WideString; Results: TWideStringList);
    procedure ViewerInclude(Sender: TObject; const Command: WideString; Params: TWideStrings; out IncludedDocument: TBuffer);
    procedure ViewerScript(Sender: TObject; const Name, ContentType, SRC, Script: WideString);
  {$endif}
{$endif}
  private
    { Private declarations }
    Histories: array [0 .. MaxHistories - 1] of TMenuItem;
    MediaCount: Integer;
    FoundObject: TImageObj;
    NewWindowFile: ThtString;
    NextFile, PresentFile: ThtString;
    TimerCount: Integer;
    OldTitle: string;
    HintWindow: THintWindow;
    HintVisible: Boolean;
    NLS: string;

    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure CloseAll;
    procedure UpdateCaption;
{$ifdef LCL}
{$else}
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
{$endif}
{$if defined(LCL) or defined(Compiler32_Plus)}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
{$ifend}
  public
    { Public declarations }
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

{$ifdef TScrollStyleInSystemUITypes}
uses
  System.UITypes;
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
  Info := Info + ' New PixelsPerInch = ' + IntToStr(PixelsPerInch);
  Edit1.Text := Info;
end;
{$ifend}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  Info: string;
begin
  BoundsRect := htGetNiceFormSize( Monitor, PixelsPerInch );

  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));

  ShowImages.Checked := Viewer.ViewImages;
  Viewer.HistoryMaxCount := MaxHistories; { defines size of history list }
{$ifdef HasGestures}
  Viewer.Touch.InteractiveGestureOptions := [igoPanSingleFingerHorizontal,
    igoPanSingleFingerVertical, igoPanInertia];
  Viewer.Touch.InteractiveGestures := [igPan];
{$endif}

{$ifdef Compiler32_Plus}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
{$endif}

  for I := 0 to MaxHistories - 1 do
  begin { create the MenuItems for the history list }
    Histories[I] := TMenuItem.Create(HistoryMenuItem);
    HistoryMenuItem.Insert(I, Histories[I]);
    with Histories[I] do
    begin
      Visible := False;
      OnClick := HistoryClick;
      Tag := I;
    end;
  end;

  HintWindow := ThtHintWindow.Create(Self);
  HintWindow.Color := $CCFFFF;

  if sizeof(char) = 1 then
    Info := 'Program uses single byte characters.'
  else
    Info := 'Program uses unicode characters.';
{$if defined(LCL) or defined(Compiler22_Plus)}
  Info := Info + ' PixelsPerInch = ' + IntToStr(Monitor.PixelsPerInch);
  Edit1.Text := Info;
{$ifend}
  UpdateCaption;
  ReloadButton.Enabled := Viewer.Text <> '';

{$ifdef LCL}
{$else}
  DragAcceptFiles(Handle, True);
  Application.OnMessage := AppMessage;
{$endif}
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if (ParamCount >= 1) then
  begin { Parameter is file to load }
    S := CmdLine;
    I := Pos('" ', S);
    if I > 0 then
      Delete(S, 1, I + 1) { delete EXE name in quotes }
    else
      Delete(S, 1, Length(ParamStr(0))); { in case no quote marks }
    I := Pos('"', S);
    while I > 0 do { remove any quotes from parameter }
    begin
      Delete(S, I, 1);
      I := Pos('"', S);
    end;
    Viewer.LoadFromFile(Viewer.HtmlExpandFilename(S));
  end
//  else
//    Viewer.LoadFromResource(HINSTANCE, 'XLeft1', HTMLType);
end;

procedure TForm1.OpenFileClick(Sender: TObject);
var
  TheSize: TSize;
begin
  if (Viewer.CurrentFile <> '') and (Copy(Viewer.CurrentFile, 1, 9) <> 'source://') then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile)
  else
    OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog.Filter := 'HTML Files (*.htm,*.html)|*.htm;*.html' +
    '|Text Files (*.txt)|*.txt' + '|All Files (*.*)|*.*';
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
  begin
    Update;
    Viewer.LoadFromFile(OpenDialog.Filename);
    TheSize := Viewer.FullDisplaySize(Viewer.ClientWidth);
    UpdateCaption;
  end;
end;

procedure TForm1.HotSpotChange(Sender: TObject; const URL: ThtString);
{ mouse moved over or away from a hot spot.  Change the status line }
var
  Caption: ThtString;
begin
  Caption := '';
  if URL <> '' then
    Caption := Caption + 'URL: ' + URL + '     ';
  if Viewer.TitleAttr <> '' then
    Caption := Caption + 'Title: ' + Viewer.TitleAttr;
  Panel1.Caption := Caption;
end;

procedure TForm1.HotSpotClick(Sender: TObject; const URL: ThtString; var Handled: Boolean);
{ This routine handles what happens when a hot spot is clicked.  The assumption
  is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
  handled here, but other file types could be easily added.

  If the URL is handled here, set Handled to True.  If not handled here, set it
  to False and ThtmlViewer will handle it. }
{$ifndef MultiMediaMissing}
const
  snd_Async = $0001; { play asynchronously }
{$endif}
var
  PC: array [0 .. 255] of {$ifdef UNICODE} WideChar {$else} AnsiChar {$endif};
  uURL, S, Params, Ext: ThtString;
  I, J, K: Integer;
begin
  Handled := False;

  { check for various file types }
  uURL := htUpperCase(URL);
  I := Pos(':', uURL);
  J := Pos('FILE:', uURL);
  if (I <= 2) or (J > 0) then
  begin { apparently the URL is a filename }
    S := URL;
    K := Pos('?', S);  {look for parameters, could be '?x,y' , etc}
    if K > 0 then
    begin
      Params := Copy(S, K + 1, 255); { save any parameters }
      SetLength(S, K - 1); { truncate S }
    end
    else
      Params := '';
    S := (Sender as THTMLViewer).HtmlExpandFilename(S);
    Ext := htUpperCase(ExtractFileExt(S));
    if Ext = '.WAV' then
    begin
      Handled := True;
{$ifndef MultiMediaMissing}
      sndPlaySound(StrPCopy(PC, S), snd_Async);
{$endif}
    end
    else if Ext = '.EXE' then
      Handled := StartProcess(S, Params)
    else if (Ext = '.MID') or (Ext = '.AVI') then
      Handled := OpenDocument(S);
    { else ignore other extensions }
  end
  else
  begin
    I := Pos('MAILTO:', uURL) + Pos('HTTP://', uURL) + Pos('HTTPS://', uURL);
    if I > 0 then
      Handled := OpenDocument(URL);
  end;

  Edit1.Text := URL; { other protocoll }
end;

procedure TForm1.ShowImagesClick(Sender: TObject);
{ The Show Images menu item was clicked }
begin
  With Viewer do
  begin
    ViewImages := not ViewImages;
    (Sender as TMenuItem).Checked := ViewImages;
  end;
end;

procedure TForm1.ReloadButtonClick(Sender: TObject);
{ the Reload button was clicked }
begin
  with Viewer do
  begin
    ReloadButton.Enabled := False;
    if CurrentFile <> '' then
      ReLoad
    else
      ReText;
    Viewer.Realign;
    ReloadButton.Enabled := Text <> '';
    Viewer.SetFocus;
  end;
end;

procedure TForm1.FwdBackClick(Sender: TObject);
{ Either the Forward or Back button was clicked }
begin
  with Viewer do
  begin
    if Sender = BackButton then
      HistoryIndex := HistoryIndex + 1
    else
      HistoryIndex := HistoryIndex - 1;
    UpdateCaption;
  end;
end;

procedure TForm1.HistoryChange(Sender: TObject);
{ This event occurs when something changes history list }
var
  I: Integer;
  Cap: ThtString;
  HI: THistoryItem;
begin
  with Sender as THTMLViewer do
  begin
    { check to see which buttons are to be enabled }
    FwdButton.Enabled := HistoryIndex > 0;
    BackButton.Enabled := HistoryIndex < History.Count - 1;

    { Enable and caption the appropriate history menuitems }
    HistoryMenuItem.Visible := History.Count > 0;
    for I := 0 to MaxHistories - 1 do
      with Histories[I] do
        if I < History.Count then
        begin
          HI := History[I];
          Cap := HI.URL;
          if HI.Title <> '' then
            Cap := Cap + '--' + HI.Title;
          Caption := Cap; { Cap limits string to 80 char }
          Visible := True;
          Checked := I = HistoryIndex;
        end
        else
          Histories[I].Visible := False;
    UpdateCaption;
    Viewer.SetFocus;
  end;
end;

procedure TForm1.HistoryClick(Sender: TObject);
{ A history list menuitem got clicked on }
begin
  { Changing the HistoryIndex loads and positions the appropriate document }
  Viewer.HistoryIndex := (Sender as TMenuItem).Tag;
end;

procedure TForm1.About1Click(Sender: TObject);
begin
  with TAboutBox.CreateIt(Self, 'HTMLDemo', 'THtmlViewer') do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FontsClick(Sender: TObject);
var
  FontForm: TFontForm;
begin
  FontForm := TFontForm.Create(Self);
  try
    with FontForm do
    begin
      FontName := ReadFontName(Viewer.DefFontName);
      FontColor := Viewer.DefFontColor;
      FontSize := Viewer.DefFontSize;
      HotSpotColor := Viewer.DefHotSpotColor;
      Background := Viewer.DefBackground;
      if ShowModal = mrOK then
      begin
        Viewer.DefFontName := FontName;
        Viewer.DefFontColor := FontColor;
        Viewer.DefFontSize := FontSize;
        Viewer.DefHotSpotColor := HotSpotColor;
        Viewer.DefBackground := Background;
        ReloadButtonClick(Self); { reload to see how it looks }
      end;
    end;
  finally
    FontForm.Free;
  end;
end;

procedure TForm1.Print1Click(Sender: TObject);
begin
  PrintWithDialog(Self, PrintDialog, Viewer);
end;

procedure TForm1.PrinterSetupClick(Sender: TObject);
begin
  PrinterSetupDialog.Execute;
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

procedure TForm1.Find1Click(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TForm1.FindDialogFind(Sender: TObject);
begin
  with FindDialog do
  begin
    if not Viewer.FindEx(FindText, frMatchCase in Options,
      not(frDown in Options)) then
      MessageDlg('No further occurances of "' + FindText + '"', mtInformation,
        [mbOK], 0);
  end;
end;

procedure TForm1.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
var
  Enabled: Boolean;
begin
  Enabled := not ProcessingOn;
  if ProcessingOn then
    CloseAll; { in case hint window is open }
  FwdButton.Enabled := Enabled and (Viewer.HistoryIndex > 0);
  BackButton.Enabled := Enabled and
    (Viewer.HistoryIndex < Viewer.History.Count - 1);
  RepaintButton.Enabled := Enabled and (Viewer.Text <> '');
  ReloadButton.Enabled := Enabled and (Viewer.Text <> '');
  Print1.Enabled := Enabled and (Viewer.Text <> '');
  Printpreview.Enabled := Print1.Enabled;
  Find1.Enabled := Print1.Enabled;
  SelectAllItem.Enabled := Print1.Enabled;
  Open.Enabled := Enabled;
end;

procedure TForm1.CopyItemClick(Sender: TObject);
begin
  Viewer.CopyToClipboard;
end;

procedure TForm1.Edit2Click(Sender: TObject);
begin
  CopyItem.Enabled := Viewer.SelLength <> 0;
end;

procedure TForm1.SelectAllItemClick(Sender: TObject);
begin
  Viewer.SelectAll;
end;

procedure TForm1.OpenTextFileClick(Sender: TObject);
begin
  if Viewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := 'HTML Files (*.htm,*.html)|*.htm;*.html' +
    '|Text Files (*.txt)|*.txt' + '|All Files (*.*)|*.*';
  OpenDialog.FilterIndex := 2;
  if OpenDialog.Execute then
  begin
    ReloadButton.Enabled := False;
    Update;
    Viewer.LoadFromFile(OpenDialog.Filename, TextType);
    if Viewer.CurrentFile <> '' then
    begin
      UpdateCaption;
      ReloadButton.Enabled := True;
    end;
  end;
end;

procedure TForm1.OpenImageFileClick(Sender: TObject);
begin
  if Viewer.CurrentFile <> '' then
    OpenDialog.InitialDir := ExtractFilePath(Viewer.CurrentFile);
  OpenDialog.Filter := GetImageLoader.GetFilter('Graphics Files') +
    '|All Files (*.*)|*.*';
  OpenDialog.FilterIndex := 1;
  if OpenDialog.Execute then
  begin
    ReloadButton.Enabled := False;
    Viewer.LoadFromFile(OpenDialog.Filename, ImgType);
    if Viewer.CurrentFile <> '' then
    begin
      UpdateCaption;
      ReloadButton.Enabled := True;
    end;
  end;
end;

procedure TForm1.wmDropFiles(var Message: TMessage);
{$ifdef LCL}
begin
{$else}
var
  S: string;
begin
  setLength(S, 1024);
  setLength(S, DragQueryFile(Message.WParam, 0, @S[1], 1024));
  DragFinish(Message.WParam);
  if Length(S) > 0 then
    Viewer.LoadFromFile(S);
{$endif}
  Message.Result := 0;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // Dropping File in LCL
  Viewer.LoadFromFile(FileNames[0]);
end;

procedure TForm1.MediaPlayerNotify(Sender: TObject);
begin
{$ifndef LCL}
{$ifdef MsWindows}
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
          Close;
      end;
  except
  end;
{$endif}
{$endif}
end;

procedure TForm1.SoundRequest(Sender: TObject; const SRC: ThtString; Loop: Integer; Terminate: Boolean);
begin
{$ifndef LCL}
{$ifdef MsWindows}
  try
    with MediaPlayer do
      if Terminate then
        Close
      else
      begin
        Filename := (Sender as THTMLViewer).HtmlExpandFilename(SRC);
        Notify := True;
        Open;
        if Loop < 0 then
          MediaCount := 9999
        else if Loop = 0 then
          MediaCount := 1
        else
          MediaCount := Loop;
      end;
  except
  end;
{$endif}
{$endif}
end;

procedure TForm1.ViewimageClick(Sender: TObject);
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

procedure TForm1.CopyImageToClipboardClick(Sender: TObject);
begin
  Clipboard.Assign(FoundObject.Graphic);
end;

procedure TForm1.ObjectClick(Sender, Obj: TObject; const OnClick: ThtString);
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
  // MessageDlg(OnClick, mtCustom, [mbOK], 0);
end;

procedure TForm1.ViewerInclude(Sender: TObject; const Command: ThtString; Params: ThtStrings; out IncludedDocument: TBuffer);
{ OnInclude handler }
var
  Filename: ThtString;
  I: Integer;
  Stream: TFileStream;
begin
  if htCompareText(Command, 'Date') = 0 then
    IncludedDocument := TBuffer.Create(DateToStr(Date)) { <!--#date --> }
  else if htCompareText(Command, 'Time') = 0 then
    IncludedDocument := TBuffer.Create(TimeToStr(Time)) { <!--#time --> }
  else if htCompareText(Command, 'Include') = 0 then
  begin { an include file <!--#include FILE="filename" --> }
    if (Params.Count >= 1) then
    begin
      I := Pos('file=', Lowercase(Params[0]));
      if I > 0 then
      begin
        Filename := Copy(Params[0], 6, Length(Params[0]) - 5);
        try
          if FileExists(Filename) then
          begin
            Stream := TFileStream.Create(Filename, fmOpenRead or
              fmShareDenyWrite);
            IncludedDocument := TBuffer.Create(Stream);
          end
        except
        end;
      end;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  HintWindow.Free;
end;

procedure TForm1.RightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  Pt: TPoint;
  S, Dest: ThtString;
  HintWindow: ThtHintWindow;
  ARect: TRect;
begin
  with Parameters do
  begin
    FoundObject := Image;
    Viewimage.Enabled := (FoundObject <> Nil) and (FoundObject.Image <> Nil);
    CopyImageToClipboard.Enabled := (FoundObject <> Nil) and (FoundObject.Graphic <> Nil);
    if URL <> '' then
    begin
      SplitDest(URL, S, Dest);
      if S = '' then
        S := Viewer.CurrentFile
      else
        S := Viewer.HtmlExpandFilename(S);
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
  StartProcess(ParamStr(0), NewWindowFile);
end;

procedure TForm1.MetaTimerTimer(Sender: TObject);
begin
  MetaTimer.Enabled := False;
  if Viewer.CurrentFile = PresentFile
  then { don't load if current file has changed }
  begin
    Viewer.LoadFromFile(NextFile);
    UpdateCaption;
  end;
end;

procedure TForm1.MetaRefreshEvent(Sender: TObject; Delay: Integer; const URL: ThtString);
begin
  NextFile := Viewer.HtmlExpandFilename(URL);
  if FileExists(NextFile) then
  begin
    PresentFile := Viewer.CurrentFile;
    MetaTimer.Interval := Delay * 1000;
    MetaTimer.Enabled := True;
  end;
end;

procedure TForm1.PrintPreviewClick(Sender: TObject);
{$ifndef NoMetaFile}
var
{$ifdef UseOldPreviewForm}
  pf: TPreviewForm;
{$else UseOldPreviewForm}
  pf: TBegaHtmlPrintPreviewForm;
{$endif UseOldPreviewForm}
  Abort: Boolean;
begin
{$ifdef UseOldPreviewForm}
  pf := TPreviewForm.CreateIt(Self, Viewer, Abort);
{$else UseOldPreviewForm}
  pf := TBegaHtmlPrintPreviewForm.Create(Self);
  pf.HtmlViewer := Viewer;
  Abort := False;
{$endif UseOldPreviewForm}
  try
    if not Abort then
      pf.ShowModal;
  finally
    pf.Free;
  end;
{$else !NoMetaFile}
begin
{$endif !NoMetaFile}
end;

procedure TForm1.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  TitleStr: string;
begin
  if not Timer1.Enabled and Assigned(ActiveControl) and ActiveControl.Focused
  then { 9.25 }
  begin
    TitleStr := Viewer.TitleAttr;
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
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  StartCount = 2; { timer counts before hint window opens }
  EndCount = 20; { after this many timer counts, hint window closes }
var
  Pt, Pt1: TPoint;
  ARect: TRect;
  TitleStr: string;

begin
  Inc(TimerCount);
  GetCursorPos(Pt);
  Pt1 := Viewer.ScreenToClient(Pt);
  TitleStr := Viewer.TitleAttr;
  if (TitleStr = '') or not PtInRect(Viewer.ClientRect, Pt1) then
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
  else if (TimerCount >= StartCount) and not HintVisible then
  begin
{$ifdef ver90}  { Delphi 2 }
    ARect := Rect(0, 0, 0, 0);
    DrawText(HintWindow.Canvas.Handle, PChar(TitleStr), Length(TitleStr), ARect,
      DT_CALCRECT);
{$else}
    ARect := HintWindow.CalcHintRect(300, TitleStr, Nil);
{$endif}
    with ARect do
      HintWindow.ActivateHint(Rect(Pt.X, Pt.Y + 18, Pt.X + Right,
        Pt.Y + 18 + Bottom), TitleStr);
    HintVisible := True;
  end;
end;

procedure TForm1.ViewerProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Integer);
begin
  ProgressBar.Position := PercentDone;
  case Stage of
    psStarting:
      ProgressBar.Visible := True;
    psRunning:
      ;
    psEnding:
      ProgressBar.Visible := False;
  end;
  ProgressBar.Update;
end;

procedure TForm1.ViewerScript(Sender: TObject; const Name, ContentType, SRC, Script: ThtString);
begin
  NLS := Name + '::' + ContentType + '::' + SRC + '::'#10#13 + Script;
end;

{ HTML for print header and footer }
const
  HFText: ThtString = '<html><head><style>' + 'body  {font: sans-serif 8pt;}' +
    '</style></head>' + '<body marginwidth="0">' +
    '<table border="0" cellspacing="2" cellpadding="1" width="100%">' + '<tr>' +
    '<td>#left</td><td align="right">#right</td>' + '</tr>' +
    '</table></body></html>';

function ReplaceStr(Const S, FromStr, ToStr: ThtString): ThtString;
{ replace FromStr with ToStr in string S.
  for Delphi 6, 7, AnsiReplaceStr may be used instead. }
var
  I: Integer;
begin
  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Result := S;
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end;
end;

procedure TForm1.ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
  NumPage: Integer; LastPage: Boolean; var XL, XR: Integer; var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', Viewer.DocumentTitle);
  S := ReplaceStr(S, '#right', Viewer.CurrentFile);
  HFViewer.Text := S;
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
  NumPage: Integer; LastPage: Boolean; var XL, XR: Integer; var StopPrinting: Boolean);
var
  S: ThtString;
begin
  S := ReplaceStr(HFText, '#left', DateToStr(Date));
  S := ReplaceStr(S, '#right', 'Page ' + IntToStr(NumPage));
  HFViewer.Text := S;
end;

procedure TForm1.RepaintButtonClick(Sender: TObject);
begin
  Viewer.Repaint;
end;

procedure TForm1.UpdateCaption;
var
  Title, Cap: ThtString;
begin
  if Viewer.DocumentTitle <> '' then
    Title := Viewer.DocumentTitle
  else if Viewer.URL <> '' then
    Title := Viewer.URL
  else if (Viewer.CurrentFile <> '') and (Copy(Viewer.CurrentFile, 1, 9) <> 'source://') then
    Title := Viewer.CurrentFile
  else
    Title := '';

  Cap := 'HtmlViewer ' + VersionNo + ' Demo';
  if Title <> '' then
    Cap := Cap + ' - ' + Title;
{$ifdef LCL}
  Caption := UTF8Encode(Cap);
{$else}
  Caption := Cap;
{$endif}
end;

{$ifdef LCL}
{$else}

// -- BG ---------------------------------------------------------- 16.08.2015 --
procedure TForm1.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  WinCtrl: TWinControl;
begin
  if Msg.Message = WM_MOUSEWHEEL then
  begin
    WinCtrl := FindVCLWindow(Point(Word(Msg.lParam), HiWord(Msg.lParam)));
    if (WinCtrl is TPaintPanel)
    {$ifndef UseOldPreviewForm} or (WinCtrl is TBegaZoomBox)
    {$endif UseOldPreviewForm} then
    begin
      // perform mouse wheel scrolling for the control under the mouse:
      WinCtrl.Perform(CM_MOUSEWHEEL, Msg.WParam, Msg.lParam);
      Handled := True;
    end;
  end;
end;
{$endif}

end.
