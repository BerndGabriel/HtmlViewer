{
Version   10.2
Copyright (c) 1995-2008 by L. David Baldwin, 2008-2010 by HtmlViewer Team

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
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, MMSystem, Clipbrd, ShellAPI, FontDlg, ComCtrls, StdCtrls,
{$ifdef LCL}
  LclIntf, LclType, LResources, FPImage,
{$else}
  MPlayer,
  {$if CompilerVersion > 15}
    XpMan,
  {$ifend}
{$endif}
{$ifdef UNICODE}
  WideStrings,
{$endif}
{$ifdef UseTNT}
  TntStdCtrls, TntClasses, SubmitTnt,
  {$ifdef Compiler17_Plus}
  {$else}
    TntWideStrings,
  {$endif}
{$else UseTNT}
  {$ifdef UseElPack}
  ElListBox, ElCombos, ElEdits, ElPopBtn,
  {$else UseElPack}
  {$endif UseElPack}
  Submit,
{$endif UseTNT}
  HtmlGlobals, UrlSubs, StyleUn, Readhtml, HTMLsubs, HTMLun2, Htmlview, FramView,
  DemoSubs, HTMLAbt, PreviewForm, ImgForm;

const
  MaxHistories = 6;  {size of History list}

type

  TForm1 = class(TForm)
    About1: TMenuItem;
    BackButton: TButton;
    Copy1: TMenuItem;
    CopyImagetoclipboard: TMenuItem;
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
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Open1: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenInNewWindow: TMenuItem;
    Options1: TMenuItem;
    Panel1: TPanel;
    PopupMenu: TPopupMenu;
    Print1: TMenuItem;
    PrinterSetup: TMenuItem;
    PrintPreview1: TMenuItem;
    ReloadButton: TButton;
    SelectAll1: TMenuItem;
    SetPrintScale: TMenuItem;
    Showimages: TMenuItem;
    Timer1: TTimer;
    ViewImage: TMenuItem;
    Panel3: TPanel;
    ProgressBar: TProgressBar;
    InfoPanel: TPanel;
{$ifdef LCL}
{$else}
    MediaPlayer: TMediaPlayer;
    PrintDialog: TPrintDialog;
    PrinterSetupDialog: TPrinterSetupDialog;
{$endif}
    procedure About1Click(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
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
    procedure FrameViewerInclude(Sender: TObject; const Command: String; Params: TStrings; var S: String);
    procedure FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$ifdef UseTNT}
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: WideString);
{$else}
    procedure FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: string);
{$endif}
    procedure FrameViewerProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Integer);
    procedure FrameViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure FwdButtonClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure PrinterSetupClick(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure ReloadClick(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure SetPrintScaleClick(Sender: TObject);
    procedure ShowimagesClick(Sender: TObject);
    procedure SoundRequest(Sender: TObject; const SRC: String; Loop: Integer; Terminate: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewImageClick(Sender: TObject);
{$ifdef UseTNT}
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: WideString);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: WideString; var Handled: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: WideString; Results: TTntStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: WideString);
{$else}
    procedure HotSpotTargetCovered(Sender: TObject; const Target, URL: String);
    procedure HotSpotTargetClick(Sender: TObject; const Target, URL: String; var Handled: Boolean);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: String; Results: TStringList);
    procedure WindowRequest(Sender: TObject; const Target, URL: String);
{$endif}
  private
    { Private declarations }
    Histories: array[0..MaxHistories-1] of TMenuItem;
    FoundObject: TImageObj;
    NewWindowFile: string;
    MediaCount: integer;
    ThePlayer: TOBject;
    TimerCount: integer;
    OldTitle: string;
    HintWindow: THintWindow;
    HintVisible: boolean;
    TitleViewer: ThtmlViewer;
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
{$else}
{$R *.DFM}
{$endif}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: integer;
begin
  Left := Left div 2;
  Top := Top div 2;
  Width := (Screen.Width * 8) div 10;
  Height := (Screen.Height * 6) div 8;

  {//< Temporary fix, for as long as the TFrameViewer component isn't registered :
    if Assigned(FrameViewer) then
      ShowMessage('Remove debug code')
    else
    begin
      FrameViewer := TFrameViewer.Create(Self);
      FrameViewer.Parent := Self;
      FrameViewer.Align := alClient;
    end;
  //>}

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
  DragAcceptFiles(Handle, True);
  HintWindow := THintWindow.Create(Self);
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
end;

procedure TForm1.HotSpotTargetClick(Sender: TObject; const Target, URL: ThtString; var Handled: boolean);
{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
const
  snd_Async = $0001;  { play asynchronously }
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
    sndPlaySound(StrPCopy(PC, S), snd_ASync);
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
  ShellExecute(Handle, nil, StrPCopy(PC, URL), nil, nil, SW_SHOWNORMAL);
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

procedure TForm1.Open1Click(Sender: TObject);
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

procedure TForm1.Exit1Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.Find1Click(Sender: TObject);
begin
FindDialog.Execute;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  S: string;
  I: integer;
begin
{$ifdef LCL}
  Print1.Visible := False;
  PrinterSetup.Visible := False;
  PrintPreview1.Visible := False;
{$endif}
if (ParamCount >= 1) then
  begin            {Parameter is file to load}
  S := CmdLine;
  I := Pos('" ', S);
  if I > 0 then
    Delete(S, 1, I+1)  {delete EXE name in quotes}
  else Delete(S, 1, Length(ParamStr(0)));  {in case no quote marks}
  I := Pos('"', S);
  while I > 0 do     {remove any quotes from paramenter}
    begin
    Delete(S, I, 1);
    I := Pos('"', S);
    end;
  FrameViewer.LoadFromFile(HtmlToDos(Trim(S)));
  end
else if FileExists(ExtractFilePath(ParamStr(0))+'demo.htm') then
  FrameViewer.LoadFromFile(ExtractFilePath(ParamStr(0))+'demo.htm');
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

procedure TForm1.Copy1Click(Sender: TObject);
begin
FrameViewer.CopyToClipboard;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
with FrameViewer do
  begin
  Copy1.Enabled := SelLength <> 0;
  SelectAll1.Enabled := (ActiveViewer <> Nil) and (ActiveViewer.CurrentFile <> '');
  Find1.Enabled := SelectAll1.Enabled;
  end;
end;

procedure TForm1.SelectAll1Click(Sender: TObject);
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
with Sender as TFrameViewer do
  begin
  {check to see which buttons are to be enabled}
  FwdButton.Enabled := FwdButtonEnabled;
  BackButton.Enabled := BackButtonEnabled;

  {Enable and caption the appropriate history menuitems}
  HistoryMenuItem.Visible := History.Count > 0;
  for I := 0 to MaxHistories-1 do
    with Histories[I] do
      if I < History.Count then
        Begin
        Cap := History.Strings[I];
        if TitleHistory[I] <> '' then
          Cap := Cap + '--' + TitleHistory[I];
        Caption := Cap;    {Cap limits string to 80 char}
        Visible := True;
        Checked := I = HistoryIndex;
        end
      else Histories[I].Visible := False;
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

procedure TForm1.Print1Click(Sender: TObject);
begin
{$ifdef LCL}
{$else}
  with PrintDialog do
    if Execute then
      if PrintRange = prAllPages then
        FrameViewer.Print(1, 9999)
      else
        FrameViewer.Print(FromPage, ToPage);
{$endif}
end;

procedure TForm1.File1Click(Sender: TObject);
begin
Print1.Enabled := FrameViewer.ActiveViewer <> Nil;
PrintPreview1.Enabled := Print1.Enabled;
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

{$ifdef UseTNT}
procedure TForm1.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: ThtString; Results: TTntStringList);
{$else}
procedure TForm1.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: ThtString; Results: TStringList);
{$endif}
begin
with SubmitForm do
  begin
  ActionText.Text := AnAction;
  MethodText.Text := Method;
  ResultBox.Items := Results;
  Results.Free;
  Show;
  end;
end;

procedure TForm1.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
begin
if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}
  FwdButton.Enabled := False;
  BackButton.Enabled := False;
  ReloadButton.Enabled := False;
  Print1.Enabled := False;
  PrintPreview1.Enabled := False;
  Find1.Enabled := False;
  SelectAll1.Enabled := False;
  Open1.Enabled := False;
  CloseAll;    {in case hint window is open}
  end
else
  begin
  FwdButton.Enabled := FrameViewer.FwdButtonEnabled;
  BackButton.Enabled := FrameViewer.BackButtonEnabled; 
  ReloadButton.Enabled := FrameViewer.CurrentFile <> '';
  Print1.Enabled := (FrameViewer.CurrentFile <> '') and (FrameViewer.ActiveViewer <> Nil);
  PrintPreview1.Enabled := Print1.Enabled;
  Find1.Enabled := Print1.Enabled;
  SelectAll1.Enabled := Print1.Enabled;
  Open1.Enabled := True;
  end;
end;

procedure TForm1.FwdButtonClick(Sender: TObject);
begin
FrameViewer.GoFwd;
end;

procedure TForm1.BackButtonClick(Sender: TObject);
begin
FrameViewer.GoBack;
end;

{$ifdef UseTNT}
procedure TForm1.WindowRequest(Sender: TObject; const Target, URL: WideString);
{$else}
procedure TForm1.WindowRequest(Sender: TObject; const Target, URL: String);
{$endif}
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
   StartProcess(StrPCopy(PC, ParamStr(0)+' "'+S+Dest+'"'), sw_Show);
end;

procedure TForm1.wmDropFiles(var Message: TMessage);
var
  S: string;
  Count: integer;
begin
  Count := DragQueryFile(Message.WParam, 0, @S[1], 200);
  SetLength(S, Count);
  DragFinish(Message.WParam);
  if Count >0 then
    FrameViewer.LoadFromFile(S);
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
{$ifdef LCL}
{$else}
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

procedure TForm1.SoundRequest(Sender: TObject; const SRC: String; Loop: Integer; Terminate: Boolean);
begin
{$ifdef LCL}
{$else}
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

{$ifdef UseTNT}
procedure TForm1.FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: WideString);
{$else}
procedure TForm1.FrameViewerObjectClick(Sender, Obj: TObject; const OnClick: string);
{$endif}
var
  S: ThtString;
begin
  if OnClick = 'display' then
  begin
    if Obj is TFormControlObj then
      with TFormControlObj(Obj) do
      begin
        if TheControl is TCheckBox then
          with TCheckBox(TheControl) do
          begin
            S := Value + ' is ';
            if Checked then
              S := S + 'checked'
            else
              S := S + 'unchecked';
            MessageDlg(S, mtCustom, [mbOK], 0);
          end
        else if TheControl is TRadioButton then
          with TRadioButton(TheControl) do
          begin
            S := Value + ' is checked';
            MessageDlg(S, mtCustom, [mbOK], 0);
          end;
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

procedure TForm1.FrameViewerInclude(Sender: TObject; const Command: String; Params: TStrings; var S: String);
{OnInclude handler}  
var
  Filename: string;
  I: integer;
begin
if CompareText(Command, 'Date') = 0 then
  S := DateToStr(Date) { <!--#date --> }
else if CompareText(Command, 'Time') = 0 then
  S := TimeToStr(Time)   { <!--#time -->  }
else if CompareText(Command, 'Include') = 0 then
  begin   {an include file <!--#include FILE="filename" -->  }
  if (Params.count >= 1) then
    begin
    I := Pos('file=', Lowercase(Params[0]));
    if I > 0 then
      begin
      Filename := copy(Params[0],  6, Length(Params[0])-5);
      try
        S := LoadStringFromFile(Filename);
      except
        end;
      end;
    end;
  end;
Params.Free;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  HintWindow.Free;
end;

procedure TForm1.FrameViewerRightClick(Sender: TObject; Parameters: TRightClickParameters);
var
  Pt: TPoint;
  S, Dest: string;
  I: integer;
  Viewer: ThtmlViewer;
  HintWindow: THintWindow;
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
    HintWindow := THintWindow.Create(Self);   
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
{$ifdef LCL}
{$else}
  PrinterSetupDialog.Execute;
{$endif}
end;

procedure TForm1.PrintPreview1Click(Sender: TObject);
{$ifdef LCL}
begin
end;
{$else}
var
  pf: TPreviewForm;
  Viewer: ThtmlViewer;
  Abort: boolean;
begin
  Viewer := FrameViewer.ActiveViewer;
  if Assigned(Viewer) then
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
{$endif}

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
  TitleStr: string;

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
  else if (TimerCount >= StartCount) and not HintVisible then
    begin
    {$ifdef ver90}  {Delphi 2}
    ARect := Rect(0,0,0,0);
    DrawText(HintWindow.Canvas.Handle, PChar(TitleStr), Length(TitleStr), ARect, DT_CALCRECT);
    {$else}
    ARect := HintWindow.CalcHintRect(300, TitleStr, Nil);
    {$endif}
    with ARect do
      HintWindow.ActivateHint(Rect(Pt.X, Pt.Y+18, Pt.X+Right, Pt.Y+18+Bottom), TitleStr);
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
  HFText: string =  '<html><head><style>'+
            'body  {font: Arial 8pt;}'+
          '</style></head>'+
          '<body marginwidth="0">'+
          '<table border="0" cellspacing="2" cellpadding="1" width="100%">'+
            '<tr>'+
              '<td>#left</td><td align="right">#right</td>'+
            '</tr>'+
          '</table></body></html>';

function ReplaceStr(Const S, FromStr, ToStr: string): string;
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
  S: string;
begin
S := ReplaceStr(HFText, '#left', FrameViewer.DocumentTitle);
S := ReplaceStr(S, '#right', FrameViewer.CurrentFile);
HFViewer.LoadFromString(S);
end;

procedure TForm1.ViewerPrintHTMLFooter(Sender: TObject;
  HFViewer: THTMLViewer; NumPage: Integer; LastPage: boolean; var XL, XR: integer; var StopPrinting: Boolean);
var
  S: string;
begin
S := ReplaceStr(HFText, '#left', DateToStr(Date));
S := ReplaceStr(S, '#right', 'Page '+IntToStr(NumPage));
HFViewer.LoadFromString(S);
end;

procedure TForm1.UpdateCaption;
begin
  if FrameViewer.DocumentTitle <> '' then
    Caption := 'FrameViewer Demo - ' + FrameViewer.DocumentTitle
  else
    Caption := 'FrameViewer Demo - <untitled document>';
end;

initialization
{$ifdef LCL}
{$I FDemUnit.lrs}
{$endif}
end.
