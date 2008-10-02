{$ifdef ver140}
{$warn Symbol_Platform Off}
{$endif}
{$ifdef ver150}
{$warn Symbol_Platform Off}
{$Define UseXpMan}
{$endif}
{$ifdef ver170}
{$warn Symbol_Platform Off}
{$Define UseXpMan}
{$endif}
{$ifdef ver180}
{$warn Symbol_Platform Off}
{$Define UseXpMan}
{$endif}

unit FDemUnit;
{A program to demonstrate the TFrameViewer component}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, FramView, ExtCtrls, StdCtrls, Menus, MMSystem,
  Clipbrd, HTMLsubs, HTMLun2, HTMLView, ShellAPI, PreviewForm,
  FontDlg, HTMLAbt, Submit, ImgForm, MPlayer, Readhtml, 
  {$ifdef UseXpMan} XpMan, {$endif} ComCtrls;

const
  MaxHistories = 6;  {size of History list}

  type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;                 
    Edit1: TMenuItem;
    Find1: TMenuItem;
    Panel2: TPanel;
    Copy1: TMenuItem;
    N2: TMenuItem;
    SelectAll1: TMenuItem;
    FindDialog: TFindDialog;
    Options1: TMenuItem;
    Showimages: TMenuItem;
    About1: TMenuItem;
    HistoryMenuItem: TMenuItem;
    PrintDialog: TPrintDialog;
    Print1: TMenuItem;
    Fonts: TMenuItem;
    FrameViewer: TFrameViewer;
    Panel1: TPanel;
    ReloadButton: TButton;
    FwdButton: TButton;
    BackButton: TButton;
    Edit2: TEdit;
    PopupMenu: TPopupMenu;
    CopyImagetoclipboard: TMenuItem;
    MediaPlayer: TMediaPlayer;
    ViewImage: TMenuItem;
    N3: TMenuItem;
    OpenInNewWindow: TMenuItem;
    PrintPreview1: TMenuItem;
    Timer1: TTimer;
    ProgressBar: TProgressBar;
    SetPrintScale: TMenuItem;
    PrinterSetupDialog: TPrinterSetupDialog;
    PrinterSetup: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ShowimagesClick(Sender: TObject);
    procedure HistoryClick(Sender: TObject);
    procedure HistoryChange(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FontsClick(Sender: TObject);
    procedure SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: string;
      Results: TStringList);
    procedure HotSpotTargetClick(Sender: TObject; const Target,
      URL: string; var Handled: Boolean);
    procedure HotSpotTargetChange(Sender: TObject; const Target,
      URL: string);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure WindowRequest(Sender: TObject; const Target,
      URL: string);
    procedure BackButtonClick(Sender: TObject);
    procedure FwdButtonClick(Sender: TObject);
    procedure CopyImagetoclipboardClick(Sender: TObject);
    procedure MediaPlayerNotify(Sender: TObject);
    procedure SoundRequest(Sender: TObject; const SRC: String;
      Loop: Integer; Terminate: Boolean);
    procedure FrameViewerObjectClick(Sender, Obj: TObject;
      const OnClick: String);
    procedure ViewImageClick(Sender: TObject);
    procedure FrameViewerInclude(Sender: TObject; const Command: String;
      Params: TStrings; var S: string);
    procedure FormDestroy(Sender: TObject);
    procedure FrameViewerRightClick(Sender: TObject;
      Parameters: TRightClickParameters);
    procedure OpenInNewWindowClick(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure FrameViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FrameViewerProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Integer);
    procedure SetPrintScaleClick(Sender: TObject);
    procedure ViewerPrintHTMLFooter(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure ViewerPrintHTMLHeader(Sender: TObject; HFViewer: THTMLViewer;
      NumPage: Integer; LastPage: Boolean; var XL, XR: Integer;
      var StopPrinting: Boolean);
    procedure PrinterSetupClick(Sender: TObject);
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

    procedure wmDropFiles(var Message: TMessage); message wm_DropFiles;
    procedure CloseAll;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: integer;
begin
Left := Left div 2;
Top := Top div 2;
Width := (Screen.Width * 8) div 10;
Height := (Screen.Height * 6) div 8;

//< Temporary fix, for as long as the TFrameViewer component isn't registered :
  if Assigned(FrameViewer) then
    ShowMessage('Remove debug code')
  else
  begin
    FrameViewer := TFrameViewer.Create(Self);
    FrameViewer.Parent := Self;
    FrameViewer.Align := alClient;
  end;
//>
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
end;

procedure TForm1.HotSpotTargetClick(Sender: TObject; const Target, URL: string;
          var Handled: boolean);
{This routine handles what happens when a hot spot is clicked.  The assumption
 is made that DOS filenames are being used. .EXE, .WAV, .MID, and .AVI files are
 handled here, but other file types could be easily added.

 If the URL is handled here, set Handled to True.  If not handled here, set it
 to False and ThtmlViewer will handle it.}
const
  snd_Async = $0001;  { play asynchronously }
var
  PC: array[0..255] of AnsiChar;
  S, Params: string[255];
  Ext: string[5];
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
    Viewer.IDDisplay[ID+'Plus'] := not Viewer.IDDisplay[ID+'Plus'];
    Viewer.IDDisplay[ID+'Minus'] := not Viewer.IDDisplay[ID+'Minus'];
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
    S[0] := AnsiChar(K-1);            {truncate S}
    end
  else Params := '';
  S := (Sender as TFrameViewer).HTMLExpandFileName(S);
  Ext := Uppercase(ExtractFileExt(S));
  if Ext = '.WAV' then
    begin
    Handled := True;
    sndPlaySoundA(StrPCopy(PC, S), snd_ASync);
    end
  else if Ext = '.EXE' then
    begin
    Handled := True;
    WinExec(StrPCopy(PC, S+' '+Params), sw_Show);
    end
  else if (Ext = '.MID') or (Ext = '.AVI')  then
    begin
    Handled := True;
    WinExec(StrPCopy(PC, 'MPlayer.exe /play /close '+S), sw_Show);
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
  ShellExecuteA(Handle, nil, StrPCopy(PC, URL), nil, nil, SW_SHOWNORMAL);
  Handled := True;
  Exit;
  end;
Edit2.Text := URL;   {other protocall}
end;

procedure TForm1.HotSpotTargetChange(Sender: TObject; const Target, URL: string);
{mouse moved over or away from a hot spot.  Change the status line}
begin
if URL = '' then
  Panel2.Caption := ''
else if Target <> '' then
  Panel2.Caption := 'Target: '+Target+'  URL: '+URL
else
  Panel2.Caption := 'URL: '+URL
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
if FrameViewer.CurrentFile <> '' then
  OpenDialog.InitialDir := ExtractFilePath(FrameViewer.CurrentFile)
else OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
OpenDialog.FilterIndex := 1;
if OpenDialog.Execute then
  begin
  FrameViewer.LoadFromFile(OpenDialog.Filename);
  Caption := FrameViewer.DocumentTitle;
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
  Cap: string[80];
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
  Caption := DocumentTitle;    {keep the caption updated}
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
AboutBox := TAboutBox.CreateIt(Self, 'FrameDem', 'TFrameViewer');
try
  AboutBox.ShowModal;
finally
  AboutBox.Free;
  end;
end;

procedure TForm1.Print1Click(Sender: TObject);
begin
with PrintDialog do
  if Execute then
    if PrintRange = prAllPages then
      FrameViewer.Print(1, 9999)
    else
      FrameViewer.Print(FromPage, ToPage);
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

procedure TForm1.SubmitEvent(Sender: TObject; const AnAction, Target, EncType, Method: String;
  Results: TStringList);
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

procedure TForm1.WindowRequest(Sender: TObject; const Target,
  URL: string);
var
  S, Dest: string[255];
  I: integer;
  PC: array[0..255] of AnsiChar;
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
   WinExec(StrPCopy(PC, ParamStr(0)+' "'+S+Dest+'"'), sw_Show);
end;

procedure TForm1.wmDropFiles(var Message: TMessage);
var
  S: string[200];
  Count: integer;
begin
Count := DragQueryFile(Message.WParam, 0, @S[1], 200);
Length(S) := Count;
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
  ImageFormBitmap := FoundObject.Bitmap;
  Caption := '';
  Show;
  end;
end;

procedure TForm1.MediaPlayerNotify(Sender: TObject);
begin
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
end;

procedure TForm1.SoundRequest(Sender: TObject; const SRC: String;
  Loop: Integer; Terminate: Boolean);
begin
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
      if Sender is ThtmlViewer then
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
end;

procedure TForm1.FrameViewerObjectClick(Sender, Obj: TObject;
  const OnClick: String);
var
  S: string;
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
          if Checked then S := S + 'checked'
            else S := S + 'unchecked';
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
      MessageDlg(OnClick, mtCustom, [mbOK], 0);
end;

procedure TForm1.FrameViewerInclude(Sender: TObject; const Command: String;
  Params: TStrings; var S: string);
{OnInclude handler}  
var
  Filename: string;
  I: integer;
  MS: TMemoryStream;
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
      MS := TMemoryStream.Create;
      try
        try
          MS.LoadFromFile(Filename);
          SetString(S, PChar(MS.Memory), MS.Size);
        finally
          MS.Free;
          end;
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
  PC: array[0..255] of AnsiChar;
begin
  WinExec(StrPCopy(PC, AnsiString(ParamStr(0)+' "'+NewWindowFile+'"')), sw_Show);
end;

procedure TForm1.PrinterSetupClick(Sender: TObject);
begin
PrinterSetupDialog.Execute;
end;

procedure TForm1.PrintPreview1Click(Sender: TObject);
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

end.
