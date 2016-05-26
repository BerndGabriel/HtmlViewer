unit logwin;

interface
{$include htmlcons.inc}
{$include options.inc}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$ifdef UseVCLStyles}
  Vcl.Styles,
  Vcl.Themes,
  Vcl.ActnPopup,
{$endif}
  Dialogs, StdCtrls, IniFiles, Menus, SyncObjs;

type
  ThtLogSubject = (laDiag, laHttpHeader, laHttpScript);
  ThtLogSubjectSet = set of ThtLogSubject;

{$ifdef UseVCLStyles}
  TPopupMenu = class(Vcl.ActnPopup.TPopupActionBar);
{$endif}
  TLogForm = class(TForm)
    LogMemo: TMemo;
    PopupMenu: TPopupMenu;
    pCopy: TMenuItem;
    pClear: TMenuItem;
    pCopyAll: TMenuItem;
    procedure pClearClick(Sender: TObject);
    procedure pCopyAllClick(Sender: TObject);
    procedure pCopyClick(Sender: TObject);
    procedure LogMemoDblClick(Sender: TObject);
    procedure LogMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLogged: Boolean;
    FLogIt: ThtLogSubjectSet;
    function GetLogIt(Index: ThtLogSubject): Boolean;
    procedure SetLogIt(Index: ThtLogSubject; const Value: Boolean);
  public
    Lock: TCriticalSection;
    Strings: TStringList;
    constructor Create(AOwner: TComponent; ATop, ALeft, AWidth, AHeight: Integer);
    procedure Log(const AString: string); overload;
    procedure Log(const AStrings: TStrings); overload;
    procedure LogSynced(const AString: string); overload;
    procedure LogSynced(const AStrings: TStrings); overload;
    destructor Destroy; override;
    property LogActive[Index: ThtLogSubject]: Boolean read GetLogIt write SetLogIt;
    property Logged: Boolean read FLogged write FLogged;
  end;

var
  LogForm: TLogForm;

implementation

{$R *.dfm}

//-- BG ---------------------------------------------------------- 23.05.2016 --
constructor TLogForm.Create(AOwner: TComponent; ATop, ALeft, AWidth, AHeight: Integer);
begin
  inherited Create(AOwner);
  Top := ATop;
  Left := ALeft;
  if AWidth > 0 then
    Width := AWidth;
  if AHeight > 0 then
    Height := AHeight;
  Lock := TCriticalSection.Create;
  Strings := TStringList.Create;
end;

destructor TLogForm.Destroy;
begin
  Strings.Free;
  Lock.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.Log(const AString: string);
begin
  LogMemo.Lines.Add(AString);
//  Visible := True;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.Log(const AStrings: TStrings);
begin
  LogMemo.Lines.AddStrings(AStrings);
//  Visible := True;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.LogSynced(const AString: string);
begin
  Lock.Enter;
  try
    Log(AString);
  finally
    Lock.Leave;
  end;
//  Visible := True;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.LogSynced(const AStrings: TStrings);
begin
  Lock.Enter;
  try
    Log(AStrings);
  finally
    Lock.Leave;
  end;
//  Visible := True;
end;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

//-- BG ---------------------------------------------------------- 25.05.2016 --
function TLogForm.GetLogIt(Index: ThtLogSubject): Boolean;
begin
  Result := Index in FLogIt;
end;

procedure TLogForm.LogMemoDblClick(Sender: TObject);
begin
  LogMemo.Lines.Clear;
end;

procedure TLogForm.LogMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
  if Key = VK_CONTROL then exit;
  if NOT (ssCtrl in Shift) then exit;
  if Key = Ord ('C') then LogMemo.CopyToClipboard;
  if Key = Ord ('A') then
  begin
      LogMemo.SelectAll;
      LogMemo.CopyToClipboard;
  end;
  if Key = Ord ('X') then LogMemo.Lines.Clear;
end;

procedure TLogForm.pClearClick(Sender: TObject);
begin
  LogMemo.Lines.Clear;
end;

procedure TLogForm.pCopyAllClick(Sender: TObject);
begin
  LogMemo.SelectAll;
  LogMemo.CopyToClipboard;
end;

procedure TLogForm.pCopyClick(Sender: TObject);
begin
  LogMemo.CopyToClipboard;
end;

//-- BG ---------------------------------------------------------- 25.05.2016 --
procedure TLogForm.SetLogIt(Index: ThtLogSubject; const Value: Boolean);
begin
  if Value then
    Include(FLogIt, Index)
  else
    Exclude(FLogIt, Index);
end;

//initialization
//  Lock := TCriticalSection.Create;
//  Strings := TStringList.Create;
//finalization
//  Strings.Free;
//  Lock.Free;
end.
