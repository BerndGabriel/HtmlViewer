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
  Dialogs, StdCtrls, IniFiles, Menus;

type
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
    FLogIt: Boolean;
  public
    constructor Create(AOwner: TComponent; ATop, ALeft, AWidth, AHeight: Integer);
    procedure Log(const AString: string); overload;
    procedure Log(const AStrings: TStrings); overload;
    property LogActive: Boolean read FLogIt write FLogIt;
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
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.Log(const AString: string);
begin
  if FLogIt then
  begin
    LogMemo.Lines.Add(AString);
    Visible := True;
  end;
end;

//-- BG ---------------------------------------------------------- 23.05.2016 --
procedure TLogForm.Log(const AStrings: TStrings);
var
  I : Integer;
begin
  if FLogIt then
    for I := 0 to AStrings.Count -1 do
      Log(AStrings[I]);
end;

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
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

end.
