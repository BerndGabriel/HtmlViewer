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
  TPopupMenu=class(Vcl.ActnPopup.TPopupActionBar);
  {$endif}
  TLogForm = class(TForm)
    LogMemo: TMemo;
    PopupMenu: TPopupMenu;
    pCopy: TMenuItem;
    pClear: TMenuItem;
    pCopyAll: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pClearClick(Sender: TObject);
    procedure pCopyAllClick(Sender: TObject);
    procedure pCopyClick(Sender: TObject);
    procedure LogMemoDblClick(Sender: TObject);
    procedure LogMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LogForm: TLogForm;

implementation
uses FBUnitId10;

{$R *.dfm}

procedure TLogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TLogForm.FormCreate(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(HTTPForm.FIniFileName);
  try
    Top := IniFile.ReadInteger('LogForm', 'Top', Top);
    Left := IniFile.ReadInteger('LogForm', 'Left',   Left);
    Width := IniFile.ReadInteger('LogForm', 'Width',  Width);
    Height := IniFile.ReadInteger('LogForm', 'Height', Height);
  finally
    IniFile.Free;
    end;
end;

procedure TLogForm.FormDestroy(Sender: TObject);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(HTTPForm.FIniFileName);
  try
    IniFile.WriteInteger('LogForm', 'Top', Top);
    IniFile.WriteInteger('LogForm', 'Left', Left);
    IniFile.WriteInteger('LogForm', 'Width', Width);
    IniFile.WriteInteger('LogForm', 'Height', Height);
  finally
    IniFile.Free;
    end;
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
