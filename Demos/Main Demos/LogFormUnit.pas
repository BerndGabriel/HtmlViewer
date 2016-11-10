{
Version   11.7
Copyright (c) 2008-2016 by HtmlViewer Team

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
unit LogFormUnit;

interface
{$include htmlcons.inc}
uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  WinTypes, WinProcs, Messages,
{$endif}
  //Windows,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$ifdef UseVCLStyles}
  Vcl.Styles,
  Vcl.Themes,
  Vcl.ActnPopup,
{$endif}
  Dialogs, StdCtrls, IniFiles, Menus, SyncObjs;

type
  ThtLogSubject = (laDiag, laHttpHeader, laHttpScript, laJavaScript);
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
    constructor Create(AOwner: TComponent; ATop, ALeft, AWidth, AHeight: Integer); reintroduce;
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

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

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
