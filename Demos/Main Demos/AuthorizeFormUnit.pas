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
unit AuthorizeFormUnit;

{$include htmlcons.inc}

{This module handles getting Usernames and Passwords}

interface

uses
{$ifdef LCL}
  LCLIntf, LCLType, LMessages,
{$else}
  WinTypes, WinProcs,
{$endif}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type

  { TAuthForm }

  TAuthorizeForm = class(TForm)
    AuthUsername: TEdit;
    AuthPassword: TEdit;
    OKBtn: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    LabelRealm: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    RealmList: TStringList;
  public
    class function Execute(TryRealm: Boolean; const Realm: string; var UName, PWord: string): Boolean;

    { Public declarations }
    function GetAuthorization(TryRealm: boolean; const Realm: string; var UName, PWord: string): boolean;
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

type
  PTwoStrings = ^TwoStrings;
  TwoStrings = record
    UN, PW: string;
  end;

var
  AuthorizeForm: TAuthorizeForm;

//-- BG ---------------------------------------------------------- 31.05.2016 --
class function TAuthorizeForm.Execute(TryRealm: Boolean; const Realm: string; var UName, PWord: string): Boolean;
begin
  if AuthorizeForm = nil then
    AuthorizeForm := TAuthorizeForm.Create(Application);
  Result := AuthorizeForm.GetAuthorization(TryRealm, Realm, UName, PWord);
end;

procedure TAuthorizeForm.FormShow(Sender: TObject);
begin
  AuthUsername.Text := '';
  AuthPassword.Text := '';
  AuthUsername.SetFocus;
end;

procedure TAuthorizeForm.OKBtnClick(Sender: TObject);
begin
  if (AuthUsername.Text = '') or (AuthPassword.Text = '') then
  begin
    ShowMessage('Entry required for both Username and Password');
    ModalResult := mrNone;
  end
  else
    ModalResult := mrOK;
end;

procedure TAuthorizeForm.FormCreate(Sender: TObject);
begin
  RealmList := TStringList.Create;
  RealmList.Sorted := True;
end;

procedure TAuthorizeForm.FormDestroy(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to RealmList.Count-1 do
    Dispose(PTwoStrings(RealmList.Objects[I]));
  RealmList.Free;
end;

function TAuthorizeForm.GetAuthorization(TryRealm: Boolean; const Realm: string; var UName, PWord: string): Boolean;
{TryRealm is only set for one try at most}
var
  I: Integer;
  P: PTwoStrings;
begin
  Result := False;
  if TryRealm then
  begin
    Result :=  RealmList.Find(Realm, I); {see if the password has already been saved for this Realm}
    if Result then
    begin
      UName := PTwoStrings(RealmList.Objects[I])^.UN;
      PWord := PTwoStrings(RealmList.Objects[I])^.PW;
    end;
  end;

  if not Result then
  begin
    I := Pos('=', Realm);
    if I > 0 then   // ANGUS: tell user what realm
      LabelRealm.Caption := Copy(Realm, I + 1, MaxInt)
    else
      LabelRealm.Caption := Realm;

    Result := ShowModal = mrOK;  {Use dialog to get info}
    if Result then
    begin
      UName := AuthUsername.Text;
      PWord := AuthPassword.Text;
      if RealmList.Find(Realm, I) then  {get rid of info for this realm that didn't work}
      begin
        Dispose(PTwoStrings(RealmList.Objects[I]));
        RealmList.Delete(I);
      end;
      New(P);
      P^.UN := UName;
      P^.PW := PWord;
      RealmList.AddObject(Realm, TObject(P));
    end;
  end;
end;

end.
