unit AuthUnit;

{This module handles getting Usernames and Passwords}

interface

uses
  WinTypes, WinProcs,  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, Buttons;

type
  TAuthForm = class(TForm)
    AuthUsername: TEdit;
    AuthPassword: TEdit;
    OKBtn: TBitBtn;
    BitBtn2: TBitBtn;
    Label2: TLabel;
    Label1: TLabel;
    LabelRealm: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    RealmList: TStringList;
  public
    { Public declarations }
    function GetAuthorization(TryRealm: boolean; const Realm: string;
                 var UName, PWord: string): boolean;
  end;

var
  AuthForm: TAuthForm;

implementation

{$R *.DFM}

type
  PTwoStrings = ^TwoStrings;
  TwoStrings = record
    UN, PW: string;
    end;

procedure TAuthForm.FormShow(Sender: TObject);
begin
AuthUsername.Text := '';
AuthPassword.Text := '';
AuthUsername.SetFocus;
end;

procedure TAuthForm.OKBtnClick(Sender: TObject);
begin
if (AuthUsername.Text = '') or (AuthPassword.Text = '') then
  begin
  ShowMessage('Entry required for both Username and Password');
  ModalResult := mrNone;
  end
else ModalResult := mrOK;
end;

procedure TAuthForm.FormCreate(Sender: TObject);
begin
RealmList := TStringList.Create;
RealmList.Sorted := True;
end;

procedure TAuthForm.FormDestroy(Sender: TObject);
var
  I: integer;
begin
for I := 0 to RealmList.Count-1 do
  Dispose(PTwoStrings(RealmList.Objects[I]));
RealmList.Free;
end;

function TAuthForm.GetAuthorization(TryRealm: boolean; const Realm: string;
  var UName, PWord: string): boolean;
{TryRealm is only set for one try at most}
var
  I: integer;
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
  I := Pos ('=', Realm) ;
  if I > 0 then   // ANGUS tell user what realm 
    LabelRealm.Caption := Copy (Realm, I + 1, 999)
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
