unit SubmitTNT;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, TntStdCtrls
{$ifdef LCL}

{$endif}
  ;

type
  TSubmitForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    ResultBox: TTntListBox;
    ActionText: TTntEdit;
    MethodText: TTntEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SubmitForm: TSubmitForm;

implementation

{$R *.dfm}

var
  VForm: TSubmitForm;

function SubmitForm: TSubmitForm;
begin
  if VForm = nil then
    VForm := TSubmitForm.Create(Application);
  Result := VForm;
end;

procedure TSubmitForm.Button1Click(Sender: TObject);
begin
Close;
end;

end.
