unit SUBMIT;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TSubmitForm = class(TForm)
    ActionText: TEdit;
    MethodText: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ResultBox: TListBox;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function SubmitForm: TSubmitForm;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

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
