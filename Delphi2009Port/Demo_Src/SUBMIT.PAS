unit Submit;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls;

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

var
  SubmitForm: TSubmitForm;

implementation

{$R *.DFM}

procedure TSubmitForm.Button1Click(Sender: TObject);
begin
Close;
end;

end.
