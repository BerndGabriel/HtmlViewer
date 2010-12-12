unit Submit;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls
{$ifdef LCL}
  , LResources
{$endif}
  ;

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

{$ifdef LCL}
{$else}
{$R *.DFM}
{$endif}

procedure TSubmitForm.Button1Click(Sender: TObject);
begin
Close;
end;

{$ifdef LCL}
initialization
{$I Submit.lrs}
{$else}
{$endif}
end.
