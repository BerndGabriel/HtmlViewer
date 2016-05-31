unit InfoFormUnit;

{$include htmlcons.inc}

interface

uses
  {$ifdef LCL}
    LCLIntf, LCLType, LMessages,
  {$else}
    WinTypes, WinProcs, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    OKButton: TBitBtn;
    mmoInfo: TMemo;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TInfoForm.FormResize(Sender: TObject);
begin
  OKButton.Left := (ClientWidth - OKButton.Width) div 2;
end;

end.
