unit InfoDlg;

interface
{$include htmlcons.inc}
{$include options.inc}
uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TInfoForm = class(TForm)
    Label1: TLabel;
    OKBurron: TBitBtn;
    mmoInfo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.DFM}

end.
