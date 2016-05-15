unit InfoDlg;

{$include htmlcons.inc}
{$include options.inc}

interface

uses
  {$IFnDEF FPC}
    WinTypes, WinProcs, Messages,
  {$ELSE}
    LCLIntf, LCLType,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

end.
