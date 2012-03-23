unit ProxyDlg;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TProxyForm = class(TForm)
    ProxyEdit: TEdit;
    PortEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OKBurron: TBitBtn;
    CancelButton: TBitBtn;
    ProxyUsername: TEdit;
    ProxyPassword: TEdit;
    Label3: TLabel;
    Label4: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ProxyForm: TProxyForm;

implementation

{$R *.DFM}

end.
