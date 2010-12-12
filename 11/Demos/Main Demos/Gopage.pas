unit Gopage;

interface

uses
  Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls,
{$ifdef LCL}
  LResources,
{$else}
{$endif}
  Spin;

type
  TGoPageForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Bevel1: TBevel;
    PageNum: TSpinEdit;
    procedure PageNumEnter(Sender: TObject);
    procedure PageNumKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GoPageForm: TGoPageForm;

implementation

{$ifdef LCL}
{$else}
{$R *.DFM}
{$endif}

procedure TGoPageForm.PageNumEnter(Sender: TObject);
begin
PageNum.SelectAll;
end;

procedure TGoPageForm.PageNumKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if Key = 13 then
  Begin
  Key := 0;
  OKBtn.Click;
  end;
end;

initialization
{$ifdef LCL}
{$include GoPage.lrs}
{$else}
{$endif}
end.
