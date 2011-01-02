unit ImgForm;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Math;

type
  TImageForm = class(TForm)
  published
    Image1: TImage;
  private
    procedure setBitmap(const Value: TBitmap);
  public
    property Bitmap: TBitmap write setBitmap;
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TImageForm.setBitmap(const Value: TBitmap);
var
  OldWidth, OldHeight: Integer;
begin
  OldWidth  := Image1.Width;
  OldHeight := Image1.Height;
  Image1.Picture.Bitmap := Value;
  Left := Max(Left + (OldWidth - Image1.Width) div 2, 0);
  Top  := Max(Top + (OldHeight - Image1.Height) div 2, 0);
end;

end.
