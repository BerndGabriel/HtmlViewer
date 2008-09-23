unit ImgForm;

interface

uses
  Wintypes, Winprocs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TImageForm = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ImageFormBitmap: TBitmap;
  end;

var
  ImageForm: TImageForm;

implementation

{$R *.DFM}

procedure TImageForm.FormShow(Sender: TObject);
begin
Image1.Picture.Bitmap := ImageFormBitmap;
Width := Image1.Width + 30;  {makes for better fit}
ClientHeight := Image1.Height;
ClientWidth := Image1.Width;
end;

end.
