{
Version   11.7
Copyright (c) 1995-2008 by L. David Baldwin
Copyright (c) 2008-2015 by HtmlViewer Team

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Note that the source modules HTMLGIF1.PAS and DITHERUNIT.PAS
are covered by separate copyright notices located in those modules.
}

unit ImgForm;

{$include ..\..\source\htmlcons.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Math,
  HtmlImages;

type
  TImageForm = class(TForm)
  published
    Image1: TImage;
  private
    procedure SetGraphic(const Value: TGraphic);
    procedure SetImage(const Value: ThtImage);
  public
    property Graphic: TGraphic write SetGraphic;
    property Image: ThtImage write SetImage;
  end;

implementation

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}

procedure TImageForm.SetGraphic(const Value: TGraphic);
var
  OldWidth, OldHeight: Integer;
begin
  OldWidth  := Image1.Width;
  OldHeight := Image1.Height;
  Image1.Picture.Graphic := Value;
  Left := Max(Left + (OldWidth - Image1.Width) div 2, 0);
  Top  := Max(Top + (OldHeight - Image1.Height) div 2, 0);
end;

procedure TImageForm.SetImage(const Value: ThtImage);
begin
  if Value <> nil then
  begin
    SetGraphic(Value.Graphic);
    Image1.Transparent := Value.Transp <> itrNone;
  end
  else
    SetGraphic(nil);
end;

end.
