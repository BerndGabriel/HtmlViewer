{-------------------------------------------------------------------------------
HtmlViewer6TestForm.pas, (C) 03/2011 by Bernd Gabriel, all rights reserved

mailto: info@fast-function-factory.de
-------------------------------------------------------------------------------}
unit HtmlViewer6TestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls,

  // shared units
  TntDialogs,

  // own units
  HtmlBoxes,
  HtmlBuffer,
  HtmlDocument,
  HtmlDraw,
  HtmlElements,
  HtmlGlobals,
  HtmlImages,
  HtmlParser,
  HtmlSymbols,
  HtmlViewer,
  StyleTypes,
  UrlSubs, ExtCtrls;

type
  TFormHtmlViewer12Test = class(TForm)
    dlgFileOpen: TTntOpenDialog;
    menu: TMainMenu;
    menuFile: TMenuItem;
    menuFileOpen: TMenuItem;
    HtmlViewer: THtmlViewer12;
    PageControl: TPageControl;
    HtmlTab: TTabSheet;
    CssTab: TTabSheet;
    CssMemo: TMemo;
    BegaSplitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuFileOpenClick(Sender: TObject);
  private
    FName: ThtString;
    FStream: TStream;
    FBuffer: TBuffer;
    FDocument: THtmlDocument;
    FParser: THtmlParser;
    FView: THtmlBox;
    FImageCache: ThtImageCache;
  public
    procedure Load(FileName: ThtString);
  end;

var
  FormHtmlViewer12Test: TFormHtmlViewer12Test;

implementation

{$R *.dfm}

{ TFormHtmlViewer12Test }

//-- BG ---------------------------------------------------------- 28.03.2011 --
procedure TFormHtmlViewer12Test.menuFileOpenClick(Sender: TObject);
begin
  dlgFileOpen.FileName := FName;
  if dlgFileOpen.Execute then
    Load(dlgFileOpen.FileName);
end;


//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TFormHtmlViewer12Test.FormCreate(Sender: TObject);
var
  Image: ThtImage;
  ImageName: string;
  ImageIndex: Integer;
begin
  FImageCache := ThtImageCache.Create;
  ImageName := 'pengbrew.png';
  Image := LoadImageFromFile(ImageName, TrPng);
  if Image <> nil then
    FImageCache.AddObject(ImageName, Image);

  FView := THtmlBodyBox.Create(nil, THtmlBodyControl.Create(HtmlViewer));
  FView.BoundsRect := Rect(4, 8, 404, 308);
  FView.Margins := RectIntegers(16, 22, 4, 8);
  FView.BorderWidths := RectIntegers(4, 8, 16, 0);
  FView.Padding := RectIntegers(6, 2, 4, 10);
  FView.BorderColors := RectColors(clRed, clAqua, clGreen, clOlive);
  FView.BorderStyles := RectStyles(bssSolid, bssDashed, bssDouble, bssGroove);
  FView.Color := clNone;
  FView.Text := 'HtmlViewer ' + ThtChar(8805) + ' 12';
  FView.Alignment := taCenter;
  FView.Font.Style := [fsItalic];
  FView.Font.Color := clCaptionText;
  FView.Font.Size := 16;
  ImageIndex := FImageCache.IndexOf(ImageName);
  if ImageIndex >= 0 then
  begin
    FView.Image := FImageCache.GetImage(ImageIndex);
    FView.Image.EndUse; // Both FView and GetImage have started a use, but one is required only.
  end
  else
    FView.Image := ErrorImage;
  FView.Tiled := True;
  FView.TileWidth := 100;
  FView.TileHeight := 100;
  HtmlViewer.AddView(FView);
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TFormHtmlViewer12Test.FormDestroy(Sender: TObject);
begin
  FView.Image := nil;
  FView.Free;
  FImageCache.Free;
  Load('');
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.Load(FileName: ThtString);
begin
  CssMemo.Clear;
  FreeAndNil(FParser);
  FreeAndNil(FDocument);
  FreeAndNil(FBuffer);
  FreeAndNil(FStream);
  FName := FileName;
  if Length(FName) > 0 then
  begin
    FStream := TFileStream.Create(FName, fmOpenRead + fmShareDenyNone);
    FBuffer := TBuffer.Create(FStream, 'file://' + DosToHtml(FName));
    FDocument := THtmlDocument.Create;
    FParser := THtmlParser.Create(FBuffer);
    FParser.ParseHtmlDocument(FDocument);
    CssMemo.Lines.Text := FDocument.RuleSets.ToString;
  end;
end;

end.
