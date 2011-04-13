{-------------------------------------------------------------------------------
HtmlViewer12TestForm.pas, (C) 03/2011 by Bernd Gabriel, all rights reserved

mailto: info@fast-function-factory.de
-------------------------------------------------------------------------------}
unit HtmlViewer12TestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,

  // shared units
  TntDialogs,
  VirtualTrees,

  // fast function factory units
  BegaComboBox,
  BegaSplitter,
  BegaVirtualTrees,

  // own units
  HtmlGlobals,
  HtmlBuffer,
  HtmlSymbols,
  HtmlDocument,
  HtmlDraw,
  HtmlImages,
  HtmlTree,
  HtmlParser,
  HtmlViewer;

type
  TFormHtmlViewer12Test = class(TForm)
    cbFiles: TBegaCombobox;
    dlgFileOpen: TTntOpenDialog;
    menu: TMainMenu;
    menuFile: TMenuItem;
    menuFileOpen: TMenuItem;
    vtDocument: TBegaVirtualStringTree;
    BegaSplitter1: TBegaSplitter;
    HtmlViewer: THtmlViewer12;
    procedure cbFilesKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuFileOpenClick(Sender: TObject);
    procedure vtDocumentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vtDocumentInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtDocumentInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FName: ThtString;
    FStream: TStream;
    FBuffer: TBuffer;
    FDocument: THtmlDocument;
    FParser: THtmlParser;
    FView: THtmlView;
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
  dlgFileOpen.FileName := cbFiles.Text;
  if dlgFileOpen.Execute then
  begin
    Load(dlgFileOpen.FileName);
    cbFiles.AddItem(FName, nil);
  end;
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.vtDocumentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  Elem: THtmlElement;
  Ed: THtmlElementDescription;
begin
  Elem := THtmlElement(vtDocument.Objects[Node]);
  case Column of
    1: CellText := Implode(Elem.Classes, ' ');
    2: CellText := Implode(Elem.Ids, ' ');
    3: CellText := IntToStr(Elem.DocPos);
    4: CellText := Elem.StyleProperties.ToString;
    5: CellText := Elem.AttributeProperties.ToString;
    6: CellText := Elem.OtherAttributes.ToString;
  else
    Ed := ElementSymbolToElementDescription(Elem.Symbol);
    CellText := Ed.Name;
  end;
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.vtDocumentInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  Child: THtmlElement;
begin
  ChildCount := 0;
  Child := THtmlElement(vtDocument.Objects[Node]).FirstChild;
  while Child <> nil do
  begin
    Inc(ChildCount);
    Child := Child.Next;
  end;
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.vtDocumentInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);

  function GetChild(Element: THtmlElement; Index: Integer): THtmlElement;
  begin
    Result := Element.FirstChild;
    while (Index > 0) and (Result <> nil) do
    begin
      Dec(Index);
      Result := Result.Next;
    end;
  end;

var
  Element: THtmlElement;
begin
  Include(InitialStates, ivsExpanded);
  if ParentNode = nil then
  begin
    vtDocument.Objects[Node] := FDocument.Tree;
    if FDocument.Tree.FirstChild <> nil then
      Include(InitialStates, ivsHasChildren);
  end
  else
  begin
    Element := GetChild(THtmlElement(vtDocument.Objects[ParentNode]), Node.Index);
    vtDocument.Objects[Node] := Element;
    if Element.FirstChild <> nil then
      Include(InitialStates, ivsHasChildren);
  end;
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.cbFilesKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13:
    begin
      Load(cbFiles.Text);
      Key := #0;
    end;
  end;
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

  FView := THtmlView.Create;
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
    FView.Image := FImageCache.GetImage(ImageIndex)
  else
    FView.Image := ErrorImage;
  FView.Tiled := True;
  FView.TileWidth := 100;
  FView.TileHeight := 100;
  HtmlViewer.HtmlView := FView;

end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TFormHtmlViewer12Test.FormDestroy(Sender: TObject);
begin
  FImageCache.Free;
  FView.Free;
end;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.Load(FileName: ThtString);
begin
  vtDocument.RootNodeCount := 0;
  FreeAndNil(FParser);
  FreeAndNil(FDocument);
  FreeAndNil(FBuffer);
  FreeAndNil(FStream);
  FName := FileName;
  FStream := TFileStream.Create(FName, fmOpenRead + fmShareDenyWrite);
  FBuffer := TBuffer.Create(FStream, FName);
  FDocument := THtmlDocument.Create;
  FParser := THtmlParser.Create(FBuffer);
  FParser.ParseHtmlDocument(FDocument);
  if FDocument.Tree <> nil then
    vtDocument.RootNodeCount := 1;
end;

end.
