{-------------------------------------------------------------------------------
HtmlViewer12TestForm.pas, (C) 03/2011 by Bernd Gabriel, all rights reserved

mailto: info@fast-function-factory.de
-------------------------------------------------------------------------------}
unit HtmlViewer12_2009TestForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls,

  // shared units
  VirtualTrees,

  // fast function factory units
  BegaComboBox,
  BegaSplitter,
  BegaVirtualTrees,

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
    cbFiles: TBegaCombobox;
    dlgFileOpen: TOpenDialog;
    menu: TMainMenu;
    menuFile: TMenuItem;
    menuFileOpen: TMenuItem;
    HtmlViewer: THtmlViewer12;
    PageControl: TPageControl;
    HtmlTab: TTabSheet;
    CssTab: TTabSheet;
    vtDocument: TBegaVirtualStringTree;
    CssMemo: TMemo;
    BegaSplitter1: TSplitter;
    procedure cbFilesKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure menuFileOpenClick(Sender: TObject);
    procedure vtDocumentGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtDocumentInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vtDocumentInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FName: ThtString;
    FStream: TStream;
    FBuffer: TBuffer;
    FDocument: THtmlDocument;
    FParser: THtmlParser;
    FView: THtmlBox;
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
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
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

  // simple test example with parsing a document:
  procedure InitTest;
  begin
    Load('HtmlViewer12Test.html');
  end;

begin
  InitTest;
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TFormHtmlViewer12Test.FormDestroy(Sender: TObject);
begin
  if FView <> nil then
  begin
    FView.Image := nil;
    FView.Free;
  end;
  Load('');
end;

var
  Uri: TUri;
  Filename: ThtString;
  Filenames: ThtStringList;

//-- BG ---------------------------------------------------------- 30.03.2011 --
procedure TFormHtmlViewer12Test.Load(FileName: ThtString);
begin
  vtDocument.RootNodeCount := 0;
  CssMemo.Clear;
  HtmlViewer.HtmlDocument := nil;
  FreeAndNil(FParser);
  FreeAndNil(FDocument);
  FreeAndNil(FBuffer);
  FreeAndNil(FStream);
  FName := FileName;
  if Length(FName) > 0 then
  begin
    FStream := TFileStream.Create(FName, fmOpenRead + fmShareDenyNone);
    FBuffer := TBuffer.Create(FStream, 'file:///' + DosToHtml(FName));
    FDocument := THtmlDocument.Create;
    FParser := THtmlParser.Create(FBuffer);
    FParser.ParseHtmlDocument(FDocument);
    if FDocument.Tree <> nil then
      vtDocument.RootNodeCount := 1;
    CssMemo.Lines.Text := FDocument.RuleSets.ToString;
    HtmlViewer.HtmlDocument := FDocument;
    if cbFiles.Items.IndexOf(FileName) < 0 then
      cbFiles.Items.Insert(0, FileName);
  end;
end;

initialization
  Filenames := ThtStringList.Create;
finalization
  Filenames.Free;
end.
