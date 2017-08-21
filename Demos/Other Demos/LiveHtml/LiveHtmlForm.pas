unit LiveHtmlForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, Menus, ClipBrd,
  //
  HtmlGlobals, HTMLUn2, HtmlView, SynEditHighlighter, SynHighlighterHtml, SynEdit, SynEditOptionsDialog, HtmlBuffer;

type
  TFormLiveHtml = class(TForm)
    HtmlViewer: THtmlViewer;
    Splitter: TSplitter;
    PopupMenu: TPopupMenu;
    pmSelectAll: TMenuItem;
    pmCopy: TMenuItem;
    pmPaste: TMenuItem;
    N1: TMenuItem;
    NameList1: TMenuItem;
    Memo: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynEditOptionsDialog1: TSynEditOptionsDialog;
    PopupMenu1: TPopupMenu;
    Options1: TMenuItem;
    DocumentSource1: TMenuItem;
    TestAsString1: TMenuItem;
    procedure MemoChange(Sender: TObject);
{$ifdef UNICODE}
    procedure HtmlViewerObjectClick(Sender, Obj: TObject; OnClick: string);
{$else}
    procedure HtmlViewerObjectClick(Sender, Obj: TObject; OnClick: WideString);
{$endif}
    procedure pmSelectAllClick(Sender: TObject);
    procedure pmCopyClick(Sender: TObject);
    procedure pmPasteClick(Sender: TObject);
    procedure NameList1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure DocumentSource1Click(Sender: TObject);
    procedure HtmlViewerParseBegin(Sender: TObject; var Source: TBuffer);
  protected
    procedure Loaded; override;
  end;

var
  FormLiveHtml: TFormLiveHtml;

implementation

{$R *.dfm}

procedure TFormLiveHtml.DocumentSource1Click(Sender: TObject);
begin
  Memo.Lines.Add(HtmlViewer.Text);
//  Memo.Lines.SaveToFile(ChangeFileExt(Application.ExeName, '.log'));
end;

procedure TFormLiveHtml.HtmlViewerObjectClick(Sender, Obj: TObject; OnClick: ThtString);
begin
  HtmlViewer.Clear;
end;

var Str: ThtString;
procedure TFormLiveHtml.HtmlViewerParseBegin(Sender: TObject; var Source: TBuffer);
begin
  if TestAsString1.Checked then
  begin
    Str := Source.AsString;
  end;
end;

procedure TFormLiveHtml.Loaded;
begin
  inherited;
  HtmlViewer.Text := 'Type or paste <u>html text</u> into the <b>field below</b> and see the results in <i>this HtmlViewer</i>.';
  Memo.Lines.Text := 'Type or paste html text into this field and see the results in the above HtmlViewer.';
  Memo.SelectAll;
  Caption := 'HtmlViewer ' + VersionNo + ' Live';
end;

//-- BG ---------------------------------------------------------- 21.10.2012 --
procedure TFormLiveHtml.MemoChange(Sender: TObject);
begin
  HtmlViewer.Text := Memo.Text;
end;

//-- BG ---------------------------------------------------------- 12.09.2012 --
procedure TFormLiveHtml.NameList1Click(Sender: TObject);
var
  Infos: TStringList;
  Names: TIDObjectList;
  I: Integer;
begin
  // simply get the names
  //Clipboard.AsText := HtmlViewer.NameList.Text;

  // get the names and the actual types of the associated TIDObject derivates.
  Infos := TStringList.Create;
  try
    Names := HtmlViewer.SectionList.IDNameList;
    for I := 0 to Names.Count - 1 do
      Infos.Add(Names.Strings[I] + ' (' + Names.Objects[I].ClassName + ')');
    Clipboard.AsText := Infos.Text;
  finally
    Infos.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 21.10.2012 --
procedure TFormLiveHtml.Options1Click(Sender: TObject);
var
  Options: TSynEditorOptionsContainer;
begin
  Options := TSynEditorOptionsContainer.Create(Self);
  try
  Options.Assign(Memo);
    if SynEditOptionsDialog1.Execute(Options) then
      Options.AssignTo(Memo);
  finally
    Options.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 03.06.2012 --
procedure TFormLiveHtml.pmCopyClick(Sender: TObject);
begin
  HtmlViewer.CopyToClipboard;
end;

//-- BG ---------------------------------------------------------- 03.06.2012 --
procedure TFormLiveHtml.pmPasteClick(Sender: TObject);
begin
  Memo.PasteFromClipboard;
end;

//-- BG ---------------------------------------------------------- 03.06.2012 --
procedure TFormLiveHtml.pmSelectAllClick(Sender: TObject);
begin
  HtmlViewer.SelectAll;
end;

end.
