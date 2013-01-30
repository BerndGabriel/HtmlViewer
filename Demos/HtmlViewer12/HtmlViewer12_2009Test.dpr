program HtmlViewer12_2009Test;

uses
  Forms,
  BegaObjects in 'BegaObjects.pas',
  BegaComboBox in 'BegaComboBox.pas',
  BegaSplitter in 'BegaSplitter.pas',
  BegaVirtualTrees in 'BegaVirtualTrees.pas',
  HtmlViewer12_2009TestForm in 'HtmlViewer12_2009TestForm.pas' {FormHtmlViewer6Test};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHtmlViewer12Test, FormHtmlViewer12Test);
  Application.Run;
end.
