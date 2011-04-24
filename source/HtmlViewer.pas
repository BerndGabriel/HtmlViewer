{
Version   12
Copyright (c) 2011 by Bernd Gabriel

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

{$I htmlcons.inc}

unit HtmlViewer;

interface

uses
  Windows, Graphics, Messages, SysUtils, Classes, Controls, ExtCtrls,
  //
  HtmlBoxes,
  HtmlDocument,
  HtmlElements;

type

  THtmlViewerOptions = set of (
    voOwnsDocument
  );

  THtmlViewerState = set of (
    vsDocumentChanged,
    vsViewChanged
  );

  TCustomHtmlViewer = class(TWinControl)
  private
    FState: THtmlViewerState;
    FOptions: THtmlViewerOptions;
    FDocument: THtmlDocument;
    FView: THtmlBox;
    procedure SetDocument(const Value: THtmlDocument);
    procedure SetViewerOptions(const Value: THtmlViewerOptions);
    procedure SetViewerState(const Value: THtmlViewerState);
    procedure SetView(const Value: THtmlBox);
  protected
    procedure UpdateDocument;
    procedure UpdateView;
    procedure Resize; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    property HtmlDocument: THtmlDocument read FDocument write SetDocument;
    property HtmlView: THtmlBox read FView write SetView;
    property ViewerOptions: THtmlViewerOptions read FOptions write SetViewerOptions;
    property ViewerState: THtmlViewerState read FState write SetViewerState;
  end;

  THtmlViewer12 = class(TCustomHtmlViewer)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ViewerOptions;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HtmlViewer', [THtmlViewer12]);
end;

{ TCustomHtmlViewer }

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.Resize;
begin
  inherited;
  if FView <> nil then
    FView.Resized;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.SetDocument(const Value: THtmlDocument);
begin
  if FDocument <> Value then
  begin
    FDocument := Value;
    Include(FState, vsDocumentChanged);
    Invalidate;
  end;
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TCustomHtmlViewer.SetView(const Value: THtmlBox);
begin
  if FView <> Value then
  begin
    FView := Value;
    Include(FState, vsViewChanged);
    Invalidate;
  end;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.SetViewerOptions(const Value: THtmlViewerOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Include(FState, vsViewChanged);
    Invalidate;
  end;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.SetViewerState(const Value: THtmlViewerState);
begin
  if FState <> Value then
  begin
    FState := Value;
    Invalidate;
  end;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.UpdateDocument;
// rebuild internal representation after document/structure has changed.
begin
  if vsDocumentChanged in FState then
  begin
    Exclude(FState, vsDocumentChanged);
    //TODO -1 -oBG, 04.04.2011: get (new) display structure from document

    Include(FState, vsViewChanged);
  end;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.UpdateView;
// update internal representation after visually relevant parameters have changed.
begin
  if vsViewChanged in FState then
  begin
    Exclude(FState, vsViewChanged);
    //TODO -1 -oBG, 04.04.2011: update display structure after any visually relevant changes except document changes.

  end;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if csDesigning in ComponentState then
    inherited
  else
    Message.Result := 1;
end;

end.
