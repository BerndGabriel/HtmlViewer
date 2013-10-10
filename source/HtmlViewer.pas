{
Version   12
Copyright (c) 2011-2012 by Bernd Gabriel

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
  HtmlElements,
  HtmlFonts,
  HtmlImages,
  HtmlRenderer,
  StyleTypes;

type

//------------------------------------------------------------------------------
// TCustomHtmlViewer is base class for THtmlViewer 12
//------------------------------------------------------------------------------
// It is the hood of both framed and single HTML document visualizations.
//------------------------------------------------------------------------------

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
    FControlMap: THtmlControlOfElementMap; // remember the controls per element
    FImageCache: ThtImageCache;
    FView: THtmlBox; // the hook for the boxes to show.
    FScale: Double;
    procedure SetDocument(const Value: THtmlDocument);
    procedure SetViewerOptions(const Value: THtmlViewerOptions);
    procedure SetViewerState(const Value: THtmlViewerState);
    procedure SetScale(const Value: Double);
    procedure ScrollControlScrolled(Sender: TObject);
  protected
    procedure UpdateDocument;
    procedure UpdateView;
    procedure Resize; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure AddView(const Value: THtmlBox);
    procedure ClearView;
    property HtmlDocument: THtmlDocument read FDocument write SetDocument;
    property HtmlView: THtmlBox read FView;
    property Scale: Double read FScale write SetScale;
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

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure TCustomHtmlViewer.AddView(const Value: THtmlBox);
begin
  FView.Children.Add(Value);
  Include(FState, vsViewChanged);
  Invalidate;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure TCustomHtmlViewer.AfterConstruction;
begin
  inherited;
  FControlMap := THtmlControlOfElementMap.Create;
  FImageCache := ThtImageCache.Create;
  FView := THtmlBox.Create;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure TCustomHtmlViewer.BeforeDestruction;
begin
  SetDocument(nil);
  FView.Free;
  FControlMap.Free;
  FImageCache.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure TCustomHtmlViewer.ClearView;
begin
  FView.Element := nil;
  FView.Children.Clear;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.Resize;
begin
  inherited;
  Include(FState, vsDocumentChanged);
  UpdateDocument;
end;

//-- BG ---------------------------------------------------------- 10.10.2013 --
procedure TCustomHtmlViewer.ScrollControlScrolled(Sender: TObject);
begin
  Include(FState, vsDocumentChanged);
  UpdateDocument;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.SetDocument(const Value: THtmlDocument);
begin
  if FDocument <> Value then
  begin
    if FDocument <> nil then
      if voOwnsDocument in FOptions then
        FDocument.Free;
    FControlMap.Clear;
    FDocument := Value;
    Include(FState, vsDocumentChanged);
    UpdateDocument;
    UpdateView;
    Invalidate;
  end;
end;

//-- BG ---------------------------------------------------------- 25.04.2011 --
procedure TCustomHtmlViewer.SetScale(const Value: Double);
begin
  if FScale <> Value then
  begin
    FScale := Value;
    FView.Rescaled(Value);
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
// Rebuild internal representation after document/structure or client rect has changed.
var
  DefaultFont: ThtFont;
  Renderer: THtmlVisualRenderer;
  I: Integer;
  Ctrl: TControl;
begin
  if vsDocumentChanged in FState then
  try
    FView.Children.Clear;
    if FDocument <> nil then
    begin                                                                       //TODO -1 -oBG, 02.05.2011:  Defaultfont parameter
      FView.BoundsRect := ClientRect;
      
      // setting bounds rect may have called Resize and thus has updated the document already.
      if vsDocumentChanged in FState then
      begin
        // render document
        DefaultFont := ThtFont.Create;
        try
          DefaultFont.Assign(Font);
          DefaultFont.bgColor := Color;
          Renderer := THtmlVisualRenderer.Create(Self, FDocument, FControlMap, FImageCache, mtScreen, DefaultFont, ClientWidth, ClientHeight);
          try
            Renderer.MediaCapabilities := [mcFrames];
            Renderer.RenderDocument(FView);
          finally
            Include(FState, vsViewChanged);
            Renderer.Free;
          end;
        finally
          DefaultFont.Free;
        end;

        // set event handlers
        for I := 0 to FControlMap.Count - 1 do
        begin
          Ctrl := FControlMap.Controls[I];

          // set scroll handler
          if Ctrl is THtmlScrollControl then
            THtmlScrollControl(Ctrl).OnScrolled := ScrollControlScrolled;
        end;
      end;
    end;
  finally
    Exclude(FState, vsDocumentChanged);
  end;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure TCustomHtmlViewer.UpdateView;
// Update internal representation after visually relevant parameters have changed.
begin
  if vsViewChanged in FState then
  begin
    Exclude(FState, vsViewChanged);
    //TODO -1 -oBG, 04.04.2011: update display structure after any visually relevant changes except document changes.
    UpdateDocument;
  end;
end;

//-- BG ---------------------------------------------------------- 24.04.2011 --
procedure TCustomHtmlViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if FView.Children.IsEmpty then
    // fill background as long as there is no box to show.
    inherited
  else
    // avoid flickering, if there is a box.
    Message.Result := 1;
end;

end.
