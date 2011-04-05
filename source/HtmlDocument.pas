{
HtmlViewer Version 12
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

unit HtmlDocument;

interface

uses
  Windows, Classes, Graphics,
  //
  HtmlDraw,
  HtmlGlobals,
  HtmlTree;

type

//------------------------------------------------------------------------------
// THtmlDocument holds the complete element tree of an HTML document.
//------------------------------------------------------------------------------

  THtmlDocument = class
  private
    FTitle: ThtString;
    FTree: THtmlElement;
    procedure setTree(const Value: THtmlElement);
  public
    destructor Destroy; override;
    property Title: ThtString read FTitle write FTitle;
    property Tree: THtmlElement read FTree write setTree;
  end;

//------------------------------------------------------------------------------
// THtmlView is base class for the visual representation of the THtmlElements
//------------------------------------------------------------------------------

  THtmlView = class;

  THtmlViewList = {$ifdef UseEnhancedRecord} record {$else} class {$endif}
  private
    FFirst: THtmlView;
    FLast: THtmlView;
  public
    function IsEmpty: Boolean;
    procedure Add(View: THtmlView);
    procedure Init;
    procedure Clear;
    procedure Remove(View: THtmlView);
    property First: THtmlView read FFirst;
    property Last: THtmlView read FLast;
  end;

  THtmlView = class
  private
    FParent: THtmlView;                    // parent view or nil if this is the root view.
    FPrev: THtmlView;                      // previous sibling in parent's content.
    FNext: THtmlView;                      // next sibling in parent's content.
    FChildren: THtmlViewList;              // the content.
    FBounds: TRect;
    //
    FMargins: TRectIntegers;
    FBorderWidths: TRectIntegers;
    FBorderColors: TRectColors;
    FBorderStyles: TRectStyles;
    FColor: TColor;
    FPadding: TRectIntegers;
    //
    FText: ThtString;
    FAlignment: TAlignment;
    FFont: TFont;
  protected
    property Parent: THtmlView read FParent;
    property Prev: THtmlView read FPrev;
    property Next: THtmlView read FNext;
    property Children: THtmlViewList read FChildren;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    //
    procedure Paint(Canvas: TCanvas); virtual;
    property BoundsRect: TRect read FBounds write FBounds;
    //
    property Margins: TRectIntegers read FMargins write FMargins;
    property BorderWidths: TRectIntegers read FBorderWidths write FBorderWidths;
    property BorderColors: TRectColors read FBorderColors write FBorderColors;
    property BorderStyles: TRectStyles read FBorderStyles write FBorderStyles;
    property Color: TColor read FColor write FColor;
    property Padding: TRectIntegers read FPadding write FPadding;
    //
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Font: TFont read FFont;
    property Text: ThtString read FText write FText;
  end;

  THtmlFrame = class(THtmlView)

  end;

implementation

{ THtmlDocument }

//-- BG ---------------------------------------------------------- 27.03.2011 --
destructor THtmlDocument.Destroy;
begin
  FTree.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 29.03.2011 --
procedure THtmlDocument.setTree(const Value: THtmlElement);
begin
  if FTree <> Value then
  begin
    FTree.Free;
    FTree := Value;
  end;
end;

{ THtmlViewList }

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlViewList.Add(View: THtmlView);
// append View
var
  Prev: THtmlView;
begin
  assert(View.Next = nil, 'Don''t add chained links twice. View.Next is not nil.');
  assert(View.Prev = nil, 'Don''t add chained links twice. View.Prev is not nil.');

  Prev := Last;

  // link to prev
  if Prev = nil then
    FFirst := View
  else
    Prev.FNext := View;
  View.FPrev := Prev;

  // link to next
  FLast := View;
  View.FNext := nil;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlViewList.Clear;
var
  View: THtmlView;
begin
  FLast := nil;
  while First <> nil do
  begin
    View := First;
    FFirst := View.Next;
    View.Free;
  end;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlViewList.Init;
begin
  FFirst := nil;
  FLast := nil;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
function THtmlViewList.IsEmpty: Boolean;
begin
  Result := FFirst = nil;
end;

//-- BG ---------------------------------------------------------- 03.04.2011 --
procedure THtmlViewList.Remove(View: THtmlView);
var
  Prev: THtmlView;
  Next: THtmlView;
begin
  Prev := View.Prev;
  Next := View.Next;

  if Prev = nil then
  begin
    if First = View then
      FFirst := Next;
  end
  else
  begin
    Prev.FNext := Next;
    View.FPrev := nil;
  end;

  if Next = nil then
  begin
    if Last = View then
      FLast := Prev;
  end
  else
  begin
    Next.FPrev := Prev;
    View.FNext := nil;
  end;
end;

{ THtmlView }

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlView.AfterConstruction;
begin
  inherited;
  FFont := TFont.Create;
end;

//-- BG ---------------------------------------------------------- 05.04.2011 --
procedure THtmlView.BeforeDestruction;
begin
  FFont.Free;
  inherited;
end;

//-- BG ---------------------------------------------------------- 04.04.2011 --
procedure THtmlView.Paint(Canvas: TCanvas);
var
  Rect: TRect;
  Child: THtmlView;
  Org: TPoint;
  Flags: Integer;
begin
  DeflateRect(Rect, BoundsRect, Margins);
  DrawBorder(Canvas, Rect, BorderWidths, BorderColors, BorderStyles, Color);
  DeflateRect(Rect, BorderWidths);
  DeflateRect(Rect, Padding);
  if Length(Text) > 0 then
  begin
    case Alignment of
      taLeftJustify: Flags := DT_LEFT;
      taRightJustify: Flags := DT_RIGHT;
      taCenter: Flags := DT_CENTER;
    end;
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Font;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    DrawTextW(Canvas.Handle, PhtChar(Text), Length(Text), Rect, Flags + DT_NOCLIP + DT_VCENTER + DT_SINGLELINE);
  end;
  if Children.First <> nil then
  begin
    GetWindowOrgEx(Canvas.Handle, Org);
    SetWindowOrgEx(Canvas.Handle, Org.X + Rect.Left, Org.Y + Rect.Top, nil);
    Child := Children.First;
    repeat
      Child.Paint(Canvas);
      Child := Child.Next;
    until Child = nil;
  end;
end;

end.
