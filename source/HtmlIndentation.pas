{
Version   12
Copyright (c) 1995-2008 by L. David Baldwin,
Copyright (c) 2008-2010 by HtmlViewer Team
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

unit HtmlIndentation;

interface

uses
{$ifdef LCL}
  LclIntf, IntfGraphics, FpImage, LclType, LResources, LMessages, HtmlMisc,
{$else}
  Windows,
{$endif}
  SysUtils, Contnrs, Math, //Classes, //Graphics, ClipBrd, Controls, Messages, Variants, Types,
  HtmlGlobals;

//------------------------------------------------------------------------------
// TSketchMap manages the placing of floating boxes and the flow of inline
//            elements around the floating boxes.
//------------------------------------------------------------------------------

type

  EIndentationException = class(Exception);

  // BG, 27.08.2011: copied and modified IndentRec of HtmlViewer11
  TIndentation = class
  private
    // all coordinates relative to the containing box: (x=0, y=0) is the upper left edge of the containing box.
    FX: Integer;   // if left indentation: the first available x-coordinate to the right of this indentation.
                   // if right indentation: the first unavailable x-coordinate due to this indentation.
    FYT: Integer;  // top Y inclusive coordinate of this indentation.
    FYB: Integer;  // bottom Y exclusive coordinate of this indentation.
  public
    constructor Create(X, YT, YB: Integer);
    property X:  Integer read FX;
    property YT: Integer read FYT;
    property YB: Integer read FYB;
  end;

  // BG, 27.08.2011:
  TIndentationList = class(TObjectList)
  private
    function getIndentation(Index: Integer): TIndentation;
  public
    function GetMaxYB: Integer;
    property Items[Index: Integer]: TIndentation read getIndentation; default;
  end;

  // BG, 27.08.2011: copied and modified TIndentManager of HtmlViewer11
  TSketchMap = class
  private
    L: TIndentationList;  // list of left/top hand indentations.
    R: TIndentationList;  // list of right/bottom hand indentations.
    Width: Integer;       // width of the managed box.
    function LeftEdge(Y: Integer): Integer;
    function RightEdge(Y: Integer): Integer;
  public
    constructor Create(Width: Integer);
    destructor Destroy; override;
    function AddLeft(YT, YB, W: Integer): TIndentation;
    function AddRight(YT, YB, W: Integer): TIndentation;
    function AlignLeft(var Y: Integer; W: Integer; SpW: Integer = 0; SpH: Integer = 0): Integer;
    function AlignRight(var Y: Integer; W: Integer; SpW: Integer = 0; SpH: Integer = 0): Integer;
    function GetNextWiderY(Y: Integer): Integer;
    function GetClearY: Integer; overload;
    function GetClearLeftY: Integer; overload;
    function GetClearRightY: Integer; overload;
  end;

  ESketchMapStackException = class(EIndentationException);

  // BG, 28.08.2011:
  TSketchMapStack = class
  private
    FStack: TStack;
    function getTop: TSketchMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(IM: TSketchMap);
    function Pop: TSketchMap;
    property Top: TSketchMap read getTop;
  end;

implementation

{ TIndentation }

//-- BG ---------------------------------------------------------- 27.08.2011 --
constructor TIndentation.Create(X, YT, YB: Integer);
begin
  inherited Create;
  FX := X;
  FYT := YT;
  FYB := YB;
end;

{ TIndentationList }

//-- BG ---------------------------------------------------------- 27.08.2011 --
function TIndentationList.getIndentation(Index: Integer): TIndentation;
begin
  Result := Get(Index);
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
function TIndentationList.GetMaxYB: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    with Items[I] do
      if YB > Result then
        Result := YB;
end;

{ TIndentationManager }

//-- BG ---------------------------------------------------------- 27.08.2011 --
constructor TSketchMap.Create(Width: Integer);
begin
  inherited Create;
  Self.Width := Width;
  R := TIndentationList.Create;
  L := TIndentationList.Create;
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
destructor TSketchMap.Destroy;
begin
  R.Free;
  L.Free;
  inherited Destroy;
end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TSketchMap.AddLeft(YT, YB, W: Integer): TIndentation;
// Adds the indentation due to a box floating to the left.
// Returns the added indentation with Result.X being the first pixel to the right of the box.
// Thus Result.X is the right x-coordinate of the box and Result.X - W the left.
begin
  Result := TIndentation.Create(LeftEdge(YT) + W, YT, YB);
  L.Add(Result);
end;

//-- BG ---------------------------------------------------------- 05.02.2011 --
function TSketchMap.AddRight(YT, YB, W: Integer): TIndentation;
// Adds the indentation due to a box floating to the right.
// Returns the added indentation with Result.X being the first pixel of the box.
// Thus Result.X is the left x-coordinate of the box and Result.X + W the right.
begin
  Result := TIndentation.Create(RightEdge(YT) - W, YT, YB);
  R.Add(Result);
end;

////-- BG ---------------------------------------------------------- 23.02.2011 --
//procedure TSketchMap.Init(Width: Integer);
//begin
//  Self.Width := Width;
//  R.Clear;
//  L.Clear;
//end;

const
  BigY = 9999999;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TSketchMap.LeftEdge(Y: Integer): Integer;
// Returns the right most left indentation at Y.
// If there are no left indentations at Y, returns 0.
var
  I: Integer;
  IR: TIndentation;
  MinX: Integer;
begin
  Result := -MaxInt;
  MinX := 0;
  for I := 0 to L.Count - 1 do
  begin
    IR := L.Items[I];
    if (Y >= IR.YT) and (Y < IR.YB) and (Result < IR.X) then
      Result := IR.X;
  end;
  if Result = -MaxInt then
    Result := MinX;
end;

//-- BG ---------------------------------------------------------- 23.02.2011 --
function TSketchMap.RightEdge(Y: Integer): Integer;
// Returns the left most right indentation at Y.
// If there are no indentations at Y, returns Width.
var
  I: Integer;
  IR: TIndentation;
begin
  Result := MaxInt;
  for I := 0 to R.Count - 1 do
  begin
    IR := R.Items[I];
    if (Y >= IR.YT) and (Y < IR.YB) and (Result > IR.X) then
      Result := IR.X;
  end;
  if Result = MaxInt then
    Result := Width;
end;

//------------------------------------------------------------------------------
function TSketchMap.GetClearY: Integer;
// Returns the bottom most bottom of all floating boxes.
begin
  Result := Max(L.GetMaxYB, R.GetMaxYB);
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
function TSketchMap.GetClearLeftY: Integer;
// Returns the bottom most bottom of all boxes floating to the left.
begin
  Result := L.GetMaxYB;
end;

//-- BG ---------------------------------------------------------- 27.08.2011 --
function TSketchMap.GetClearRightY: Integer;
// Returns the bottom most bottom of all boxes floating to the right.
begin
  Result := R.GetMaxYB;
end;

//-- BG ---------------------------------------------------------- 06.02.2011 --
function TSketchMap.AlignLeft(var Y: Integer; W, SpW, SpH: Integer): Integer;
// Returns the aligned Y position of a box of width W starting at Y.
// Result is > Y, if available width at Y is too small for the block and optional additional space Sp
// Additional space e.g. for a textRec between aligned images.
var
  I, CL, CR, LX, RX, XL, XR, YY, MinX: Integer;
begin
  Result := LeftEdge(Y);
  if Result + W + SpW > RightEdge(Y) then
  begin
    // too wide, must find a wider place below:
    if (SpH > 0) and (Result + W <= RightEdge(Y + SpH)) then
    begin
      // fits into area below space Sp
      Inc(Y, SpH);
    end
    else
    begin
      // too small, must find a wider place below:
      YY := Y;
      MinX := 0;

      CL := Y;
      XL := Result; // valium for the compiler
      for I := L.Count - 1 downto 0 do
        with L.Items[I] do
        begin
          if (YB > Y) and ((YB < CL) or (CL = Y)) then
          begin
            if X = LeftEdge(YB - 1) then
            begin
              // This is the right most left indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CL := YB;
                XL := LX;
              end;
            end;
          end;
        end;

      CR := Y;
      XR := Result; // valium for the compiler
      for I := R.Count - 1 downto 0 do
        with R.Items[I] do
        begin
          if (YB > Y) and ((YB < CR) or (CR = Y)) then
          begin
            if X = RightEdge(YB - 1) then
            begin
              // This is the left most right indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CR := YB;
                XR := LX;
              end;
            end;
          end;
        end;

      if CL = Y then
      begin
        if CR = Y then
        begin
          // no better place found, just append at the end.
          Y := YY;
          Result := MinX;
        end
        else
        begin
          Y := CR;
          Result := XR;
        end
      end
      else if CR = Y then
      begin
        Y := CL;
        Result := XL;
      end
      else if CL < CR then
      begin
        Y := CL;
        Result := XL;
      end
      else
      begin
        Y := CR;
        Result := XR;
      end;
    end;
  end;
end;

function TSketchMap.AlignRight(var Y: Integer; W, SpW, SpH: Integer): Integer;
// Returns the aligned Y position of a box of width W starting at Y.
// Result is > Y, if available width at Y is too small for the block and optional additional space Sp
// Additional space e.g. for a textRec between aligned images.
var
  I, CL, CR, LX, RX, XL, XR, YY, MaxX: Integer;
begin
  Result := RightEdge(Y) - W;
  if Result < LeftEdge(Y) + SpW then
  begin
    // too small, must find a wider place below:
    if (SpH > 0) and (Result >= LeftEdge(Y + SpH)) then
    begin
      // fits into area below space Sp
      Inc(Y, SpH);
    end
    else
    begin
      YY := Y;
      MaxX := Width - W;

      CL := Y;
      XL := Result; // valium for the compiler
      for I := L.Count - 1 downto 0 do
        with L.Items[I] do
        begin
          if (YB > Y) and ((YB < CL) or (CL = Y)) then
          begin
            if X = LeftEdge(YB - 1) then
            begin
              // This is the right most left indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CL := YB;
                XL := RX;
              end;
            end;
          end;
        end;

      CR := Y;
      XR := Result; // valium for the compiler
      for I := R.Count - 1 downto 0 do
        with R.Items[I] do
        begin
          if (YB > Y) and ((YB < CR) or (CR = Y)) then
          begin
            if X = RightEdge(YB - 1) then
            begin
              // This is the left most right indentation
              LX := LeftEdge(YB);
              RX := RightEdge(YB) - W;
              if YY < YB then
                YY := YB;
              if RX >= LX then
              begin
                CR := YB;
                XR := RX;
              end;
            end;
          end;
        end;

      if CL = Y then
      begin
        if CR = Y then
        begin
          // no better place found, just append at the end.
          Y := YY;
          Result := MaxX;
        end
        else
        begin
          Y := CR;
          Result := XR;
        end
      end
      else if CR = Y then
      begin
        Y := CL;
        Result := XL;
      end
      else if CL < CR then
      begin
        Y := CL;
        Result := XL;
      end
      else
      begin
        Y := CR;
        Result := XR;
      end;
    end;
  end;
end;

function TSketchMap.GetNextWiderY(Y: Integer): Integer;
// Returns the next Y value which offers a wider space or Y if none
var
  I, CL, CR: Integer;
begin
  CL := Y;
  for I := 0 to L.Count - 1 do
    with L.Items[I] do
      if (YB > Y) and ((YB < CL) or (CL = Y)) then
        CL := YB;
  CR := Y;
  for I := 0 to R.Count - 1 do
    with R.Items[I] do
      if (YB > Y) and ((YB < CR) or (CR = Y)) then
        CR := YB;
  if CL = Y then
    Result := CR
  else if CR = Y then
    Result := CL
  else
    Result := Min(CL, CR);
end;

{ TIndentationManagerStack }

//-- BG ---------------------------------------------------------- 17.12.2011 --
constructor TSketchMapStack.Create;
begin
  inherited Create;
  FStack := TStack.Create;
end;

//-- BG ---------------------------------------------------------- 28.08.2011 --
destructor TSketchMapStack.Destroy;
begin
  while Top <> nil do
    Pop.Free;
  FStack.Free;
end;

//-- BG ---------------------------------------------------------- 17.12.2011 --
function TSketchMapStack.getTop: TSketchMap;
begin
  if FStack.AtLeast(1) then
    Result := FStack.Peek
  else
    Result := nil;
end;

//-- BG ---------------------------------------------------------- 28.08.2011 --
function TSketchMapStack.Pop: TSketchMap;
begin
  if Top = nil then
    raise ESketchMapStackException.Create('Cannot pop from empty stack.');
  Result := FStack.Pop;
end;

//-- BG ---------------------------------------------------------- 28.08.2011 --
procedure TSketchMapStack.Push(IM: TSketchMap);
begin
  if IM = nil then
    raise ESketchMapStackException.Create('Must not push nil onto the stack.');
  FStack.Push(IM);
end;

end.
