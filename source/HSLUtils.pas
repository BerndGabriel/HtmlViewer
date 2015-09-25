{
Version   11.3
Copyright 2012 by J. Peter Mugaas

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
//------------------------------------------------------------------------------
//
// HSL - RGB colour model conversions
//
// These four functions can be used to convert between the RGB and HSL colour
// models.  RGB values are represented using the 0-255 Windows convention and
// always encapsulated in a TColor 32 bit value.  HSL values are available as
// either 0 to 1 floating point (double) values or as a 0 to a defined integer
// value.  The colour common dialog box uses 0 to 240 by example.
//
// The code is based on that found (in C) on:
//
//   http:/www.r2m.com/win-developer-faq/graphics/8.html
//
// Grahame Marsh 12 October 1997
//
// Freeware - you get it for free, I take nothing, I make no promises!
//
// Please feel free to contact me: grahame.s.marsh@corp.courtaulds.co.uk
//
// Revison History:
//    Version 1.00 - initial release  12-10-1997
//
//------------------------------------------------------------------------------

{Additions from J. Peter Mugaas
release: 2-16-2012
}
unit HSLUtils;

interface

uses
{$ifdef MSWindows}
  Windows,
{$else MSWindows}
  Types,
{$endif MSWindows}
  Graphics;

var
  HSLRange: integer = 240;

// convert a HSL value into a RGB in a TColor
// HSL values are 0.0 to 1.0 double
function HSLtoRGB (H, S, L: double): TColor; overload;
function HSLtoRGB (H : Integer; S, L: double): TColor; overload;
function HSLtoRGB (H, S, L: Integer): TColor; overload;

// convert a HSL value into a RGB in a TColor
// SL values are 0 to the HSLRange variable
// H value is to HSLRange-1
function HSLRangeToRGB (H, S, L : integer): TColor;

// convert a RGB value (as TColor) into HSL
// HSL values are 0.0 to 1.0 double
procedure RGBtoHSL (RGB: TColor; var H, S, L : double);

// convert a RGB value (as TColor) into HSL
// SL values are 0 to the HSLRange variable
// H value is to HSLRange-1
procedure RGBtoHSLRange (RGB: TColor; var H, S, L : integer);

implementation
{$ifdef MSWindows}
{$else MSWindows}
function GetRValue(RGB: TColor): Byte;
begin
  Result := Byte(RGB);
end;

function GetGValue(RGB: TColor): Byte;
begin
  Result := Byte(RGB shr 8);
end;

function GetBValue(RGB: TColor): Byte;
begin
  Result := Byte(RGB shr 16);
end;

function RGB(R, G, B: Cardinal): TColor;
begin
  Result := R + G shl 8 + LongWord(b) shl 16;
end;

{$endif MSWindows}

function HSLtoRGB (H, S, L: double): TColor;
var
  M1, M2: double;

  function HueToColourValue (Hue: double) : byte;
  var
    V : double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else
      if Hue > 1 then
        Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
      if 2 * Hue < 1 then
        V := M2
      else
        if 3 * Hue < 2 then
          V := M1 + (M2 - M1) * (2/3 - Hue) * 6
        else
          V := M1;
    Result := round (255 * V)
  end;

var
  R, G, B: byte;
begin
  if S = 0 then
  begin
    R := round (255 * L);
    G := R;
    B := R
  end else begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue (H + 1/3);
    G := HueToColourValue (H);
    B := HueToColourValue (H - 1/3)
  end;

  Result := RGB (R, G, B)
end;

function HSLtoRGB (H : Integer; S, L: double): TColor; overload;
begin
  Result := HSLtoRGB( H / 360, S, L);
end;

function HSLtoRGB (H, S, L: Integer): TColor; overload;
begin
  Result := HSLtoRGB(H,S / 100, L / 100);
end;

function HSLRangeToRGB (H, S, L : integer): TColor;
begin
  Result := HSLToRGB (H / (HSLRange-1), S / HSLRange, L / HSLRange)
end;

// Convert RGB value (0-255 range) into HSL value (0-1 values)

procedure RGBtoHSL (RGB: TColor; var H, S, L : double);

  function Max (a, b : double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b
  end;

  function Min (a, b : double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R := GetRValue (RGB) / 255;
  G := GetGValue (RGB) / 255;
  B := GetBValue (RGB) / 255;
  Cmax := Max (R, Max (G, B));
  Cmin := Min (R, Min (G, B));

// calculate luminosity
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then  // it's grey
  begin
    H := 0; // it's actually undefined
    S := 0
  end else begin
    D := Cmax - Cmin;

// calculate Saturation
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);

// calculate Hue
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H  := 2 + (B - R) /D
      else
        H := 4 + (R - G) / D;

    H := H / 6;
    if H < 0 then
      H := H + 1
  end
end;

procedure RGBtoHSLRange (RGB: TColor; var H, S, L : integer);
var
  Hd, Sd, Ld: double;
begin
  RGBtoHSL (RGB, Hd, Sd, Ld);
  H := round (Hd * (HSLRange-1));
  S := round (Sd * HSLRange);
  L := round (Ld * HSLRange);
end;

end.
