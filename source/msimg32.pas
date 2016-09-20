{
Version   11.7
Copyright 2012-2014 by J. Peter Mugaas
Copyright 2015-2016 by HtmlViewer Team

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
unit msimg32;

interface
{$ALIGN ON}
{$MINENUMSIZE 4}
{$ifdef MsWindows}

uses Windows;
{
This is not done like a typical header for several reasons.

1) We don't want to conflict with Embarcadero's Windows.pas header.
2) We do not want to load a .DLL during drawing code and that would happen if
you loaded the .DLL when calling a function for the first time.
3) Some Embarcadero's VCL versions will load the msimg32.dll so we do not want to
reload it in those cases.
}
const
  c_AC_SRC_OVER                = $00;
  c_AC_SRC_ALPHA               = $01;

  c_GRADIENT_FILL_RECT_H    = $00000000;
  c_GRADIENT_FILL_RECT_V    = $00000001;
  c_GRADIENT_FILL_TRIANGLE  = $00000002;
  c_GRADIENT_FILL_OP_FLAG   = $000000ff;

type
// Base record type for the enhanced metafile.
  t_EMR = record
    iType : DWORD;              // Enhanced metafile record type
    nSize : DWORD;              // Length of the record in bytes.
                                // This must be a multiple of 4.
  end;
  Pt_EMR = ^t_EMR;

  t_BLENDFUNCTION = record
    BlendOp : Byte;
    BlendFlags : Byte;
    SourceConstantAlpha : Byte;
    AlphaFormat : Byte;
  end;
  Pt_BLENDFUNCTION = ^t_BLENDFUNCTION;
  t_COLOR16 = WORD;
  t_TRIVERTEX = record
    x : LongInt;
    y : LongInt;
    Red : t_COLOR16;
    Green : t_COLOR16;
    Blue : t_COLOR16;
    Alpha : t_COLOR16;
  end;
  Pt_TRIVERTEX = ^t_TRIVERTEX;
  t_GRADIENT_TRIANGLE = record
    Vertex1 : ULONG;
    Vertex2 : ULONG;
    Vertex3 : ULONG;
  end;
  Pt_GRADIENT_TRIANGLE = ^t_GRADIENT_TRIANGLE;
  t_GRADIENT_RECT = record
    UpperLeft : ULONG;
    LowerRight : ULONG;
  end;
  Pt_GRADIENT_RECT = ^t_GRADIENT_RECT;
  t_EMRGRADIENTFILL = record
    emr : t_EMR;
    rclBounds : TRect;          // Inclusive-inclusive bounds in device units
    nVer : DWORD;
    nTri : DWORD;
    ulMode : ULONG;
    Ver : array[0..0] of t_TRIVERTEX;
  end;
  Pt_t_EMRGRADIENTFILL = ^t_EMRGRADIENTFILL;

type
  fn_AlphaBlend = function(hdcDest : HDC;
    xoriginDest, yoriginDest, wDest, hDest : Integer;
    hdcSrc : HDC;
    xoriginSrc, yoriginSrc, wSrc, hSrc : Integer;
    ftn : t_BLENDFUNCTION ) : BOOL stdcall;
  fn_GradientFill = function(hdc : HDC;
    pVertex : Pt_TRIVERTEX;
    nVertex : ULONG;
    pMesh : Pointer;
    nMesh : ULONG;
    ulMode : ULONG ) : BOOL stdcall;
  fn_TransparentBlt = function(hdcDest : HDC;
    xoriginDest, yoriginDest, wDest, hDest : Integer;
    hdcSrc : HDC;
    xoriginSrc, yoriginSrc, wSrc, hSrc : Integer;
    crTransparent : UINT ) : BOOL stdcall;

{Note that I could not find TransparentDIBits and AlphaDIBBlend are
not in the msimg32.dll and I can not find those function in the current
Windows 7.1 SDK.

I also can not find the alpha format flags
AC_SRC_NO_PREMULT_ALPHA
AC_SRC_NO_ALPHA
AC_DST_NO_PREMULT_ALPHA
AC_DST_NO_PREMULT_ALPHA
AC_DST_NO_ALPHA

In the Windows 7.1 SDK.  This all is interesting as some older versions of winGDI.h
include those functions and flags. None of this stuff is documented at MSDN.
}

const
  c_msimg32 = 'msimg32.dll';
  c_AlphaBlend = 'AlphaBlend';
  c_GradientFill = 'GradientFill';
  c_TransparentBlt = 'TransparentBlt';

var
  jpm_AlphaBlend : fn_AlphaBlend = nil;
  jpm_GradientFill : fn_GradientFill = nil;
  jpm_TransparentBlt : fn_TransparentBlt = nil;

var
  GLoadedMsImg : Boolean = False;
  GHandleMsImg : HMODULE = INVALID_HANDLE_VALUE;

{$endif MsWindows}
implementation
{$ifdef MsWindows}
uses SysUtils;

initialization
{if the .DLL is already loaded, then do not load it again,
use that copy instead.  Some RTL-code will load it.}
  GHandleMsImg := GetModuleHandle(c_msimg32);
  if GHandleMsImg = INVALID_HANDLE_VALUE then
  begin
    GHandleMsImg := SafeLoadLibrary(c_msimg32);
    if GHandleMsImg <> INVALID_HANDLE_VALUE then
      GLoadedMsImg := True;
  end;
  if GHandleMsImg <> INVALID_HANDLE_VALUE then
  begin
    jpm_AlphaBlend := GetProcAddress(GHandleMsImg,c_AlphaBlend);
    jpm_GradientFill := GetProcAddress(GHandleMsImg,c_GradientFill);
    jpm_TransparentBlt := GetProcAddress(GHandleMsImg,c_TransparentBlt);
  end;
finalization
  if GLoadedMsImg then begin
    FreeLibrary(GHandleMsImg);
    jpm_AlphaBlend := nil;
    jpm_GradientFill := nil;
    jpm_TransparentBlt := nil;
  end;

{$endif MsWindows}
end.
