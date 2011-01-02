{*********************************************************}
{*                     htmlmisc.pas                      *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* Copyright (C) 2006-2009 Phil Hess.                                         *}
{* All Rights Reserved.                                                       *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit HtmlMisc;

{
  This unit provides types, constants, and functions that fill
   in some gaps in the Lazarus LCL for compiling the ported THtml controls.
}

{$mode objfpc}{$H+}

interface

uses 
  LclIntf, LMessages, LclType, LclProc, InterfaceBase, Types, SysUtils,
  GraphType, Graphics, Controls, Printers;

type
  LPCSTR = PAnsiChar;
  LPWSTR = PWideChar;
  LPSTR = PAnsiChar;

  tagXFORM = packed record
    eM11: Single;
    eM12: Single;
    eM21: Single;
    eM22: Single;
    eDx: Single;
    eDy: Single;
  end;
  TXForm = tagXFORM;
  PXForm = ^TXForm;

  _RGNDATAHEADER = packed record
    dwSize: DWORD;
    iType: DWORD;
    nCount: DWORD;
    nRgnSize: DWORD;
    rcBound: TRect;
  end;
  TRgnDataHeader = _RGNDATAHEADER;
  RGNDATA = record
    rdh: TRgnDataHeader;
    Buffer: array[0..0] of CHAR;
    Reserved: array[0..2] of CHAR;
  end;
  PRgnData = ^TRgnData;
  TRgnData = RGNDATA;

const
  WM_SETREDRAW = $000B;

  GMEM_MOVEABLE = 2;
  GMEM_DDESHARE = $2000;
  GMEM_ZEROINIT = $40;
  BM_SETCHECK = $00F1;
  STRETCH_DELETESCANS = 3;
  CP_ACP = 0;  {ANSI code page}
  CP_OEMCP = 1;  {OEM code page }
  CP_MACCP = 2;  {MAC code page }
  CP_UTF8 = 65001;
  RDH_RECTANGLES = 1;
  HeapAllocFlags = GMEM_MOVEABLE;
  NUMCOLORS = 24;
{$IFNDEF MSWINDOWS}
  Win32Platform = 2;  //Set as though Windows NT (VER_PLATFORM_WIN32_NT)
  Win32MinorVersion = 0;
{$ENDIF}


{These belong in LclIntf unit}
function GetTickCount : DWORD;
procedure OutputDebugString(lpOutputString: PChar);
function GlobalAlloc(uFlags: UINT; dwBytes: DWORD): HGLOBAL;
function GlobalLock(hMem: HGLOBAL): Pointer;
function GlobalUnlock(hMem: HGLOBAL): BOOL;
function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD;
                             const lpMultiByteStr: LPCSTR; cchMultiByte: Integer;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer;
function WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer; 
                             lpMultiByteStr: LPSTR; cchMultiByte: Integer; 
                             lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBoolean): Integer;
function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
function CharLowerBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer; var Size: TSize): BOOL;
function GetTextExtentPoint32A(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): BOOL;
function SetTextAlign(DC: HDC; Flags: UINT): UINT;
function TextOutW(DC: HDC; X, Y: Integer; Str: PWideChar; Count: Integer): BOOL;
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; 
                     Str: PWideChar; Count: Longint; Dx: PInteger): BOOL;
function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer;
                   var lpRect: TRect; uFormat: UINT): Integer;
function PatBlt(DC: HDC; X, Y, Width, Height: Integer; Rop: DWORD): BOOL;
function SetTextJustification(DC: HDC; BreakExtra, BreakCount: Integer): Integer;
function GetBrushOrgEx(DC: HDC; var lppt: TPoint): BOOL;
function SetBrushOrgEx(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL;
function GlobalFree(hMem: HGLOBAL): HGLOBAL;
function timeGetTime: DWORD;
function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; 
                               p5, p6: PInteger; var p7: TSize): BOOL;
function GetTempPath(nBufferLength: DWORD; lpBuffer: PChar): DWORD;
function CharNextEx(CodePage: Word; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR;
function ExtCreateRegion(XForm: PXForm; Count: DWORD; const RgnData: TRgnData): HRGN;
//function ExtCreatePen(PenStyle, Width: DWORD; const Brush: TLogBrush; StyleCount: DWORD; Style: Pointer): HPEN;
function BeginPath(DC: HDC): BOOL;
function EndPath(DC: HDC): BOOL;
function StrokePath(DC: HDC): BOOL; 
function CloseFigure(DC: HDC): BOOL;
function ClipCursor(lpRect: PRect): BOOL;

{This belongs in Graphics unit}
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
                               SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; 
                               MaskDC: HDC; MaskX, MaskY: Integer): Boolean;


implementation

var
  ExpectsUTF8 : Boolean;  {True=widgetset expects to receive UTF8-encoded strings}

 {These functions belong in LclIntf unit}

function GetTickCount : DWORD;
 {On Windows, this is number of milliseconds since Windows was 
   started. On non-Windows platforms, LCL returns number of 
   milliseconds since Dec. 30, 1899, wrapped by size of DWORD.}
begin
  Result := LclIntf.GetTickCount;
end;

procedure OutputDebugString(lpOutputString: PChar);
begin
{
  Windows.OutputDebugString(lpOutputString);
}
end;

function GlobalAlloc(uFlags: UINT; dwBytes: DWORD): HGLOBAL;
// Replace with calls to standard Clipboard methods?
begin
//  Result := Windows.GlobalAlloc(uFlags, dwBytes);
  Result := THandle(GetMem(dwBytes));
end;

function GlobalLock(hMem: HGLOBAL): Pointer;
begin
//  Result := Windows.GlobalLock(hMem);
  Result := PAnsiChar(hMem);
end;

function GlobalUnlock(hMem: HGLOBAL): BOOL;
begin
//  Result := Windows.GlobalUnlock(hMem);
  FreeMem(Pointer(hMem));
  Result := True;
end;

function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD;
                             const lpMultiByteStr: LPCSTR; cchMultiByte: Integer;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer;
{
  Result := Windows.MultiByteToWideChar(CodePage, dwFlags, lpMultiByteStr,
                                        cchMultiByte, lpWideCharStr, cchWideChar);
}
var
  s : string;
  w : WideString;
begin
  if cchMultiByte < 0 then  {Null terminated?}
    s := lpMultiByteStr
  else begin
    SetLength(s, cchMultiByte);
    Move(lpMultiByteStr^, s[1], cchMultiByte);
  end;
  SetLength(w, Succ(Length(s)));
  StringToWideChar(s, PWideChar(w), Length(w));
   {Look for terminating null to determine length of returned string}
  Result := 0;
  while w[Succ(Result)] <> #0 do
   Inc(Result);
  if cchMultiByte < 0 then  {Include terminating null too?}
    Inc(Result);
  if cchWideChar > 0 then  {Okay to return string?}
    Move(w[1], lpWideCharStr^, Result*2);  {Assume dest. buffer has enough space}
end;

function WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer; 
                             lpMultiByteStr: LPSTR; cchMultiByte: Integer; 
                             lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBoolean): Integer;
var
  w : WideString;
  s : string;
begin
  if cchWideChar < 0 then  {Null terminated?}
    w := lpWideCharStr
  else begin  {Specifies number of wide chars to convert}
    SetLength(w, cchWideChar);
    Move(lpWideCharStr^, w[1], cchWideChar*2);
  end;
  s := WideCharToString(PWideChar(w));
  Result := Length(s);
  if cchWideChar < 0 then  {Include terminating null too?}
    Inc(Result);   
  if cchMultiByte > 0 then  {Okay to return string?}
    Move(s[1], lpMultiByteStr^, Result);  {Assume dest. buffer has enough space}
end;

function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  w : WideString;
begin
  SetLength(w, cchLength);
  Move(lpsz^, w[1], cchLength*2);
  w := WideUpperCase(w);
  Move(w[1], lpsz^, cchLength*2);
  Result := cchLength; 
end;

function CharLowerBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
var
  w : WideString;
begin
  SetLength(w, cchLength);
  Move(lpsz^, w[1], cchLength*2);
  w := WideLowerCase(w);
  Move(w[1], lpsz^, cchLength*2);
  Result := cchLength; 
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer;
                             var Size: TSize): BOOL;
var
  w : WideString;
  s : string;
begin
  if Count = 0 then  {No text? (don't want range error with w[1])}
  begin
    Size.cx := 0;
    Size.cy := 0;
    Result := True;
    Exit;
  end;
  {First copy to WideString since it may not have terminating null}
  SetLength(w, Count);
  Move(Str^, w[1], Count*2);
  if ExpectsUTF8 then
    s := UTF8Encode(w)  {Widgetset expects UTF8, so encode wide string as UTF8}
  else
    s := w;  {Just convert to ANSI}
  Result := LclIntf.GetTextExtentPoint32(DC, PChar(s), Length(s), Size);
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer;
                               var Size: TSize): BOOL;
begin
  Result := GetTextExtentPointW(DC, Str, Count, Size);  //No Point32W function
end;

function GetTextExtentPoint32A(DC: HDC; Str: PChar; Count: Integer; var Size: TSize): BOOL;
begin
  Result := GetTextExtentPoint(DC, Str, Count, Size);  //No Point32A function
end;

var
  CurTextAlign : UINT;
  CurTA_DC     : HDC;

function GetTextAlign(DC: HDC): UINT;
begin
  if DC = CurTA_DC then
    Result := CurTextAlign
  else
    Result := 0;
end;

function SetTextAlign(DC: HDC; Flags: UINT): UINT;
begin
   {Save the most recently set DC's text alignment flags with the
     assumption that usually working with just one DC at a time that
     has non-default alignment flags. (Better solution would be to
     save each DC's alignment flags in a collection or something.)
     Use these flags in TextOut and ExtTextOut to implement.}
  Result := GetTextAlign(DC);
  CurTextAlign := Flags;
  CurTA_DC := DC;
end;

function TextOutW(DC: HDC; X, Y: Integer; Str: PWideChar; Count: Integer): BOOL;
var
  TM : TEXTMETRIC;
  w  : WideString;
  s  : string;
begin
  if Count = 0 then  {Nothing to output? (don't want range error with w[1])}
  begin
    Result := True;
    Exit;
  end;
  if CurTA_DC = DC then
  begin  //Adjust reference point here since not done in widgetset
    GetTextMetrics(DC, TM);
    if (CurTextAlign and TA_BASELINE) <> 0 then
      Y := Y - (TM.tmHeight - TM.tmDescent);
  end;
  {First copy to WideString since it may not have terminating null}
  SetLength(w, Count);
  Move(Str^, w[1], Count*2);
  if ExpectsUTF8 then
    s := UTF8Encode(w)  {Widgetset expects UTF8, so encode wide string as UTF8}
  else
    s := w;  {Just convert to ANSI}
  Result := TextOut(DC, X, Y, PChar(s), Length(s)); 
   {Note not calling LclIntf's TextOut}
end;

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; 
                     Str: PWideChar; Count: Longint; Dx: PInteger): BOOL;
var
  TM : TEXTMETRIC;
  w : WideString;
  s : string;
begin
  if Count = 0 then  {Nothing to output? (don't want range error with w[1])}
  begin
    Result := True;
    Exit;
  end;
  if CurTA_DC = DC then
  begin  //Adjust reference point here since not done in widgetset
    GetTextMetrics(DC, TM);
    if (CurTextAlign and TA_BASELINE) <> 0 then
      Y := Y - (TM.tmHeight - TM.tmDescent);
  end;
  {First copy to WideString since it may not have terminating null}
  SetLength(w, Count);
  Move(Str^, w[1], Count*2);
  if ExpectsUTF8 then
    s := UTF8Encode(w) {Widgetset expects UTF8, so encode wide string as UTF8}
  else
    s := w;  {Just convert to ANSI}
  Result := ExtTextOut(DC, X, Y, Options, Rect, PChar(s), Length(s), Dx);
   {Note not calling LclIntf's ExtTextOut}
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer;
                   var lpRect: TRect; uFormat: UINT): Integer;
var
  w : WideString;
  s : string;
begin
  if nCount = -1 then  {String is null-terminated?}
    w := WideString(lpString)
  else
  begin
     {First copy to WideString since it may not have terminating null}
    SetLength(w, nCount);
    Move(lpString^, w[1], nCount*2);
  end;
  if ExpectsUTF8 then
    s := UTF8Encode(w)  {Widgetset expects UTF8, so encode wide string as UTF8}
  else
    s := w;  {Just convert to ANSI}
  Result := LclIntf.DrawText(hDC, PChar(s), Length(s), lpRect, uFormat);
end;

function PatBlt(DC: HDC; X, Y, Width, Height: Integer; Rop: DWORD): BOOL;
begin
  WriteLn('PatBlt not implemented yet');
end;

function SetTextJustification(DC: HDC; BreakExtra, BreakCount: Integer): Integer;
begin
//  WriteLn('SetTextJustification not implemented yet');
end;

function GetBrushOrgEx(DC: HDC; var lppt: TPoint): BOOL;
begin
  WriteLn('GetBrushOrgEx not implemented yet');
end;

function SetBrushOrgEx(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL;
begin
  WriteLn('SetBrushOrgEx not implemented yet');
end;

function GlobalFree(hMem: HGLOBAL): HGLOBAL;
begin
  WriteLn('GlobalFree not implemented yet');
end;

function timeGetTime: DWORD;
begin
  Result := GetTickCount;
//  Result := MMSystem.timeGetTime;  //If take out, don't need MMSystem in uses.
//  Result := Trunc(TimeStampToMSecs(DateTimeToTimeStamp(Now)));  //Can overflow.
end;

function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer; 
                               p5, p6: PInteger; var p7: TSize): BOOL;
begin
  WriteLn('GetTextExtentExPointW not implemented yet');
end;

function GetTempPath(nBufferLength: DWORD; lpBuffer: PChar): DWORD;
var
  TmpDirName: String;
begin
  TmpDirName := GetTempDir;
  if Length(TmpDirName) >= nBufferLength then  {Buffer not big enough?}
  begin
    Move(TmpDirName[1], lpBuffer, nBufferLength);
    Result := Length(TmpDirName)+1;
  end
  else begin
    Move(TmpDirName[1], lpBuffer, Length(TmpDirName)+1); //Include terminating null
    Result := Length(TmpDirName);
  end;
end;

function CharNextEx(CodePage: Word; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR;
begin
  Result := lpCurrentChar + 1;  //For now.
end;

function ExtCreateRegion(XForm: PXForm; Count: DWORD; const RgnData: TRgnData): HRGN;
begin
  WriteLn('ExtCreateRegion not implemented yet');
end;

function BeginPath(DC: HDC): BOOL;
begin
  WriteLn('BeginPath not implemented yet');
end;

function EndPath(DC: HDC): BOOL;
begin
  WriteLn('EndPath not implemented yet');
end;

function StrokePath(DC: HDC): BOOL; 
begin
  WriteLn('StrokePath not implemented yet');
end;

function CloseFigure(DC: HDC): BOOL;
begin
  WriteLn('CloseFigure not implemented yet');
end;

function ClipCursor(lpRect: PRect): BOOL;
begin
  WriteLn('ClipCursor not implemented yet');
end;


 {This belongs in Graphics unit}
function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
                               SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; 
                               MaskDC: HDC; MaskX, MaskY: Integer): Boolean;
begin
// Need implementation, but for now just call StretchBlt.
  Result := StretchBlt(DstDC, DstX, DstY, DstW, DstH, 
                       SrcDC, SrcX, SrcY, SrcW, SrcH, SrcCopy);
end;


initialization
  ExpectsUTF8 := WidgetSet.LCLPlatform in [lpCarbon, lpQt, lpGTK2, lpWin32];

end.

