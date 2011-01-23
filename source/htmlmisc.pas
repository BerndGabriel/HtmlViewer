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
   in some gaps in the Lazarus LCL for compiling the ported
   THtml controls.
}

interface

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
   Windows,
  {$ELSE}
   Types, Printers,
  {$ENDIF}
  LclIntf, LMessages, LclType, LclProc, InterfaceBase,
  GraphType, Graphics, Controls;
   {Important: Be sure to list LclType after SysUtils and Classes
     in order to use LclType's THandle declaration (32 or 64 bits)
     rather than THandle in SysUtils and Classes (=System.THandle,
     which apparently is always 32 bits).}

type
  TButtonStyle = (bsAutoDetect, bsWin31, bsNew);

{$IFDEF MSWINDOWS}
{$ELSE}
  TWMMouse = TLMMouse;
  TWMKeyDown = TLMKeyDown;
  TWMNCHitTest = TLMNCHitTest;
  TWMSetText = TLMSetText;
  TCMDesignHitTest = TWMMouse;
  TWMChar = TLMChar;
  TWMClear = TLMNoParams;
  TWMCopy = TLMNoParams;
  TWMCut = TLMNoParams;
  TWMLButtonDblClk = TLMLButtonDblClk;
  TWMLButtonDown = TLMLButtonDown;
  TWMLButtonUp = TLMLButtonUp;
  TWMRButtonDown = TLMRButtonDown;
  TWMSysKeyDown = TLMSysKeyDown;
  TWMMouseActivate = packed record
    Msg: Cardinal;
    TopLevel: HWND;
    HitTestCode: Word;
    MouseMsg: Word;
    Result: Longint;
    end;
  TWMMouseMove = TLMMouseMove;
  TWMPaste = TLMNoParams;
  TMessage = TLMessage;
  TWMEraseBkgnd = TLMEraseBkgnd;
  TWMGetText = TLMGetText;
  TWMGetTextLength = TLMGetTextLength;
  TWMKillFocus = TLMKillFocus;
  TWMSetCursor = packed record
    Msg: Cardinal;
    CursorWnd: HWND;
    HitTest: Word;
    MouseMsg: Word;
    Result: Longint;
    end;
  TWMSetFocus = TLMSetFocus;
  TWMGetDlgCode = TLMNoParams;
  TWMSize = TLMSize;
  TWMSetFont = packed record
    Msg: Cardinal;
    Font: HFONT;
    Redraw: WordBool;
    Unused: Word;
    Result: Longint;
    end;
  TWMCommand = TLMCommand;
  TWMDrawItem = TLMDrawItems;
  LPDWORD = PDWORD;
  TFNWndEnumProc = TFarProc;
  TNonClientMetrics = packed record
    cbSize: UINT;
    iBorderWidth: Integer;
    iScrollWidth: Integer;
    iScrollHeight: Integer;
    iCaptionWidth: Integer;
    iCaptionHeight: Integer;
    lfCaptionFont: TLogFontA;
    iSmCaptionWidth: Integer;
    iSmCaptionHeight: Integer;
    lfSmCaptionFont: TLogFontA;
    iMenuWidth: Integer;
    iMenuHeight: Integer;
    lfMenuFont: TLogFontA;
    lfStatusFont: TLogFontA;
    lfMessageFont: TLogFontA;
    end;
  TWMKey = TLMKey;
  TWMScroll = TLMScroll;
  TWMNoParams = TLMNoParams;
  TWMPaint = TLMPaint;
  TWMNCPaint = packed record
    Msg: Cardinal;
    RGN: HRGN;
    Unused: Longint;
    Result: Longint;
    end;
  TWMHScroll = TLMHScroll;
  TWMVScroll = TLMVScroll;

  PINT = ^Integer;
  PUINT = ^UINT;
{$ENDIF}

{$IFDEF MSWINDOWS}
  tagXFORM = XFORM;
  TXForm = tagXFORM;

  TGCPResultsW = GCP_RESULTS;

//  OSVERSIONINFO = _OSVERSIONINFO;

{$ELSE}
  tagXFORM = packed record
    eM11: Single;
    eM12: Single;
    eM21: Single;
    eM22: Single;
    eDx: Single;
    eDy: Single;
  end;
  TXForm = tagXFORM;

  tagGCP_RESULTSW = packed record
    lStructSize: DWORD;
    lpOutString: PWideChar;
    lpOrder: PUINT;
    lpDx: PINT;
    lpCaretPos: PINT;
    lpClass: PWideChar;
    lpGlyphs: PUINT;
    nGlyphs: UINT;
    nMaxFit: Integer;
  end;
  TGCPResults = tagGCP_RESULTSW;
  TGCPResultsW = tagGCP_RESULTSW;

  _OSVERSIONINFOA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar;
  end;
  OSVERSIONINFOA = _OSVERSIONINFOA;
  OSVERSIONINFO = OSVERSIONINFOA;
  TOSVersionInfoA = _OSVERSIONINFOA;
  TOSVersionInfo = TOSVersionInfoA;

const
  CCHDEVICENAME = 32;
  CCHFORMNAME   = 32;

type
  _devicemodeA = packed record
    dmDeviceName: array[0..CCHDEVICENAME - 1] of AnsiChar;
    dmSpecVersion: Word;
    dmDriverVersion: Word;
    dmSize: Word;
    dmDriverExtra: Word;
    dmFields: DWORD;
    dmOrientation: SHORT;
    dmPaperSize: SHORT;
    dmPaperLength: SHORT;
    dmPaperWidth: SHORT;
    dmScale: SHORT;
    dmCopies: SHORT;
    dmDefaultSource: SHORT;
    dmPrintQuality: SHORT;
    dmColor: SHORT;
    dmDuplex: SHORT;
    dmYResolution: SHORT;
    dmTTOption: SHORT;
    dmCollate: SHORT;
    dmFormName: array[0..CCHFORMNAME - 1] of AnsiChar;
    dmLogPixels: Word;
    dmBitsPerPel: DWORD;
    dmPelsWidth: DWORD;
    dmPelsHeight: DWORD;
    dmDisplayFlags: DWORD;
    dmDisplayFrequency: DWORD;
    dmICMMethod: DWORD;
    dmICMIntent: DWORD;
    dmMediaType: DWORD;
    dmDitherType: DWORD;
    dmICCManufacturer: DWORD;
    dmICCModel: DWORD;
    dmPanningWidth: DWORD;
    dmPanningHeight: DWORD;
  end;
  TDeviceModeA = _devicemodeA;
  PDeviceModeA = ^TDeviceModeA;
  PDeviceMode = PDeviceModeA;

  LPCSTR = PAnsiChar;
  LPWSTR = PWideChar;
  LPSTR = PAnsiChar;

  LCID = DWORD;

  HKL = type LongWord;

  LCTYPE = DWORD;

  PBOOL = ^BOOL;
{$ENDIF}

{$IFDEF VER2_0}
  TMonthNameArray = array[1..12] of string;
  TWeekNameArray = array[1..7] of string;

  TFormatSettings = record
    CurrencyFormat: Byte;
    NegCurrFormat: Byte;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    CurrencyDecimals: Byte;
    DateSeparator: Char;
    TimeSeparator: Char;
    ListSeparator: Char;
    CurrencyString: string;
    ShortDateFormat: string;
    LongDateFormat: string;
    TimeAMString: string;
    TimePMString: string;
    ShortTimeFormat: string;
    LongTimeFormat: string;
    ShortMonthNames: TMonthNameArray;
    LongMonthNames: TMonthNameArray;
    ShortDayNames: TWeekNameArray;
    LongDayNames: TWeekNameArray;
    TwoDigitYearCenturyWindow: Word;
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
{$ELSE}
  PDevMode = PDeviceMode;

  TWMDropFiles = packed record
    Msg: Cardinal;
    Drop: THANDLE;
    Unused: Longint;
    Result: Longint;
  end;
{$ENDIF}

  TCMGotFocus = TWMNoParams;
  TCMExit = TWMNoParams;


const
  WM_WININICHANGE = CM_WININICHANGE;
  WM_CANCELMODE = LM_CANCELMODE;
  WM_ERASEBKGND = LM_ERASEBKGND;
  WM_GETTEXTLENGTH = LM_GETTEXTLENGTH;
  WM_KEYDOWN = LM_KEYDOWN;
  WM_KILLFOCUS = LM_KILLFOCUS;
  WM_LBUTTONDOWN = LM_LBUTTONDOWN;
  WM_LBUTTONUP = LM_LBUTTONUP;
  WM_MOUSEMOVE = LM_MOUSEMOVE;
  WM_NCHITTEST = LM_NCHITTEST;
  WM_SETCURSOR = LM_SETCURSOR;
  WM_SETTEXT = $000C;
  WM_GETTEXT = $000D;
  WM_SETFOCUS = LM_SETFOCUS;
  WM_CHAR = LM_CHAR;
  WM_CLEAR = LM_CLEAR;
  WM_COPY = LM_COPY;
  WM_CUT = LM_CUT;
  WM_PASTE = LM_PASTE;
// With Lazarus versions prior to March 2008, LM_CLEAR, etc. are not defined,
//  so comment previous 4 lines and uncomment next 4 lines.
{
  WM_CLEAR = LM_CLEARSEL;
  WM_COPY = LM_COPYTOCLIP;
  WM_CUT = LM_CUTTOCLIP;
  WM_PASTE = LM_PASTEFROMCLIP;
}
  WM_GETDLGCODE = LM_GETDLGCODE;
  WM_SIZE = LM_SIZE;
  WM_SETFONT = LM_SETFONT;
  WM_SYSKEYDOWN = LM_SYSKEYDOWN;
  WM_RBUTTONUP = LM_RBUTTONUP;
  WM_MOUSEACTIVATE = $0021;
  WM_LBUTTONDBLCLK = LM_LBUTTONDBLCLK;
  WM_SETREDRAW = $000B;
  WM_NEXTDLGCTL = $0028;
  WM_MOUSEWHEEL = LM_MOUSEWHEEL;
  WM_PAINT = LM_PAINT;
  WM_VSCROLL = LM_VSCROLL;
  WM_HSCROLL = LM_HSCROLL;
  WM_NCPAINT = LM_NCPAINT;
  WM_MEASUREITEM = LM_MEASUREITEM;

  EM_GETMODIFY = $00B8;
  EM_SETMODIFY = $00B9;
  EM_GETSEL = $00B0;
  EM_SETSEL = $00B1;
  EM_GETLINECOUNT = $00BA;
  EM_LINELENGTH = $00C1;
  EM_LINEINDEX = $00BB;
  EM_GETLINE = $00C4;
  EM_REPLACESEL = $00C2;

  CS_SAVEBITS = $800;
  CS_DBLCLKS = 8;
  SPI_GETWORKAREA = 48;
  SPI_GETNONCLIENTMETRICS = 41;
  DLGC_STATIC = $100;
  GW_HWNDLAST = 1;
  GW_HWNDNEXT = 2;
  GW_HWNDPREV = 3;
  GW_CHILD = 5;
  DT_EXPANDTABS = $40;
  DT_END_ELLIPSIS = $8000;
  DT_MODIFYSTRING = $10000;
  GHND = 66;
  TMPF_TRUETYPE = 4;
  SWP_HIDEWINDOW = $80;
  SWP_SHOWWINDOW = $40;
  RDW_INVALIDATE = 1;
  RDW_UPDATENOW = $100;
  RDW_FRAME = $400;
  LANG_JAPANESE = $11;
  ES_PASSWORD = $20;
  ES_LEFT = 0;
  ES_RIGHT = 2;
  ES_CENTER = 1;
  ES_AUTOHSCROLL = $80;
  ES_MULTILINE = 4;
  ODS_COMBOBOXEDIT = $1000;
  CB_FINDSTRING = $014C;
  CB_SETITEMHEIGHT = $0153;
  CB_FINDSTRINGEXACT = $0158;
  CB_SETDROPPEDWIDTH = 352;
  CBS_DROPDOWN = 2;
  CBS_DROPDOWNLIST = 3;
  CBS_OWNERDRAWVARIABLE = $20;
  CBS_AUTOHSCROLL = $40;
  CBS_HASSTRINGS = $200;
  WHEEL_DELTA = 120;
  LB_GETCARETINDEX = $019F;
  LB_GETCOUNT = $018B;
  LB_GETCURSEL = $0188;
  LB_GETITEMHEIGHT = $01A1;
  LB_GETITEMRECT = $0198;
  LB_GETSEL = $0187;
  LB_GETTOPINDEX = $018E;
  LB_RESETCONTENT = $0184;
  LB_SELITEMRANGE = $019B;
  LB_SETCURSEL = $0186;
  LB_SETSEL = $0185;
  LB_SETTABSTOPS = $0192;
  LB_SETTOPINDEX = $0197;
  LB_ERR = -1;
  MA_ACTIVATE = 1;
  MA_NOACTIVATEANDEAT = 4;

  MB_PRECOMPOSED = 1;
  MB_USEGLYPHCHARS = 4;
  WC_DISCARDNS = $10;
  WC_SEPCHARS = $20;
  WC_DEFAULTCHAR = $40;
  WC_COMPOSITECHECK = $200;
  GM_ADVANCED = 2;
  GCP_REORDER = 2;
  GCP_USEKERNING = 8;
  GCP_LIGATE = 32;
  GCP_DISPLAYZWG = $400000;
  MM_ANISOTROPIC = 8;
  MWT_LEFTMULTIPLY = 2;
  WM_TIMER = LM_TIMER;
  WM_DROPFILES = LM_DROPFILES;
  WM_IME_STARTCOMPOSITION = $010D;
  WM_IME_ENDCOMPOSITION   = $010E;
  WM_IME_COMPOSITION      = $010F;
  WM_MOUSEFIRST = LM_MOUSEFIRST;
  WM_MOUSELAST = LM_MOUSELAST;
  WM_KEYFIRST = LM_KEYFIRST;
  WM_KEYLAST = LM_KEYLAST;
  CF_UNICODETEXT = 13;
  CF_ENHMETAFILE = 14;
  MM_TWIPS = 6;
  GMEM_MOVEABLE = 2;
  GMEM_DDESHARE = $2000;
  GMEM_ZEROINIT = $40;
  EM_GETRECT = $00B2;
  EM_SETRECTNP = $00B4;
  MB_TASKMODAL = $00002000;
  PHYSICALOFFSETX = 112;
  PHYSICALOFFSETY = 113;

  BM_SETCHECK = $00F1;
  PLANES = 14;
  NUMCOLORS = 24;
  STRETCH_DELETESCANS = 3;
  CP_ACP = 0;  {ANSI code page}
  CP_OEMCP = 1;  {OEM code page }
  CP_MACCP = 2;  {MAC code page }
  HeapAllocFlags = GMEM_MOVEABLE;  {2}
  CP_UTF8 = 65001;
  RDH_RECTANGLES = 1;
  MAXLONG = $7FFFFFFF;
  VER_PLATFORM_WIN32_WINDOWS = 1;
{$IFNDEF MSWINDOWS}
  Win32Platform = 2;  //Set as though Windows NT (VER_PLATFORM_WIN32_NT)
  Win32MinorVersion = 0;
{$ENDIF}

type
  PWCHAR = PWideChar;
  PXForm = ^TXForm;

{$IFDEF MSWINDOWS}
  TRgnDataHeader = Windows.TRGNDATAHEADER;
{$ELSE}
  _RGNDATAHEADER = packed record
    dwSize: DWORD;
    iType: DWORD;
    nCount: DWORD;
    nRgnSize: DWORD;
    rcBound: TRect;
  end;
  TRgnDataHeader = _RGNDATAHEADER;
{$ENDIF}

{$IFDEF MSWINDOWS}
  PRgnData = Windows.PRGNDATA;
  TRgnData = Windows.TRGNDATA;
{$ELSE}
  RGNDATA = record
    rdh: TRgnDataHeader;
    Buffer: array[0..0] of CHAR;
    Reserved: array[0..2] of CHAR;
  end;
  PRgnData = ^TRgnData;
  TRgnData = RGNDATA;
{$ENDIF}

{$IFDEF MSWINDOWS}
  TLogBrush = Windows.LOGBRUSH;
{$ENDIF}


 {These belong in LclIntf unit}
function GetTickCount : DWORD;
{$IFNDEF MSWINDOWS}
function GetSystemMetrics(nIndex: Integer): Integer;
{$ENDIF}
procedure OutputDebugString(lpOutputString: PChar);
function GlobalAlloc(uFlags: UINT; dwBytes: DWORD): HGLOBAL;
function GlobalLock(hMem: HGLOBAL): Pointer;
function GlobalUnlock(hMem: HGLOBAL): BOOL;
function GlobalFree(hMem: HGLOBAL): HGLOBAL;
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;

function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD;
                             const lpMultiByteStr: LPCSTR; cchMultiByte: Integer;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer;
function WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer;
                             lpMultiByteStr: LPSTR; cchMultiByte: Integer;
                             lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer;
function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
function CharLowerBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer;
                               var Size: TSize): BOOL;
function SetTextAlign(DC: HDC; Flags: UINT): UINT;
function GetMapMode(DC: HDC): Integer;
function SetMapMode(DC: HDC; p2: Integer): Integer;
function SetViewportExtEx(DC: HDC; XExt, YExt: Integer; Size: PSize): BOOL;
function GetViewportExtEx(DC: HDC; var Size: TSize): BOOL;
function GetWindowOrgEx(DC: HDC; var Point: TPoint): BOOL;
function SetWindowExtEx(DC: HDC; XExt, YExt: Integer; Size: PSize): BOOL;
function GetWindowExtEx(DC: HDC; var Size: TSize): BOOL;
{$IFNDEF MSWINDOWS}
function GetDeviceCaps(DC: HDC; Index: Integer): Integer;
{$ENDIF}
function TextOutW(DC: HDC; X, Y: Integer; Str: PWideChar; Count: Integer): BOOL;
function ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
                     Str: PWideChar; Count: Longint; Dx: PInteger): BOOL;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer;
                   var lpRect: TRect; uFormat: UINT): Integer;
function PatBlt(DC: HDC; X, Y, Width, Height: Integer; Rop: DWORD): BOOL;
function SetTextJustification(DC: HDC; BreakExtra, BreakCount: Integer): Integer;
function GetBrushOrgEx(DC: HDC; var lppt: TPoint): BOOL;
function SetBrushOrgEx(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL;
function timeGetTime: DWORD;
function GetTextExtentExPointW(DC: HDC; p2: PWideChar; p3, p4: Integer;
                               p5, p6: PInteger; var p7: TSize): BOOL;
function GetTempPath(nBufferLength: DWORD; lpBuffer: PChar): DWORD;
function CharNextEx(CodePage: Word; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR;
function ExtCreateRegion(XForm: PXForm; Count: DWORD; const RgnData: TRgnData): HRGN;
function ExtCreatePen(PenStyle, Width: DWORD; const Brush: TLogBrush;
                      StyleCount: DWORD; Style: Pointer): HPEN;
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
{$IFDEF MSWINDOWS}
  Result := Windows.GetTickCount;
{$ELSE}
  Result := LclIntf.GetTickCount;
{$ENDIF}
end;

{$IFNDEF MSWINDOWS}
function GetSystemMetrics(nIndex: Integer): Integer;
// SM_CYBORDER, etc. not implemented yet in GTK widgetset.
begin
  if nIndex = SM_SWAPBUTTON then
    Result := 0  //Not implemented on GTK, so assume buttons not swapped.
  else
    begin
    if nIndex = SM_CYBORDER then
      nIndex := SM_CYEDGE;  //Substitute for now so returned value is valid.
    Result := LclIntf.GetSystemMetrics(nIndex);
    end;
end;
{$ENDIF}

procedure OutputDebugString(lpOutputString: PChar);
begin
{$IFDEF MSWINDOWS}
  Windows.OutputDebugString(lpOutputString);
{$ENDIF}
end;

function GlobalAlloc(uFlags: UINT; dwBytes: DWORD): HGLOBAL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GlobalAlloc(uFlags, dwBytes);
{$ELSE}
  Result := HGLOBAL(GetMem(dwBytes));  {Treating pointer to memory as "handle"}
{$ENDIF}
end;

function GlobalLock(hMem: HGLOBAL): Pointer;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GlobalLock(hMem);
{$ELSE}
  Result := Pointer(hMem);  {"Handle" is pointer to memory}
{$ENDIF}
end;

function GlobalUnlock(hMem: HGLOBAL): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GlobalUnlock(hMem);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function GlobalFree(hMem: HGLOBAL): HGLOBAL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GlobalFree(hMem);
{$ELSE}
  FreeMem(Pointer(hMem));  {"Handle" is pointer to memory}
  Result := 0;
{$ENDIF}
end;

function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
 {Use control's Perform method to force it to respond to posted message.
  This doesn't work:  Result := LclIntf.PostMessage(hWnd, Msg, wParam, lParam); }
var
  AWinControl : TWinControl;
begin
  Assert(hWnd <> 0, 'Window handle not assigned on entry to PostMessage');
  AWinControl := FindOwnerControl(hWnd);
//  Assert(AWinControl <> nil,
//         'Owner control not found in PostMessage ($' + IntToHex(Msg, 4) + ') ');
  if AWinControl <> nil then
    AWinControl.Perform(Msg, wParam, lParam);
  Result := True;
end;

function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
 {Use control's Perform method to force it to respond to sent message.
  This doesn't work: Result := LclIntf.SendMessage(hWnd, Msg, wParam, lParam); }
var
  AWinControl : TWinControl;
begin
  Assert(hWnd <> 0, 'Window handle not assigned on entry to SendMessage');
  AWinControl := FindOwnerControl(hWnd);
//  Assert(AWinControl <> nil,
//         'Owner control not found in SendMessage ($' + IntToHex(Msg, 4) + ') ');
  if AWinControl <> nil then
    Result := AWinControl.Perform(Msg, wParam, lParam);
end;

function MultiByteToWideChar(CodePage: UINT; dwFlags: DWORD;
                             const lpMultiByteStr: LPCSTR; cchMultiByte: Integer;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.MultiByteToWideChar(CodePage, dwFlags, lpMultiByteStr,
                                        cchMultiByte, lpWideCharStr, cchWideChar);
{$ELSE}
var
  s : string;
  w : WideString;
begin
  if cchMultiByte < 0 then  {Null terminated?}
    s := lpMultiByteStr
  else
    begin
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
{$ENDIF}
end;

function WideCharToMultiByte(CodePage: UINT; dwFlags: DWORD;
                             lpWideCharStr: LPWSTR; cchWideChar: Integer;
                             lpMultiByteStr: LPSTR; cchMultiByte: Integer;
                             lpDefaultChar: LPCSTR; lpUsedDefaultChar: PBOOL): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.WideCharToMultiByte(CodePage, dwFlags, lpWideCharStr,
                                        cchWideChar, lpMultiByteStr, cchMultiByte,
                                        lpDefaultChar, lpUsedDefaultChar);
{$ELSE}
var
  w : WideString;
  s : string;
begin
  if cchWideChar < 0 then  {Null terminated?}
    w := lpWideCharStr
  else  {Specifies number of wide chars to convert}
    begin
    SetLength(w, cchWideChar);
    Move(lpWideCharStr^, w[1], cchWideChar*2);
    end;
  s := WideCharToString(PWideChar(w));
  Result := Length(s);
  if cchWideChar < 0 then  {Include terminating null too?}
    Inc(Result);
  if cchMultiByte > 0 then  {Okay to return string?}
    Move(s[1], lpMultiByteStr^, Result);  {Assume dest. buffer has enough space}
{$ENDIF}
end;

function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.CharUpperBuffw(lpsz, cchLength);
{$ELSE}
var
  w : WideString;
begin
  SetLength(w, cchLength);
  Move(lpsz^, w[1], cchLength*2);
  w := WideUpperCase(w);
  Move(w[1], lpsz^, cchLength*2);
  Result := cchLength;
{$ENDIF}
end;

function CharLowerBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.CharLowerBuffw(lpsz, cchLength);
{$ELSE}
var
  w : WideString;
begin
  SetLength(w, cchLength);
  Move(lpsz^, w[1], cchLength*2);
  w := WideLowerCase(w);
  Move(w[1], lpsz^, cchLength*2);
  Result := cchLength;
{$ENDIF}
end;

function GetTextExtentPointW(DC: HDC; Str: PWideChar; Count: Integer;
                             var Size: TSize): BOOL;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.GetTextExtentPointW(DC, Str, Count, Size);
{$ELSE}
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
{$ENDIF}
end;

function GetTextExtentPoint32W(DC: HDC; Str: PWideChar; Count: Integer;
                               var Size: TSize): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTextExtentPoint32W(DC, Str, Count, Size);
{$ELSE}
  Result := GetTextExtentPointW(DC, Str, Count, Size);  //No Point32W function
{$ENDIF}
end;

{$IFNDEF MSWINDOWS}
var
  CurTextAlign : UINT;
  CurTA_DC     : HDC;
{$ENDIF}

function GetTextAlign(DC: HDC): UINT;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTextAlign(DC);
{$ELSE}
  if DC = CurTA_DC then
    Result := CurTextAlign
  else
    Result := 0;
{$ENDIF}
end;

function SetTextAlign(DC: HDC; Flags: UINT): UINT;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.SetTextAlign(DC, Flags);
{$ELSE}
   {Save the most recently set DC's text alignment flags with the
     assumption that usually working with just one DC at a time that
     has non-default alignment flags. (Better solution would be to
     save each DC's alignment flags in a collection or something.)
     Use these flags in TextOut and ExtTextOut to implement.}
  Result := GetTextAlign(DC);
  CurTextAlign := Flags;
  CurTA_DC := DC;
{$ENDIF}
end;

function GetMapMode(DC: HDC): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetMapMode(DC);
{$ELSE}
//  WriteLn('GetMapMode not implemented yet');
{$ENDIF}
end;

function SetMapMode(DC: HDC; p2: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.SetMapMode(DC, p2);
{$ELSE}
//  WriteLn('SetMapMode not implemented yet');
{$ENDIF}
end;

function SetViewportExtEx(DC: HDC; XExt, YExt: Integer; Size: PSize): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.SetViewportExtEx(DC, XExt, YExt, Size);
{$ELSE}
//  Result := LclIntf.SetViewportExtEx(DC, XExt, YExt, Size);
  Result := True;
{$ENDIF}
end;

function GetViewportExtEx(DC: HDC; var Size: TSize): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetViewportExtEx(DC, Size);
{$ELSE}  //Since normally used with GetWindowExtEx, just return 1 for now.
  Size.cx := 1;
  Size.cy := 1;
  Result := True;
{$ENDIF}
end;

function GetWindowOrgEx(DC: HDC; var Point: TPoint): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetWindowOrgEx(DC, Point);
{$ELSE}
  if DC <> 1 then
    Result := BOOL(LclIntf.GetWindowOrgEx(DC, @Point))
  else  //Assume dummy DC is for CUPS printer canvas.
    begin
    Point.X := 0;
    Point.Y := 0;
    Result := True;
    end;
{$ENDIF}
end;

function SetWindowExtEx(DC: HDC; XExt, YExt: Integer; Size: PSize): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.SetWindowExtEx(DC, XExt, YExt, Size);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function GetWindowExtEx(DC: HDC; var Size: TSize): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetWindowExtEx(DC, Size);
{$ELSE}  //Since normally used with GetViewportExtEx, just return 1 for now.
  Size.cx := 1;
  Size.cy := 1;
  Result := True;
{$ENDIF}
end;

{$IFNDEF MSWINDOWS}
function GetDeviceCaps(DC: HDC; Index: Integer): Integer;
begin
  if DC <> 1 then
    begin
     {First check for Index values that may not be implemented in widgetset}
    if Index = PLANES then
      Result := 1
    else if Index = NUMCOLORS then
      Result := 100  {Return large enough value so not BxW device}
    else
      Result := LclIntf.GetDeviceCaps(DC, Index);
    end
  else  //Assume dummy DC is for CUPS printer canvas.
    begin
    case Index of
      LOGPIXELSX : Result := Printer.XDPI;
      LOGPIXELSY : Result := Printer.YDPI;
      PHYSICALOFFSETX : Result := Printer.PaperSize.PaperRect.WorkRect.Left;
      PHYSICALOFFSETY : Result := Printer.PaperSize.PaperRect.WorkRect.Top;
      end;
    end;
end;
{$ENDIF}

function TextOutW(DC: HDC; X, Y: Integer; Str: PWideChar; Count: Integer): BOOL;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.TextOutW(DC, X, Y, Str, Count);
{$ELSE}
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
{$ENDIF}
end;

function ExtTextOutW(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
                     Str: PWideChar; Count: Longint; Dx: PInteger): BOOL;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.ExtTextOutW(DC, X, Y, Options, Rect, Str, Count, Dx);
{$ELSE}
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
{$ENDIF}
end;

function DrawTextW(hDC: HDC; lpString: PWideChar; nCount: Integer;
                   var lpRect: TRect; uFormat: UINT): Integer;
{$IFDEF MSWINDOWS}
begin
  Result := Windows.DrawTextW(hDC, lpString, nCount, lpRect, uFormat);
{$ELSE}
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
{$ENDIF}
end;

function PatBlt(DC: HDC; X, Y, Width, Height: Integer; Rop: DWORD): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.PatBlt(DC, X, Y, Width, Height, Rop);
{$ELSE}
  WriteLn('PatBlt not implemented yet');
{$ENDIF}
end;

function SetTextJustification(DC: HDC; BreakExtra, BreakCount: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := Integer(Windows.SetTextJustification(DC, BreakExtra, BreakCount));
{$ELSE}
//  WriteLn('SetTextJustification not implemented yet');
{$ENDIF}
end;

function GetBrushOrgEx(DC: HDC; var lppt: TPoint): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetBrushOrgEx(DC, lppt);
{$ELSE}
  WriteLn('GetBrushOrgEx not implemented yet');
{$ENDIF}
end;

function SetBrushOrgEx(DC: HDC; X, Y: Integer; PrevPt: PPoint): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.SetBrushOrgEx(DC, X, Y, PrevPt);
{$ELSE}
  WriteLn('SetBrushOrgEx not implemented yet');
{$ENDIF}
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
{$IFDEF MSWINDOWS}
  Result := Windows.GetTextExtentExPointW(DC, p2, p3, p4, p5, p6, p7);
{$ELSE}  //Don't need if use GetTextExtentPoint32W
  WriteLn('GetTextExtentExPointW not implemented yet');
{$ENDIF}
end;

function GetTempPath(nBufferLength: DWORD; lpBuffer: PChar): DWORD;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.GetTempPath(nBufferLength, lpBuffer);
{$ELSE}
  if Length(GetTempDir) >= nBufferLength then  {Buffer not big enough?}
    begin
    Move(GetTempDir[1], lpBuffer, nBufferLength);
    Result := Length(GetTempDir)+1;
    end
  else
    begin
    Move(GetTempDir[1], lpBuffer, Length(GetTempDir)+1);  //Include terminating null
    Result := Length(GetTempDir);
    end;
{$ENDIF}
end;

function CharNextEx(CodePage: Word; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.CharNextExA(CodePage, lpCurrentChar, dwFlags);  //Note "A"
{$ELSE}
  Result := lpCurrentChar + 1;  //For now.
{$ENDIF}
end;

function ExtCreateRegion(XForm: PXForm; Count: DWORD; const RgnData: TRgnData): HRGN;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.ExtCreateRegion(XForm, Count, RgnData);
{$ELSE}
  WriteLn('ExtCreateRegion not implemented yet');
{$ENDIF}
end;

function ExtCreatePen(PenStyle, Width: DWORD; const Brush: TLogBrush;
                      StyleCount: DWORD; Style: Pointer): HPEN;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.ExtCreatePen(PenStyle, Width, Brush, StyleCount, Style);
{$ELSE}
  WriteLn('ExtCreatePen not implemented yet');
{$ENDIF}
end;

function BeginPath(DC: HDC): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.BeginPath(DC);
{$ELSE}
  WriteLn('BeginPath not implemented yet');
{$ENDIF}
end;

function EndPath(DC: HDC): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.EndPath(DC);
{$ELSE}
  WriteLn('EndPath not implemented yet');
{$ENDIF}
end;

function StrokePath(DC: HDC): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.StrokePath(DC);
{$ELSE}
  WriteLn('StrokePath not implemented yet');
{$ENDIF}
end;

function CloseFigure(DC: HDC): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.CloseFigure(DC);
{$ELSE}
  WriteLn('CloseFigure not implemented yet');
{$ENDIF}
end;

function ClipCursor(lpRect: PRect): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.ClipCursor(lpRect);
{$ELSE}
  WriteLn('ClipCursor not implemented yet');
{$ENDIF}
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

