{
Version   11.10
Copyright (c) 2008-2023 by HtmlViewer Team

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

unit HtmlGlobals;

{$I htmlcons.inc}

interface

uses
{$ifdef HasSystemUITypes}
  System.UITypes,
{$endif}
{$ifdef UseVCLStyles}
  Vcl.Themes,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  Classes, SysUtils, Graphics, Controls,
{$ifdef LCL}
  LclIntf, LclType, LCLVersion, Types, Messages,
  StdCtrls, Buttons, Base64, Dialogs, Process,
  HtmlMisc,
  {$ifdef DebugIt}
    {$message 'HtmlViewer uses LCL standard controls.'}
  {$endif}
{$else}
  Consts, StrUtils, StdCtrls, ShellAPI,
  {$ifdef UseTNT}
    {$ifdef DebugIt}
      {$message 'HtmlViewer uses TNT unicode controls.'}
    {$endif}
    Messages,
    TntControls,
    TntStdCtrls,
    {$ifdef Compiler18_Plus}
      WideStrings,
    {$else}
      TntWideStrings,
    {$endif}
    TntClasses,
  {$else UseTNT}
    {$ifdef DebugIt}
      {$message 'HtmlViewer uses VCL standard controls.'}
    {$endif}
    Buttons,
    Messages,
    {$ifdef Compiler18_Plus}
      WideStrings,
    {$else}
      TntWideStrings,
      TntClasses,
    {$endif}
  {$endif UseTNT}
{$endif}
  Forms,
  Clipbrd,
  Math;

const
{$ifndef LCL}
  lcl_fullversion = Integer(0);
  fpc_fullversion = Integer(0);
{$endif}
{$ifndef MSWindows}
  //Charsets defined in unit Windows:
  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  //JOHAB_CHARSET       = 130;
  GB2312_CHARSET      = 134;
  CHINESEBIG5_CHARSET = 136;
  GREEK_CHARSET       = 161;
  TURKISH_CHARSET     = 162;
  //VIETNAMESE_CHARSET  = 163;
  HEBREW_CHARSET      = 177;
  ARABIC_CHARSET      = 178;
  RUSSIAN_CHARSET     = 204;
  THAI_CHARSET        = 222;
  EASTEUROPE_CHARSET  = 238;
  OEM_CHARSET         = 255;
{$endif MSWindows}
  // more charset constants
  UNKNOWN_CHARSET = -1;

  // some more codepage constants
  CP_UNKNOWN = -1;
  CP_UTF16LE = 1200;
  CP_UTF16BE = 1201;
  CP_ISO2022JP = 50220;


type
{$IFNDEF DOTNET}
  {$IFNDEF FPC}
    {$ifndef PtrInt_defined}
      {$define PtrInt_defined}
       //needed so that in FreePascal, we can use pointers of different sizes
      {$IFDEF WIN32}
        PtrInt = LongInt;
        PtrUInt = LongWord;
      {$ENDIF}
      {$IFDEF WIN64}
        PtrInt = Int64;
        PtrUInt = Int64;
      {$ENDIF}
      //NOTE:  The code below asumes a 32bit Linux architecture (such as target i386-linux)
      {$IFDEF KYLIX}
        PtrInt = LongInt;
        PtrUInt = LongWord;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$ifdef DebugIt}
  {$ifdef UNICODE}
    {$message 'Compiler uses unicode by default.'}
  {$else}
    {$message 'Compiler uses single byte chars by default.'}
  {$endif}

  {$message 'HtmlViewer uses unicode.'}
{$endif}

{$ifdef FPC}
{$else}
  {$ifdef Compiler18_Plus}
  {$else}
    TWideStringList = class(TTntStringList);
  {$endif}
{$endif}

{$ifdef UNICODE}
  ThtChar = Char;
  ThtString = string;
  ThtStrings = TStrings;
  ThtStringList = TStringList;
  PhtChar = PChar;
{$else}
  ThtChar = WideChar;
  PhtChar = PWideChar;
  {$if fpc_fullversion < 30000}
    UnicodeString = WideString;
    ThtString = WideString;
    ThtStrings = TWideStrings;
    ThtStringList = TWideStringList;
  {$else}
    ThtString = UnicodeString;
    ThtStrings = TUnicodeStrings;
    ThtStringList = TUnicodeStringList;
  {$ifend}
{$endif}

  ThtCharArray = array of ThtChar;
  ThtStringArray = array of ThtString;
  ThtIntegerArray = array of Integer;

  ThtEdit = class({$ifdef UseTNT} TTntEdit {$else} TEdit {$endif})
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
{$ifndef TEditHasTextHint}
  private
    FTextHint: ThtString;
    FCanvas : TControlCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TextHint: ThtString read FTextHint write FTextHint;
{$endif}
  end;

  ThtComboBox = class({$ifdef UseTNT} TTntComboBox {$else} TComboBox {$endif})
{$ifndef TComboBoxHasTextHint}
  private
    FTextHint: ThtString;
    FCanvas : TControlCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TextHint: ThtString read FTextHint write FTextHint;
{$endif}
  end;

  {Hack solution based on:
  http://stackoverflow.com/questions/1465845/cuetext-equivalent-for-a-tmemo
  }
  ThtMemo = class({$ifdef UseTNT} TTntMemo {$else} TMemo {$endif})
  private
    FTextHint: TStrings;
    FCanvas : TControlCanvas;
    FMaxLength : Integer;
    procedure SetMaxLength(const AValue: Integer);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TextHint: TStrings read FTextHint {write FTextHint};  // BG, 29.07.2013: "write FTextHint" would produce memory leaks!!!
    property MaxLength: Integer read FMaxLength write SetMaxLength;
  end;

{$ifdef UseTNT}
  ThtButton = TTntButton;
  ThtMemoBase = TTntMemo;
  ThtListbox = TTntListBox;
  ThtCheckBox = TTntCheckBox;
  ThtRadioButton = TTntRadioButton;
  ThtHintWindow = TTntHintWindow;
{$else}
  ThtButton = TBitBtn; //BG, 25.12.2010: TBitBtn uses correct charset, but TButton does not.
  ThtListbox = TListbox;
  ThtCheckBox = TCheckBox;
  ThtRadioButton = TRadioButton;
  ThtHintWindow = THintWindow;
{$endif}

{$ifdef HasSystemUITypes}
   ThtScrollStyle = System.UITypes.TScrollStyle;
{$else}
  ThtScrollStyle = TScrollStyle;
{$endif}

  ThtStyleElement = (
    htseFont,
    htseClient,
    htseBorder
  );

  ThtThemedColor = function (AColor: TColor; AStyleElement: ThtStyleElement): TColor  of object;

  //BG, 05.02.2023: don't add virtual methods or fields. It is only used to access protected stuff of TCustomForm.
  ThtCustomForm = class(TCustomForm)
  public
    property PixelsPerInch;
  end;

  //BG, 10.12.2010: don't add virtual methods or fields. It is only used to access protected stuff of TCanvas.
  ThtCanvas = class(TCanvas)
  public
    procedure htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
  end;

  ThtGraphic = class(TGraphic)
{$ifdef LCL}
  private
    FTransparent: Boolean;
  protected
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
{$endif}
  end;

  { ThtBitmap }

  ThtBitmap = class(TBitmap)
  private
    procedure SetMask(AValue: TBitmap);
    function GetMask: TBitmap;
    procedure SetTransparentMask(AValue: Boolean);
  protected
    FMask: TBitmap;
    FTransparent: Boolean;
  public
    constructor Create(WithTransparentMask: Boolean = False); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const DestRect: TRect); override;
    procedure StretchDraw(ACanvas: TCanvas; const DestRect, SrcRect: TRect);
    property BitmapMask: TBitmap read GetMask write SetMask;
    property WithTransparentMask: Boolean read FTransparent write SetTransparentMask;
  end;

const
  EofChar     = ThtChar(#0);
  TabChar     = ThtChar(#9);
  LfChar      = ThtChar(#10);
  FfChar      = ThtChar(#12);
  CrChar      = ThtChar(#13);
  SpcChar     = ThtChar(' ');
  NbSpcChar   = ThtChar(#160);  // Unbreakable Space, Unicode value for Html entity &nbsp;
  DotChar     = ThtChar('.');
  LessChar    = ThtChar('<');
  MinusChar   = ThtChar('-');
  GreaterChar = ThtChar('>');
  PercentChar = ThtChar('%');
  AmperChar   = ThtChar('&');
  StarChar    = ThtChar('*');
  CrLf        = ThtString(#13#10);
  CrLfTab     = ThtString(#13#10#9);
  NullRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

{$ifdef MSWindows}
  FontSerif   = ThtString('Times New Roman');
  FontMono    = ThtString('Courier New');
  FontSans    = ThtString('Arial');
  FontHelvet  = ThtString('Arial');
  FontCursive = ThtString('Lucida Handwriting');
{$endif MSWindows}
{$ifdef Linux}
  FontSerif   = ThtString('Serif');
  FontMono    = ThtString('Monospace');
  FontSans    = ThtString('Sans');
  FontHelvet  = ThtString('Sans');
  FontCursive = ThtString('Sans');
{$endif Linux}
{$ifdef Darwin}
  FontSerif   = ThtString('Times');
  FontMono    = ThtString('Courier');
  FontSans    = ThtString('Helvetica');
  FontHelvet  = ThtString('Helvetica');
  FontCursive = ThtString('Apple Chancery');
{$endif Darwin}

{$ifdef LCL}
const
  HWND_MESSAGE = HWND(-3);

  //ANSI_CHARSET            = 0;       // ANSI charset (Windows-1252)
  //DEFAULT_CHARSET         = 1;
  //SYMBOL_CHARSET          = 2;
  MAC_CHARSET             = 77;
  //SHIFTJIS_CHARSET        = 128;     // Shift JIS charset    (Windows-932)
  //HANGEUL_CHARSET         = 129;     // Hangeul charset      (Windows-949)
  JOHAB_CHARSET           = 130;     // Johab charset        (Windows-1361)
  //GB2312_CHARSET          = 134;     // GB2312 charset       (Windows-936)
  //CHINESEBIG5_CHARSET     = 136;     // Chinese Big5 charset (Windows-950)
  //GREEK_CHARSET           = 161;     // Greek charset        (Windows-1253)
  //TURKISH_CHARSET         = 162;     // Turkish charset      (Windows-1254)
  VIETNAMESE_CHARSET      = 163;     // Vietnamese charset   (Windows-1258)
  //HEBREW_CHARSET          = 177;     // Hebrew charset       (Windows-1255)
  //ARABIC_CHARSET          = 178;     // Arabic charset       (Windows-1256)
  //BALTIC_CHARSET          = 186;     // Baltic charset       (Windows-1257)
  //RUSSIAN_CHARSET         = 204;     // Cyrillic charset     (Windows-1251)
  //THAI_CHARSET            = 222;     // Thai charset         (Windows-874)
  //EASTEUROPE_CHARSET      = 238;     // Eastern european charset (Windows-1250)
  //OEM_CHARSET             = 255;

const
  { Global Memory Flags }

  GMEM_FIXED = 0;
  {$EXTERNALSYM GMEM_FIXED}
  GMEM_MOVEABLE = 2;
  {$EXTERNALSYM GMEM_MOVEABLE}
  GMEM_NOCOMPACT = $10;
  {$EXTERNALSYM GMEM_NOCOMPACT}
  GMEM_NODISCARD = $20;
  {$EXTERNALSYM GMEM_NODISCARD}
  GMEM_ZEROINIT = $40;
  {$EXTERNALSYM GMEM_ZEROINIT}
  GMEM_MODIFY = $80;
  {$EXTERNALSYM GMEM_MODIFY}
  GMEM_DISCARDABLE = $100;
  {$EXTERNALSYM GMEM_DISCARDABLE}
  GMEM_NOT_BANKED = $1000;
  {$EXTERNALSYM GMEM_NOT_BANKED}
  GMEM_SHARE = $2000;
  {$EXTERNALSYM GMEM_SHARE}
  GMEM_DDESHARE = $2000;
  {$EXTERNALSYM GMEM_DDESHARE}
  GMEM_NOTIFY = $4000;
  {$EXTERNALSYM GMEM_NOTIFY}
  GMEM_LOWER = GMEM_NOT_BANKED;
  {$EXTERNALSYM GMEM_LOWER}
  GMEM_VALID_FLAGS = 32626;
  {$EXTERNALSYM GMEM_VALID_FLAGS}
  GMEM_INVALID_HANDLE = $8000;
  {$EXTERNALSYM GMEM_INVALID_HANDLE}

  GHND = GMEM_MOVEABLE or GMEM_ZEROINIT;
  {$EXTERNALSYM GHND}
  GPTR = GMEM_FIXED or GMEM_ZEROINIT;
  {$EXTERNALSYM GPTR}

  // device caps
  {$EXTERNALSYM SHADEBLENDCAPS}
  SHADEBLENDCAPS = 45;
  {$EXTERNALSYM SB_CONST_ALPHA}
  SB_CONST_ALPHA = 1;

{
const
  HeapAllocFlags = GMEM_MOVEABLE;

type
  // missing in unit Graphics
  TProgressStage = (
    psStarting,
    psRunning,
    psEnding
  );
}

procedure DecodeStream(Input, Output: TStream);

function PromptForFileName(var AFileName: string; const AFilter: string = '';
  const ADefaultExt: string = ''; const ATitle: string = '';
  const AInitialDir: string = ''; SaveDialog: Boolean = False): Boolean;

{$else}

// Open a document with the default application associated with it in the system
function OpenDocument(const APath: ThtString): Boolean;

{$endif}

function StartProcess(const ApplicationName, Params: string; ShowWindow: Word = SW_SHOW): Boolean;

{$ifndef Compiler17_Plus}
type
  TBytes = array of byte;
{$endif}

var
  IsWin95: Boolean;
  IsWin32Platform: Boolean; {win95, 98, ME}
  ColorBits: Byte;
  ThePalette: HPalette; {the rainbow palette for 256 colors}
  PalRelative: integer;

const
{DarkerColors and LighterColors need to be in the interface section for inlining to work.}
  DarkerColors: array [0..255] of Byte = (
      0,   1,   1,   2,   3,   3,   4,   4,   5,   6,   6,   7,   8,   8,   9,   9,
     10,  11,  11,  12,  13,  13,  14,  14,  15,  16,  16,  17,  18,  18,  19,  19,
     20,  21,  21,  22,  23,  23,  24,  24,  25,  26,  26,  27,  28,  28,  29,  29,
     30,  31,  31,  32,  33,  33,  34,  35,  35,  36,  36,  37,  38,  38,  39,  40,
     40,  41,  41,  42,  43,  43,  44,  45,  45,  46,  46,  47,  48,  48,  49,  50,
     50,  51,  51,  52,  53,  53,  54,  55,  55,  56,  56,  57,  58,  58,  59,  60,
     60,  61,  61,  62,  63,  63,  64,  65,  65,  66,  67,  67,  68,  68,  69,  70,
     70,  71,  72,  72,  73,  73,  74,  75,  75,  76,  77,  77,  78,  78,  79,  80,
     80,  81,  82,  82,  83,  83,  84,  85,  85,  86,  87,  87,  88,  88,  89,  90,
     90,  91,  92,  92,  93,  93,  94,  95,  95,  96,  97,  97,  98,  99,  99, 100,
    100, 101, 102, 102, 103, 104, 104, 105, 105, 106, 107, 107, 108, 109, 109, 110,
    110, 111, 112, 112, 113, 114, 114, 115, 115, 116, 117, 117, 118, 119, 119, 120,
    120, 121, 122, 122, 123, 124, 124, 125, 125, 126, 127, 127, 128, 129, 129, 130,
    131, 131, 132, 132, 133, 134, 134, 135, 136, 136, 137, 137, 138, 139, 139, 140,
    141, 141, 142, 142, 143, 144, 144, 145, 146, 146, 147, 147, 148, 149, 149, 150,
    151, 151, 152, 152, 153, 154, 154, 155, 156, 156, 157, 157, 158, 159, 159, 160
  );
  LighterColors: array [0..255] of Byte = (
    128, 128, 129, 129, 130, 130, 131, 131, 132, 132, 133, 133, 134, 134, 135, 135,
    136, 136, 137, 137, 138, 138, 139, 139, 140, 140, 141, 141, 142, 142, 143, 143,
    144, 144, 145, 145, 146, 146, 147, 147, 148, 148, 149, 149, 150, 150, 151, 151,
    152, 152, 153, 153, 154, 154, 155, 155, 156, 156, 157, 157, 158, 158, 159, 159,
    160, 160, 161, 161, 162, 162, 163, 163, 164, 164, 165, 165, 166, 166, 167, 167,
    168, 168, 169, 169, 170, 170, 171, 171, 172, 172, 173, 173, 174, 174, 175, 175,
    176, 176, 177, 177, 178, 178, 179, 179, 180, 180, 181, 181, 182, 182, 183, 183,
    184, 184, 185, 185, 186, 186, 187, 187, 188, 188, 189, 189, 190, 190, 191, 191,
    192, 192, 193, 193, 194, 194, 195, 195, 196, 196, 197, 197, 198, 198, 199, 199,
    200, 200, 201, 201, 202, 202, 203, 203, 204, 204, 205, 205, 206, 206, 207, 207,
    208, 208, 209, 209, 210, 210, 211, 211, 212, 212, 213, 213, 214, 214, 215, 215,
    216, 216, 217, 217, 218, 218, 219, 219, 220, 220, 221, 221, 222, 222, 223, 223,
    224, 224, 225, 225, 226, 226, 227, 227, 228, 228, 229, 229, 230, 230, 231, 231,
    232, 232, 233, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 238, 239, 239,
    240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246, 246, 247, 247,
    248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 253, 253, 254, 254, 255, 255
  );

{$if not declared(CopyPalette)}
{$define CopyPaletteMissing}
function CopyPalette(Palette: HPALETTE): HPALETTE;
{$ifend}

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;

function htString(const Str: String): ThtString;
function htStringToString(const Str: ThtString): String;

procedure htAppendChr(var Dest: ThtString; C: ThtChar); {$ifdef UseInline} inline; {$endif}
procedure htAppendStr(var Dest: ThtString; const S: ThtString); {$ifdef UseInline} inline; {$endif}
procedure htSetString(var Dest: ThtString; Chr: PhtChar; Len: Integer); {$ifdef UseInline} inline; {$endif}
function htCompareStr(const S1, S2: ThtString): Integer; {$ifdef UseInline} inline; {$endif}
function htCompareText(const S1, S2: ThtString): Integer; {$ifdef UseInline} inline; {$endif}
function htSameText(const S1, S2: ThtString): Boolean; {$ifdef UseInline} inline; {$endif}
function htLowerCase(const Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
function htTrim(const Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
function htUpperCase(const Str: ThtString): ThtString; {$ifdef UseInline} inline; {$endif}
// htPos(SubStr, S, Offst): find substring in S starting at Offset: (formerly known as PosX)
function htPos(const SubStr, S: ThtString; Offset: Integer = 1): Integer;

function IsAlpha(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
function IsDigit(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
function RemoveQuotes(const S: ThtString): ThtString;

function htStringArrayToStr(const StrArray: ThtStringArray; Separator: ThtChar): ThtString;
function htSameStringArray(const A1, A2: ThtStringArray): Boolean;
procedure htSortStringArray(A: ThtStringArray);

function htFileExists(const FName: ThtString): Boolean;

//{$ifdef UnitConstsMissing}
//const
//  SOutOfResources	= 'Out of system resources';
//  SInvalidBitmap	= 'Bitmap image is not valid';
//  SScanLine		= 'Scan line index out of range';
//{$endif}

function PtrSub(P1, P2: Pointer): Integer; {$ifdef UseInline} inline; {$endif}
function PtrAdd(P1: Pointer; Offset: Integer): Pointer; {$ifdef UseInline} inline; {$endif}
procedure PtrInc(var P1; Offset: Integer); {$ifdef UseInline} inline; {$endif}

// BG, 17.04.2013: Color of Darker() and Lighter() must be RGB or palette values. Themed or system colors are not supported!
function Darker(Color : TColor): TColor; {$ifdef UseInline} inline; {$endif}
function Lighter(Color : TColor): TColor; {$ifdef UseInline} inline; {$endif}

//code movements from HTMLSubs
function FindSpaces(PStart : PWideChar; const ACount : Integer) : Integer; {$ifdef UseInline} inline; {$endif}
procedure InitFullBg(var FullBG : Graphics.TBitmap; const W, H: Integer; const AIsCopy : Boolean); {$ifdef UseInline} inline; {$endif}
procedure Circle(ACanvas : TCanvas; const X, Y, Rad: Integer); {$ifdef UseInline} inline; {$endif}

//alpha blend determination for Printers only
function CanPrintAlpha(ADC : HDC) : Boolean; {$ifdef UseInline} inline; {$endif}

procedure GetTSize(DC: HDC; P : PWideChar; N : Integer; out VSize : TSize);  {$ifdef UseInline} inline; {$endif}

//function ThemedColor_(const AColor : TColor
//  {$ifdef has_StyleElements};const AUseThemes : Boolean{$endif}
//  ): TColor; {$ifdef UseInline} inline; {$endif}

function TextEndsWith(const SubStr, S : ThtString) : Boolean; {$ifdef UseInline} inline; {$endif}
function TextStartsWith(const SubStr, S : ThtString) : Boolean; {$ifdef UseInline} inline; {$endif}

procedure CopyToClipBoardAsHtml(const Str: UTF8String);
procedure CopyToClipBoardAsText(const Str: ThtString);

implementation

{$ifndef TEditHasTextHint}
constructor ThtEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
//  FTextHintFont         := TFont.Create;
//  FTextHintFont.Color   := clGrayText;
  FCanvas.Control := Self;
end;

destructor ThtEdit.Destroy;
begin
//  FreeAndNil(FTextHintFont);
  FreeAndNil(FCanvas);
 inherited;
end;

procedure ThtEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if  (Text = '') and (not Focused) then
  begin
    FCanvas.Handle := Message.DC;
    FCanvas.Font := Font;//FTextHintFont;
    FCanvas.Font.Color := clGrayText;
    FCanvas.TextOut(1, 1, {$ifdef LCL} UTF8Encode(FTextHint) {$else} FTextHint {$endif});
  end;
end;
{$endif}

{$ifndef TComboBoxHasTextHint}
constructor ThtComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
//  FTextHintFont         := TFont.Create;
//  FTextHintFont.Color   := clGrayText;
  FCanvas.Control := Self;
end;

destructor ThtComboBox.Destroy;
begin
//  FreeAndNil(FTextHintFont);
  FreeAndNil(FCanvas);
 inherited;
end;

procedure ThtComboBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if  (Text = '') and (not Focused) then
  begin
    FCanvas.Handle := Message.DC;
    FCanvas.Font := Font;//FTextHintFont;
    FCanvas.Font.Color := clGrayText;
    FCanvas.TextOut(1, 1, {$ifdef LCL} UTF8Encode(FTextHint) {$else} FTextHint {$endif});
  end;
end;
{$endif}

constructor THtMemo.Create(AOwner: TComponent);
begin
  inherited;
  FTextHint             := TStringList.Create;
  FCanvas               := TControlCanvas.Create;
//  FTextHintFont         := TFont.Create;
//  FTextHintFont.Color   := clGrayText;
  TControlCanvas(FCanvas).Control := Self;
  FMaxLength := 0;
end;

destructor THtMemo.Destroy;
begin
//  FreeAndNil(FTextHintFont);
  FreeAndNil(FCanvas);
  FTextHint.Clear;
  FreeAndNil(FTextHint);
 inherited;
end;

procedure THtMemo.WMPaint(var Message: TWMPaint);
Var
  i            : integer;
  TextHeight   : Integer;
begin
  inherited;
  if  (Text = '') and (not Focused) then
  begin
    FCanvas.Handle := Message.DC;
    FCanvas.Font := Font;//FTextHintFont;
    FCanvas.Font.Color := clGrayText;
    TextHeight := FCanvas.TextHeight('MLZ'); //Dummy Text to determine Height
    for i := 0 to FTextHint.Count - 1 do
      FCanvas.TextOut(1, 1+(i*TextHeight), FTextHint[i]);
  end;
end;

procedure THtMemo.SetMaxLength(const AValue : Integer);
begin
  if AValue <> FMaxLength then begin
    FMaxLength := AValue;
    SendMessage(Handle, EM_LIMITTEXT, AValue, 0);
  end;
end;

//{$ifdef has_StyleElements}
//function ThemedColor(const AColor : TColor; const AUseThemes : Boolean): TColor; {$ifdef UseInline} inline; {$endif} overload;
//begin
//  if AUseThemes and TStyleManager.IsCustomStyleActive then begin
//    Result := StyleServices.GetSystemColor(AColor);
//  end else begin
//    Result := AColor;
//  end;
//  Result := ColorToRGB(Result);
//end;
//{$else}
//function ThemedColor(const AColor : TColor): TColor;
// {$ifdef UseInline} inline; {$endif}
//begin
//{$ifdef UseVCLStyles}
//  if TStyleManager.IsCustomStyleActive then begin
//    Result := StyleServices.GetSystemColor(AColor);
//  end else begin
//    Result := AColor;
//  end;
//  Result := ColorToRGB(Result);
//{$else}
//  Result := ColorToRGB(AColor);
//{$endif}
//end;
//{$endif}

procedure GetTSize(DC: HDC; P : PWideChar; N : Integer; out VSize : TSize);
 {$ifdef UseInline} inline; {$endif}
var
    Dummy: Integer;
begin
  VSize.cx := 0;
  VSize.cy := 0;
  if not IsWin32Platform then
    GetTextExtentExPointW(DC, P, N, 0, @Dummy, nil, VSize)
  else
    GetTextExtentPoint32W(DC, P, N, VSize); {win95, 98 ME}
end;

function CanPrintAlpha(ADC : HDC) : Boolean; {$ifdef UseInline} inline; {$endif}
begin
  Result := GetDeviceCaps(ADC,SHADEBLENDCAPS) and SB_CONST_ALPHA > 0;
end;

function Darker(Color : TColor): TColor; {$ifdef UseInline} inline; {$endif}
  {find a somewhat darker color for shading purposes}
var
  Red, Green, Blue: Byte;
begin
  Result := ColorToRGB(Color);
  Red   := DarkerColors[Byte(Result       )];
  Green := DarkerColors[Byte(Result shr  8)];
  Blue  := DarkerColors[Byte(Result shr 16)];
  Result := RGB(Red, Green, Blue);
end;

function Lighter(Color : TColor) : TColor; {$ifdef UseInline} inline; {$endif}
{find a somewhat lighter color for shading purposes}
var
  Red, Green, Blue: Byte;
begin
  Result := ColorToRGB(Color);
  Red   := LighterColors[Byte(Result       )];
  Green := LighterColors[Byte(Result shr  8)];
  Blue  := LighterColors[Byte(Result shr 16)];
  Result := RGB(Red, Green, Blue);
end;

function FindSpaces(PStart : PWideChar; const ACount : Integer) : Integer;
 {$ifdef UseInline} inline; {$endif}
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ACount - 2 do {-2 so as not to count end spaces}
    if ((PStart + I)^ = SpcChar) or ((PStart + I)^ = NbSpcChar) then
      Inc(Result);
end;

procedure InitFullBg(var FullBG : Graphics.TBitmap; const W, H: Integer; const AIsCopy : Boolean);
 {$ifdef UseInline} inline; {$endif}
begin
  if not Assigned(FullBG) then
  begin
    FullBG := Graphics.TBitmap.Create;
    if AIsCopy then
    begin
      FullBG.HandleType := bmDIB;
      if ColorBits <= 8 then
        FullBG.Palette := CopyPalette(ThePalette);
    end;
  end;
  FullBG.Width := Max(W,2);
  FullBG.Height := Max(H,2);
end;

procedure Circle(ACanvas : TCanvas; const X, Y, Rad: Integer);
 {$ifdef UseInline} inline; {$endif}
begin
    ACanvas.Ellipse(X, Y - Rad, X + Rad, Y);
end;

//-- BG ------------------------------------------------------------------------
function PtrSub(P1, P2: Pointer): Integer;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef FPC}
  Result := P1 - P2;
{$else}
  Result := PAnsiChar(P1) - PAnsiChar(P2);
{$endif}
end;

//-- BG ------------------------------------------------------------------------
function PtrAdd(P1: Pointer; Offset: Integer): Pointer;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef FPC}
  Result := P1 + Offset;
{$else}
  Result := PAnsiChar(P1) + Offset;
{$endif}
end;

//-- BG ------------------------------------------------------------------------
procedure PtrInc(var P1; Offset: Integer);
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef FPC}
  Inc(PAnsiChar(P1), Offset);
{$else}
  Inc(PAnsiChar(P1), Offset);
{$endif}
end;

procedure CalcPalette(DC: HDC);
 {$ifdef UseInline} inline; {$endif}
{calculate a rainbow palette, one with equally spaced colors}
const
  Values: array[0..5] of integer = (55, 115, 165, 205, 235, 255);
var
  LP: ^TLogPalette;
  I, J, K, Sub: integer;
begin
  GetMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  try
    with LP^ do
    begin
      palVersion := $300;
      palNumEntries := 256;
      GetSystemPaletteEntries(DC, 0, 256, palPalEntry);
      Sub := 10; {start at entry 10}
      for I := 0 to 5 do
        for J := 0 to 5 do
          for K := 0 to 5 do
            if not ((I = 5) and (J = 5) and (K = 5)) then {skip the white}
              with palPalEntry[Sub] do
              begin
                peBlue := Values[I];
                peGreen := Values[J];
                peRed := Values[K];
                peFlags := 0;
                Inc(Sub);
              end;
      for I := 1 to 24 do
        if not (I in [7, 15, 21]) then {these would be duplicates}
          with palPalEntry[Sub] do
          begin
            peBlue := 130 + 5 * I;
            peGreen := 130 + 5 * I;
            peRed := 130 + 5 * I;
            peFlags := 0;
            Inc(Sub);
          end;
      Sub := 245;
      with palPalEntry[Sub] do
      begin
        peBlue := 254;
        peGreen := 255;
        peRed := 255;
        peFlags := 0;
      end;
      ThePalette := CreatePalette(LP^);
    end;
  finally
    FreeMem(LP, Sizeof(TLogPalette) + 256 * Sizeof(TPaletteEntry));
  end;
end;

{$ifdef CopyPaletteMissing}
// -----------
// CopyPalette
// -----------
// Copies a HPALETTE.
//
// Copied from D3 graphics.pas. This is declared private in some old versions
// of Delphi 2 and is missing in Lazarus Component Library (LCL), so we have
// to implement it here to support those versions.
//
// Parameters:
// Palette	The palette to copy.
//
// Returns:
// The handle to a new palette.
//
function CopyPalette(Palette: HPALETTE): HPALETTE;
 {$ifdef UseInline} inline; {$endif}
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;
{$endif CopyPaletteMissing}

const
  SOutOfResources = 'Out of system resources';
var
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
{$ifndef LCL}
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
{$else}
begin
{$endif}
    OutOfResources;
end;

function GDICheck(Value: PtrUInt): PtrUInt;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  SaveObj: HGDIOBJ;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
{$ifdef MSWindows}
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(
        DstDC, DstX, DstY, DstW, DstH,
        SrcDC, SrcX, SrcY,
        MemBmp, MaskX, MaskY,
        MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;
{$endif}
  MemDC := GDICheck(CreateCompatibleDC(DstDC));
  try
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, SrcW, SrcH));
    SaveObj := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    try
      SelectPalette(SrcDC, SavePal, False);
      if SavePal <> 0 then
        SavePal := SelectPalette(MemDC, SavePal, True)
      else
        SavePal := SelectPalette(MemDC, SystemPalette16, True);
      RealizePalette(MemDC);

      { Mask out the transparent colored pixels of the source }
      BitBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcCopy);
      BitBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcErase);

      { Punch out a black hole in the background where the source image will go}
      crText := SetTextColor(DstDC, $0);
      crBack := SetBkColor(DstDC, $FFFFFF);
      StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
      SetTextColor(DstDC, crText);
      SetBkColor(DstDC, crBack);

      StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    finally
      SelectPalette(MemDC, SavePal, False);
      SelectObject(MemDC, SaveObj);
      DeleteObject(MemBmp);
    end;
  finally
    DeleteDC(MemDC);
  end;
end;

//-- BG ---------------------------------------------------------- 19.09.2018 --
function htString(const Str: String): ThtString;
begin
{$ifdef LCL}
  Result := UTF8Decode(Str);
{$else}
  Result := Str;
{$endif}
end;

function htStringToString(const Str: ThtString): String;
begin
{$ifdef LCL}
  Result := UTF8Encode(Str);
{$else}
  Result := Str;
{$endif}
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htAppendChr(var Dest: ThtString; C: ThtChar);
 {$ifdef UseInline} inline; {$endif}
begin
  SetLength(Dest, Length(Dest) + 1);
  Dest[Length(Dest)] := C;
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htAppendStr(var Dest: ThtString; const S: ThtString);
 {$ifdef UseInline} inline; {$endif}
var
  L, N: Integer;
begin
  L := Length(S);
  if L > 0 then
  begin
    N := Length(Dest);
    SetLength(Dest, N + L);
    Move(S[1], Dest[N + 1], L * sizeof(ThtChar));
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function htCompareStr(const S1, S2: ThtString): Integer;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef UNICODE}
  Result := CompareStr(S1, S2);
{$else}
  Result := WideCompareStr(S1, S2);
{$endif}
end;

//-- BG ---------------------------------------------------------- 10.12.2010 --
function htCompareText(const S1, S2: ThtString): Integer;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef UNICODE}
  Result := CompareText(S1, S2);
{$else}
  Result := WideCompareText(S1, S2);
{$endif}
end;

//-- BG ---------------------------------------------------------- 10.10.2016 --
function htSameText(const S1, S2: ThtString): Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := htCompareText(S1, S2) = 0;
end;

//-- BG ---------------------------------------------------------- 22.10.2016 --
function htStringArrayToStr(const StrArray: ThtStringArray; Separator: ThtChar): ThtString;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := Low(StrArray) to high(StrArray) do
  begin
    if Length(Result) > 0 then
      htAppendChr(Result, Separator);
    htAppendStr(Result, StrArray[I]);
  end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
function htSameStringArray(const A1, A2: ThtStringArray): Boolean;
var
  I, N: Integer;
begin
  N := Length(A1);
  Result := N = Length(A2);
  if Result then
    for I := 0 To N - 1 do
      if htCompareStr(A1[I], A2[I]) <> 0 then
      begin
        Result := False;
        break;
      end;
end;

//-- BG ---------------------------------------------------------- 20.03.2011 --
procedure htSortStringArray(A: ThtStringArray);

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: ThtString;
  begin
    repeat
      I := L;
      J := R;
      P := A[(L + R) shr 1];
      repeat
        while htCompareStr(A[I], P) < 0 do
          Inc(I);
        while htCompareStr(A[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := A[I];
          A[I] := A[J];
          A[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if length(A) > 1 then
    QuickSort(Low(A), High(A));
end;

//-- BG ---------------------------------------------------------- 28.01.2011 --
function htLowerCase(const Str: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef UNICODE}
  // LowerCase() converts 7bit chars only while AnsiLowerCase() converts UnicodeStrings correctly.
  // Actually it does the same as we do in the $else part except for Linux.
  Result := AnsiLowerCase(Str);
{$else}
  Result := WideLowerCase(Str);
{$endif}
end;

//-- BG ---------------------------------------------------------- 27.03.2011 --
procedure htSetString(var Dest: ThtString; Chr: PhtChar; Len: Integer);
 {$ifdef UseInline} inline; {$endif}
begin
{$ifdef UNICODE}
  SetString(Dest, Chr, Len);
{$else}
  SetLength(Dest, Len);
  Move(Chr^, Dest[1], Len * sizeof(ThtChar));
{$endif}
end;

//-- BG ---------------------------------------------------------- 09.08.2011 --
function htTrim(const Str: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := Trim(Str);
end;

//-- BG ---------------------------------------------------------- 28.01.2011 --
function htUpperCase(const Str: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
begin
  {$ifdef UNICODE}
    // UpperCase() converts 7bit chars only while AnsiUpperCase() converts UnicodeStrings correctly.
    // Actually it does the same as we do in the $else part except for Linux.
    Result := AnsiUpperCase(Str);
  {$else}
    Result := WideUpperCase(Str);
  {$endif}
end;

//-- BG ---------------------------------------------------------- 26.02.2023 --
function htFileExists(const FName: ThtString): Boolean;
var
  Name: string;
begin
  Name := htStringToString(FName);
  Result := FileExists(Name);
  {$if defined(FPC) and (fpc_fullversion < 3020000)}
    // Fix to disable LoadFromFile with folder path #290 (Alexey-T)
    // DirectoryExists needed in Laz, because FPC 3.0.4 gets FileExists(s)=True
    // for folder. It's changed in FPC 3.2.
    if Result then
      Result := not DirectoryExists(Name);
  {$ifend}
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function IsAlpha(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
begin
  case Ch of
    'a'..'z', 'A'..'Z':
      Result := True;
  else
    Result := False;
  end;
end;

//-- BG ---------------------------------------------------------- 21.08.2011 --
function IsDigit(Ch: ThtChar): Boolean; {$ifdef UseInline} inline; {$endif}
begin
  case Ch of
    '0'..'9':
      Result := True;
  else
    Result := False;
  end;
end;

{----------------RemoveQuotes}

function RemoveQuotes(const S: ThtString): ThtString;
 {$ifdef UseInline} inline; {$endif}
{if ThtString is a quoted ThtString, remove the quotes (either ' or ")}
begin
  Result := S;
  if Length(Result) >= 2 then
  begin
    case Result[1] of
      '"', '''':
        if Result[Length(Result)] = Result[1] then
          Result := Copy(Result, 2, Length(Result) - 2);
    end;
  end;
end;

function htPos(const SubStr, S: ThtString; Offset: Integer = 1): Integer;
 {$ifdef UseInline} inline; {$endif}
{find substring in S starting at Offset}
var
  S1: ThtString;
  I: Integer;
begin
  if Offset <= 1 then
    Result := Pos(SubStr, S)
  else
  begin
    S1 := Copy(S, Offset, Length(S) - Offset + 1);
    I := Pos(SubStr, S1);
    if I > 0 then
      Result := I + Offset - 1
    else
      Result := 0;
  end;
end;

function TextStartsWith(const SubStr, S : ThtString) : Boolean;
 {$ifdef UseInline} inline; {$endif}
begin
  Result := Copy(S,1,Length(SubStr)) = SubStr;
end;

function TextEndsWith(const SubStr, S : ThtString) : Boolean;
 {$ifdef UseInline} inline; {$endif}
var l : Integer;
begin
  l := Length(SubStr);
  Result := Copy(S,Length(S)-l+1,l) = SubStr;
end;

{ ThtEdit }

procedure ThtEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and ([ssCtrl] = Shift) then
    SelectAll;
end;

{ ThtCanvas }

procedure ThtCanvas.htTextRect(const Rect: TRect; X, Y: Integer; const Text: ThtString);
{$ifdef LCL}
{$else}
var
  Options: Longint;
{$endif LCL}
begin
{$ifdef LCL}
  inherited TextRect(Rect, X, Y, Utf8Encode(Text));
{$else}
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
  Changed;
{$endif LCL}
end;

{$ifdef LCL}

procedure DecodeStream(Input, Output: TStream);
const
  BufferSize = 999;
var
  Decoder: TBase64DecodingStream;
  Buffer: array[1..BufferSize] of Byte;
  Count: LongInt;
  I, J: Integer;
begin
  I := 0;
  J := 0;
  Decoder := TBase64DecodingStream.Create(Input, bdmMIME);
  try
    Decoder.Reset;
    repeat
      Count := Decoder.Read(Buffer[1], BufferSize);
      Output.Write(Buffer[1], Count);
      Inc(I);
      Inc(J, Count);
    until Count < BufferSize;
  finally
    Decoder.Free;
  end;
end;

function PromptForFileName(var AFileName: string; const AFilter: string = '';
  const ADefaultExt: string = ''; const ATitle: string = '';
  const AInitialDir: string = ''; SaveDialog: Boolean = False): Boolean;
var
  Dialog: TOpenDialog;
begin
  if SaveDialog then
  begin
    Dialog := TSaveDialog.Create(nil);
    Dialog.Options := Dialog.Options + [ofOverwritePrompt];
  end
  else
    Dialog := TOpenDialog.Create(nil);
  with Dialog do
  try
    Title := ATitle;
    if Length(Title) = 0 then
      if SaveDialog then
        Title := 'Select Filename'
      else
        Title := 'Select File';
    Filter := AFilter;
    if Length(Filter) = 0 then
      Filter := 'Any File (*.*)|*.*';
    DefaultExt := ADefaultExt;
    InitialDir := AInitialDir;
    FileName := AFileName;
    Result := Execute;
    if Result then
      AFileName := FileName;
  finally
    Free;
  end;
end;

{$else}

// Open a document with the default application associated with it in the system
function OpenDocument(const APath: ThtString): Boolean;
begin
  Result := ShellExecuteW(0, nil, PWideChar(APath), nil, nil, SW_SHOWNORMAL) > 32;
end;

{$endif LCL}

function StartProcess(const ApplicationName, Params: string; ShowWindow: Word): Boolean;
{$ifdef LCL}
var
  Process: TProcess;
  I: Integer;
begin
  // uses TProcess although Lazarus documentation prefers RunCommand
  // as RunCommand does not start an asynchronous process.
  Process := TProcess.Create(nil);
  try
    Process.Executable := ApplicationName;
    Process.Parameters.Add(Params);
    Process.InheritHandles := False;
    Process.ShowWindow := swoShow;

    // Copy default environment variables
    // including DISPLAY variable for GUI application to work
    for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));

    Process.Execute;
    Result := True;
  finally
    Process.Free;
  end;
end;
{$else}
var
  si: TStartupInfo;
  pi: TProcessInformation;
  CommandLine: string;
begin
  FillMemory(@si, SizeOf(si), 0);
  FillMemory(@pi, SizeOf(pi), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := ShowWindow;
  CommandLine := ApplicationName;
  if Pos(' ', CommandLine) > 0 then
    CommandLine := '"' + CommandLine + '"';
  if Length(Params) > 0 then
    CommandLine := CommandLine + ' ' + Params;
  UniqueString(CommandLine);
  Result := CreateProcess(PChar(ApplicationName), PChar(CommandLine), nil, nil, False, 0, nil, nil, si, pi);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
end;
{$endif}

//-- BG ---------------------------------------------------------- 26.09.2016 --
procedure CopyToClipBoardAsHtml(const Str: UTF8String);
// Put SOURCE on the clipboard, using FORMAT as the clipboard format
const
  HtmlClipboardFormat = 'HTML Format';
var
  CF_HTML: UINT;
  Len: Integer;
{$ifdef LCL}
  Str1: UTF8String;
{$else}
  Mem: HGLOBAL;
  Buf: PAnsiChar;
{$endif}
begin
  CF_HTML := RegisterClipboardFormat(HtmlClipboardFormat); {not sure this is necessary}
  Len := Length(Str);
{$ifdef LCL}
  Str1 := Str;
  Clipboard.AddFormat(CF_HTML, Str1[1], Len);
{$else}
  Mem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, Len + 1);
  try
    Buf := GlobalLock(Mem);
    try
      Move(Str[1], Buf[0], Len);
      Buf[Len] := #0;
      SetClipboardData(CF_HTML, Mem);
    finally
      GlobalUnlock(Mem);
    end;
  except
    GlobalFree(Mem);
  end;
{$endif}
end;

//-- BG ---------------------------------------------------------- 26.09.2016 --
procedure CopyToClipboardAsText(const Str: ThtString);
{$ifdef LCL}
var
  Utf8: Utf8String;
begin
  Utf8 := Utf8Encode(Str);
  Clipboard.AsText := Utf8;
end;
{$else}
var
  Len: Integer;
  Mem: HGLOBAL;
  Wuf: PWideChar;
begin
  Len := Length(Str);
  Mem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, (Len + 1) * SizeOf(WideChar));
  try
    Wuf := GlobalLock(Mem);
    try
      Move(Str[1], Wuf[0], Len * SizeOf(WideChar));
      Wuf[Len] := #0;
      // BG, 28.06.2012: use API method. The vcl clipboard does not support multiple formats.
      SetClipboardData(CF_UNICODETEXT, Mem);
    finally
      GlobalUnlock(Mem);
    end;
  except
    GlobalFree(Mem);
  end;
end;
{$endif}

{ ThtGraphic }

{$ifdef LCL}
//-- BG ---------------------------------------------------------- 10.10.2016 --
function ThtGraphic.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

//-- BG ---------------------------------------------------------- 10.10.2016 --
procedure ThtGraphic.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed(Self);
  end;
end;
{$endif}

{ ThtBitmap }

function ThtBitmap.GetMask: TBitmap;
{This returns mask for frame 1.  Content is black, background is white}
begin
  if not FTransparent then
    Result := nil
  else
    Result := FMask;
end;

procedure ThtBitmap.SetTransparentMask(AValue: Boolean);
begin
  if FTransparent <> AValue then
  begin
    FTransparent := AValue;
    if not FTransparent then
      FreeAndNil(FMask)
    else if FMask = nil then
    begin
      FMask := ThtBitmap.Create;
      FMask.TransparentMode := tmFixed;
      FMask.TransparentColor := clNone;
    end;
  end;
end;

constructor ThtBitmap.Create(WithTransparentMask: Boolean);
begin
  inherited Create;
  SetTransparentMask( WithTransparentMask );
end;

procedure ThtBitmap.Assign(Source: TPersistent);
var
  htSource: ThtBitmap absolute Source;
begin
  {$ifdef LCL}
    // LCL didn't copy PixelFormat before SVN-33344 (2011-11-05)
  if Source is TCustomBitmap then
    PixelFormat := TCustomBitmap(Source).PixelFormat;
  {$endif}
  inherited;
  if Source is ThtBitmap then
  begin
    FTransparent := htSource.FTransparent;
    SetMask(htSource.FMask);
  end;
end;

destructor ThtBitmap.Destroy;
begin
  FMask.Free;
  inherited;
end;

procedure ThtBitmap.SetMask(AValue: TBitmap);
begin
  if AValue = nil then
     FreeAndNil(FMask)
  else
  begin
    if FMask = nil then
    begin
      FMask := ThtBitmap.Create;
      FMask.TransparentMode := tmFixed;
      FMask.TransparentColor := clNone;
    end;
    FMask.Assign(AValue);
  end;
end;

procedure ThtBitmap.Draw(ACanvas: TCanvas; const DestRect: TRect);
{$ifdef LCL}
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (Width=0) or (Height=0)
  then Exit;

  BitmapHandleNeeded;
  if not BitmapHandleAllocated then Exit;

  if WithTransparentMask then
    UseMaskHandle := FMask.Handle
  else
    UseMaskHandle := 0;

  SrcDC := Canvas.GetUpdatedHandle([csHandleValid]);
  ACanvas.Changing;
  DestDC := ACanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(
    DestDC, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
     SrcDC,         0,        0,                  Width,                 Height,
     UseMaskHandle, 0,        0, ACanvas.CopyMode);
  ACanvas.Changed;
end;
{$else LCL}
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
begin
  with DestRect do
  begin
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if Palette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.Handle, Palette, True);
      RealizePalette(ACanvas.Handle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.Handle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.Handle, PLANES);
    DoHalftone := (BPP <= 8) and (PixelFormat in [pf15bit, pf16bit, pf24bit]);
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.Handle, pt);
      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      SetBrushOrgEx(ACanvas.Handle, pt.x, pt.y, @pt);
    end
    else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      if FTransparent then
      begin
{$ifdef Compiler23_Plus}
        if TransparentMode = tmFixed then
          DrawTransparent( ACanvas, Rect(Left, Top, Right, Bottom), 255 )
        else
{$endif}
          TransparentStretchBlt(
            ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
            Canvas.Handle, 0, 0, Self.Width, Self.Height,
            FMask.Canvas.Handle, 0, 0) {LDB}
      end
      else if AlphaFormat = afDefined then
      begin
        inherited;
      end
      else
        StretchBlt(
          ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle, 0, 0, Self.Width, Self.Height,
          ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.Handle, OldPalette, True);
    end;
  end;
end;
{$endif LCL}

procedure ThtBitmap.StretchDraw(ACanvas: TCanvas; const DestRect, SrcRect: TRect);
{Draw parts of this bitmap on ACanvas}
{$ifdef LCL}
var
  UseMaskHandle: HBitmap;
  SrcDC: hDC;
  DestDC: hDC;
begin
  if (Width=0) or (Height=0)
  then Exit;

  BitmapHandleNeeded;
  if not BitmapHandleAllocated then Exit;

  if WithTransparentMask then
    UseMaskHandle := FMask.Handle
  else
    UseMaskHandle := 0;

  SrcDC := Canvas.GetUpdatedHandle([csHandleValid]);
  ACanvas.Changing;
  DestDC := ACanvas.GetUpdatedHandle([csHandleValid]);
  StretchMaskBlt(
    DestDC, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
     SrcDC,  SrcRect.Left,  SrcRect.Top,  SrcRect.Right -  SrcRect.Left,  SrcRect.Bottom -  SrcRect.Top,
     UseMaskHandle, SrcRect.Left, SrcRect.Top, ACanvas.CopyMode);
  ACanvas.Changed;
end;
{$else LCL}
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
begin
  with DestRect do
  begin
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if Palette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.Handle, Palette, True);
      RealizePalette(ACanvas.Handle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.Handle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.Handle, PLANES);
    DoHalftone := (BPP <= 8) and (PixelFormat in [pf15bit, pf16bit, pf24bit]);
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.Handle, pt);
      SetStretchBltMode(ACanvas.Handle, HALFTONE);
      SetBrushOrgEx(ACanvas.Handle, pt.x, pt.y, @pt);
    end
    else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      if FTransparent then
        TransparentStretchBlt(
          ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
          FMask.Canvas.Handle, SrcRect.Left, SrcRect.Top) {LDB}
      else if AlphaFormat = afDefined then
      begin
        inherited;
      end
      else
        StretchBlt(
          ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top,
          ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.Handle, OldPalette, True);
    end;
  end;
end;
{$endif LCL}

{ initialization }

var
  DC: HDC;

initialization
  DC := GetDC(0);
  try
    ColorBits := GetDeviceCaps(DC, BitsPixel) * GetDeviceCaps(DC, Planes);

    if ColorBits <= 4 then
      ColorBits := 4
    else if ColorBits <= 8 then
      ColorBits := 8
    else
      ColorBits := 24;

    ThePalette := 0;
    if ColorBits = 8 then
      CalcPalette(DC);
    if ColorBits <= 8 then {use Palette Relative bit only when Palettes used}
      PalRelative := $2000000
    else
      PalRelative := 0;
  finally
    ReleaseDC(0, DC);
  end;

{$ifdef TransparentStretchBltMissing}
  // Note: This doesn't return the same palette as the Delphi 3 system palette
  // since the true system palette contains 20 entries and the Delphi 3 system
  // palette only contains 16.
  // For our purpose this doesn't matter since we do not care about the actual
  // colors (or their number) in the palette.
  // Stock objects doesn't have to be deleted.
  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
{$endif}

{$ifdef FPC}
  IsWin95 := False;           // Use the same always with FPC
  IsWin32Platform := False;
{$else}
  IsWin95 := (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (Win32MinorVersion in [0..9]);
  IsWin32Platform := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
{$endif FPC}
finalization
  if ThePalette <> 0 then
    DeleteObject(ThePalette);
end.

