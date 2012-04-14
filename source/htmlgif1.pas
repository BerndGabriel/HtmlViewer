
{***************************************************************}
{*                     htmlgif1.pas                            *}
{*                                                             *}
{*   Thanks to Ron Collins for the Gif code in this module.    *}
{*   His copyright notice is below.                            *}
{*                                                             *}
{*   This is only a portion of his code modified slightly      *}
{*   in a few places to accomodate my own needs.  Ron's        *}
{*   full package may be found at www.Torry.net/gif.htm.       *}
{*   The zip file is rctgif.zip.                               *}
{*                                                             *}
{***************************************************************}


{ ============================================================================

TGif.pas    copyright (C) 2000   R. Collins
rlcollins@ksaits.com

LEGAL STUFF:

This software is provided "as-is".  This software comes without warranty
or garantee, explicit or implied.  Use this software at your own risk.
The author will not be liable for any damage to equipment, data, or information
that may result while using this software.

By using this software, you agree to the conditions stated above.

This software may be used freely, provided the author's name and copyright
statement remain a part of the source code.

NOTE:  CompuServe, Inc. holds the patent to the compression algorithym
used in creating a GIF file.  Before you save a GIF file (using LZW
compression) that may be distributed for profit, make sure you understand
the full implications and legal ramifications of using the LZW compression.

============================================================================ }

{$I HtmlCons.inc}

unit HtmlGif1;

interface

uses
{$IFDEF UseCLX}
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, Math;
{$ELSE}
{$ifdef LCL}
  LclIntf, LclType, IntfGraphics, //LMessages,
{$else}
  Windows,
{$endif}
  Messages, SysUtils, Classes, Graphics,
  Controls, StdCtrls, ExtCtrls, Forms, Math;
{$ENDIF}

type
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF not defined(DOTNET) and not defined(FPC)}
{$IF declared(INT_PTR)}
  PtrInt = INT_PTR;
{$ELSE}
{$IFDEF WIN32}
  PtrInt = LongInt;
{$ENDIF}
{$IFDEF WIN64}
  PtrInt = Int64;
{$ENDIF}
{$IFEND}
{$IF declared(UINT_PTR)}
  PtrUInt = UINT_PTR;
{$ELSE}
{$IFDEF WIN32}
  PtrUInt = LongWord;
{$ENDIF}
{$IFDEF WIN64}
  PtrUInt = Int64;
{$ENDIF}
{$IFEND}
{$IFEND}
{$ELSE}
  PrtInt = LongInt;
  PtrUInt = LongWord;
{$ENDIF}


// LZW encode table sizes

const
  kGifCodeTableSize = 4096;


// the parts of a GIF file
// yes, yes, I know ... I don't have to put in "type"
// before each record definition.  I just think it makes it
// easier to read, especially when the definitions may be broken
// across the printed page.  if you don't like it, take them out.

type {LDB}
  TRGBQUAD = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
type
  PGifDataBlock = ^TGifDataBlock;
  TGifDataBlock = record // generic data clump
    rSize: integer; // NOTE: array starts at "1"
    rData: packed array[1..255] of byte;
  end;

type
  PGifSignature = ^TgifSignature;
  TGifSignature = record // GIF87A or GIF89A
    rSignature: packed array[1..6] of Ansichar;
  end;

type
  PGifExtensionGraphic = ^TgifExtensionGraphic;
  TGifExtensionGraphic = record // graphic control extension
    rBlockSize: integer; // must always be 4
    rDisposal: integer; // disposal method when drawing
    rUserInputValid: boolean; // wait for user input?
    rTransparentValid: boolean; // transparent color given?
    rDelayTime: integer; // delay between display images
    rTransparentIndex: integer; // into color table
  end;

type
  PGifExtensionComment = ^TgifExtensionComment;
  TGifExtensionComment = record // comment extension
    rDataBlockList: TList; // data blocks
  end;

type
  PGifExtensionText = ^TGifExtensionText;
  TGifExtensionText = record // plain text extension
    rBlockSize: integer; // must always be 12
    rGridLeft: integer; // text grid position
    rGridTop: integer;
    rGridWidth: integer; // text grid size
    rGridHeight: integer;
    rCellWidth: integer; // size of a character cell
    rCellHeight: integer;
    rForegroundIndex: integer; // text foreground color
    rBackgroundIndex: integer; // text background color
    rDataBlockList: TList; // data blocks
  end;

type
  PGifExtensionApplication = ^TgifExtensionApplication;
  TGifExtensionApplication = record // application extension
    rBlockSize: integer; // must always be 11
    rIdentifier: packed array[1..8] of Ansichar;
    rAuthentication: packed array[1..3] of Ansichar;
    rDataBlockList: TList; // data blocks
  end;

type
  PGifExtension = ^TGifExtension;
  TGifExtension = record // for any extension type
    case rLabel: byte of // cannot use CONST names
      $F9: (rGraphic: TGifExtensionGraphic);
      $FE: (rComment: TGifExtensionComment);
      $01: (rText: TGifExtensionText);
      $FF: (rApp: TGifExtensionApplication);
      $00: (rDummy: longint);
  end;

type
  PGifScreenDescriptor = ^TGifScreenDescriptor;
  TGifScreenDescriptor = record
    rWidth: integer; // size of logical screen
    rHeight: integer; // size of logical screen
    rGlobalColorValid: boolean; // global color table found in file?
    rColorResolution: integer; // bits per color
    rSorted: boolean; // global colors are sorted?
    rGlobalColorSize: integer; // size of global color table
    rBackgroundIndex: integer; // background color index
    rAspectRatio: integer; // pixel aspect ratio
    rGlobalColorTable: integer; // default color table for all images
  end;

type
  PGifColorTable = ^TGifColorTable; // pointer to a color table
  TGifColorTable = record
    rSize: integer; // number of valid entries
    rColors: array[0..255] of TColor; // the colors
  end;

type
  PGifImageDescriptor = ^TGifImageDescriptor;
  TGifImageDescriptor = record
    rIndex: integer; // which image is this?
    rLeft: integer; // position of image
    rTop: integer; // position of image
    rWidth: integer; // size of image
    rHeight: integer; // size of image
    rLocalColorValid: boolean; // color table used?
    rInterlaced: boolean; // interlaced lines?
    rSorted: boolean; // color table sorted?
    rLocalColorSize: integer; // number entries in local color table
    rLocalColorTable: integer; // index into master list
    rLZWSize: integer; // LZW minimum code size
    rExtensionList: TList; // extensions read before this image
    rPixelList: PByte; // decoded pixel indices
    rPixelCount: longint; // number of pixels
    rBitmap: TBitmap; // the actual image
  end;


type
  PGifZip = ^TGifZip;
  TGifZip = record
    rID: PGifImageDescriptor; // image parameters to decode
    rCT: PGifColorTable; // color table for this image
    rPrefix: array[0..kGifCodeTableSize - 1] of integer; // string prefixes
    rSuffix: array[0..kGifCodeTableSize - 1] of integer; // string suffixes
    rCodeStack: array[0..kGifCodeTableSize - 1] of byte; // decode/encoded pixels
    rSP: integer; // pointer into CodeStack
    rClearCode: integer; // reset decode params
    rEndCode: integer; // last code in input stream
    rHighCode: integer; // highest LZW code possible
    rCurSize: integer; // current code size
    rBitString: integer; // steady stream of bits to be decoded
    rBits: integer; // number of valid bits in BitString
    rCurSlot: integer; // next stack index to store a code
    rTopSlot: integer; // highest slot used so far
    rMaxVal: boolean; // max code value found?
    rCurX: integer; // position of next pixel
    rCurY: integer; // position of next pixel
    rCurPass: integer; // pixel line pass 1..4
    rFirstSlot: integer; // for encoding an image
    rNextSlot: integer; // for encoding
    rCount: integer; // number of bytes read/written
    rLast: integer; // last byte read in
    rUnget: boolean; // read a new byte, or use zLast?
  end;

{ ---------------------------------------------------------------------------- }

// define a GIF

type
  TGif = class(TObject)
  private
    fIOStream: TMemoryStream; // read or write the image
    fDataStream: TMemoryStream; // temp storage for LZW
    fExtension: TList; // latest extensions read/written

    fSignature: PGifSignature; // version of GIF
    fScreenDescriptor: PGifScreenDescriptor; // logical screen descriptor
    fImageDescriptorList: TList; // list of all images
    fColorTableList: TList; // list of all color tables
    fPaletteList: TList; // list of palettes from color tables
    fZipData: PGifZip; // for encode/decode image
    FLoopCount: integer; // number of animated iterations


// functions that override TGraphic items

  protected
    function GetHeight: integer;
    function GetWidth: integer;
    function GetTransparent: boolean;

// procedures to read a bitmap

  private
    procedure ReadSignature;
    procedure ReadScreenDescriptor;
    procedure ReadColorTable(Size: integer; var Table: integer);
    procedure ReadImageDescriptor;
    procedure ReadDataBlockList(List: TList);
    procedure ReadExtension(var Done: boolean);
    procedure ReadSourceInteger(size: integer; out value: integer);

// LZW encode and decode

    procedure LZWDecode(pID: PGifImageDescriptor);
    procedure LZWInit(pID: PGifImageDescriptor);
    procedure LZWFinit;
    procedure LZWReset;
    function LZWGetCode: integer;
    procedure LZWSaveCode(Code: integer);
    procedure LZWDecodeCode(var Code: integer);
    procedure LZWSaveSlot(Prefix, Suffix: integer);
    procedure LZWIncrPosition;
    procedure LZWCheckSlot;
    procedure LZWWriteBitmap;
    function LZWReadBitmap: integer;

// procedures used to implement the PROPERTIES

    function GetSignature: AnsiString;
    function GetScreenDescriptor: PGifScreenDescriptor;
    function GetImageCount: integer;
    function GetImageDescriptor(image: integer): PGifImageDescriptor;
    function GetBitmap(image: integer): TBitmap;
    function GetColorTableCount: integer;
    function GetColorTable(table: integer): PGifColorTable;
    function GetImageDelay(Image: integer): integer; {LDB}
    function GetImageDisposal(Image: integer): integer; {LDB}
    function GetColorIndex(image, x, y: integer): integer;
    function GetTransparentIndex(image: integer): integer;
    function GetTransparentColor: TColor;
    function GetImageLeft(image: integer): integer;
    function GetImageTop(image: integer): integer;
    function GetImageWidth(image: integer): integer;
    function GetImageHeight(image: integer): integer;
    function GetImageDepth(image: integer): integer;

// generally usefull routines

    procedure FreeDataBlockList(var list: TList);
    procedure FreeExtensionList(var list: TList);
    procedure MakeBitmaps;
    function FindGraphicExtension(image: integer): PGifExtensionGraphic;
    function FindColorIndex(c: TColor; ct: PGifColorTable): integer;
    procedure ExtractLoopCount(List: TList);

  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeImage;

    procedure LoadFromStream(Source: TStream);
    function GetStripBitmap(out Mask: TBitmap): TBitmap; {LDB}

    property Signature: AnsiString read GetSignature;
    property ScreenDescriptor: PGifScreenDescriptor read GetScreenDescriptor;
    property ImageCount: integer read GetImageCount;
    property ImageDescriptor[Image: integer]: PGifImageDescriptor read GetImageDescriptor;
    property Bitmap[Image: integer]: TBitmap read GetBitmap;
    property ColorTableCount: integer read GetColorTableCount;
    property ColorTable[Table: integer]: PGifColorTable read GetColorTable;
    property Height: integer read GetHeight;
    property Width: integer read GetWidth;

    property ImageDelay[Image: integer]: integer read GetImageDelay;
    property ImageDisposal[Image: integer]: integer read GetImageDisposal;

    property Transparent: boolean read GetTransparent;
    property TransparentIndex[Image: integer]: integer read GetTransparentIndex;
    property TransparentColor: TColor read GetTransparentColor;
    property ImageLeft[Image: integer]: integer read GetImageLeft;
    property ImageTop[Image: integer]: integer read GetImageTop;
    property ImageWidth[Image: integer]: integer read GetImageWidth;
    property ImageHeight[Image: integer]: integer read GetImageHeight;
    property ImageDepth[Image: integer]: integer read GetImageDepth;
    property LoopCount: integer read FLoopCount;

  end;

implementation

{$IFDEF UNICODE}
uses
  AnsiStrings;
{$ENDIF}

const
  TransColor = $170725;

  // GIF record separators

const
  kGifImageSeparator: byte = $2C;
  kGifExtensionSeparator: byte = $21;
  kGifTerminator: byte = $3B;
  kGifLabelGraphic: byte = $F9;
  kGifLabelComment: byte = $FE;
  kGifLabelText: byte = $01;
  kGifLabelApplication: byte = $FF;

// define a set of error messages

const
  kGifErrorMessages: array[0..27] of string = (
    'no error', // 0
    'Invalid GIF Signature Code', // 1
    'No Local or Global Color Table for Image', // 2
    'Unknown Graphics Extension Type', // 3
    'Unknown Graphics Operation Code', // 4
    'Invalid Extension Block Size', // 5
    '[special message]', // 6
    'Invalid Extension Block Terminator', // 7
    'Invalid Integer Size', // 8
    'No GIF Terminator Found', // 9
    'Extension Block Out-Of-Order With Image Data', // 10
    'Invalid Image Descriptor Index', // 11
    'Invalid LZW Code Size', // 12
    'Invalid LZW Data Format', // 13
    'LZW Code Overflow', // 14
    'Value Out Of Range', // 15
    'NIL Pointer assigned', // 16
    'Invalid Color Table Size', // 17
    'No Image Description', // 18
    'Invalid Bitmap Image', // 19
    'Invalid Color Table Index', // 20
    'Invalid Interlace Pass', // 21
    'Invalid Bitmap', // 22
    'Too Many Colors In Bitmap', // 23
    'Unexpected end of file', // 24   {LDB}
    'Animated GIF too large', // 25   {LDB}
    'Zero width or height', // 26   {LDB}
    'next message' //
    );

var
  GIF_ErrorCode: integer; // last error
  GIF_ErrorString: string; // last error

procedure GIF_Error(n: integer); forward;
procedure GIF_ErrorMessage(m: string); forward;


constructor TGif.Create;
begin
  inherited Create;

// nothing defined yet

  fIOStream := nil;
  fDataStream := nil;
  fExtension := nil;
  fSignature := nil;
  fScreenDescriptor := nil;
  fImageDescriptorList := nil;
  fColorTableList := nil;
  fPaletteList := nil;
  fZipData := nil;
  FLoopCount := -1; // -1 is no loop count entered

// some things, though, will always be needed

  new(fSignature);
  if (fSignature = nil) then
    OutOfMemoryError;
  fSignature^.rSignature := AnsiString('------');

  new(fScreenDescriptor);
  if (fScreenDescriptor = nil) then
    OutOfMemoryError;
  fillchar(fScreenDescriptor^, sizeof(TGifScreenDescriptor), 0);

  fImageDescriptorList := TList.Create;
  fColorTableList := TList.Create;
  fPaletteList := TList.Create;

end;


destructor TGif.Destroy;
begin

// clean up most of the data

  FreeImage;

// and then the left-overs

  dispose(fSignature);
  dispose(fScreenDescriptor);

  fImageDescriptorList.Free;
  fColorTableList.Free;
  fPaletteList.Free;

// and the ancestor

  inherited;
end;

{ ---------------------------------------------------------------------------- }
{ release all memory used to store image data }

procedure TGif.FreeImage;
var
  i: integer;
  id: PGifImageDescriptor;
  ct: PGifColorTable;
begin

// temp input/output stream

  if (fIOStream <> nil) then
    fIOStream.Free;
  fIOStream := nil;

// temp encoded data

  if (fDataStream <> nil) then
    fDataStream.Free;
  fDataStream := nil;

// temp list of image extensions

  if (fExtension <> nil) then
    FreeExtensionList(fExtension);
  fExtension := nil;

// signature record stays, but is cleared

  if (fSignature = nil) then
    new(fSignature);
  fSignature^.rSignature := AnsiString('------');

// ditto the screen descriptor

  if (fScreenDescriptor = nil) then
    new(fScreenDescriptor);
  fillchar(fScreenDescriptor^, sizeof(TGifScreenDescriptor), 0);

// delete all items from image list, but leave the list

  if (fImageDescriptorList = nil) then
    fImageDescriptorList := TList.Create;
  for i := 0 to (fImageDescriptorList.Count - 1) do
  begin
    id := fImageDescriptorList.Items[i];
    if (id <> nil) then
    begin
      if (id^.rExtensionList <> nil) then
        FreeExtensionList(id^.rExtensionList);
      if (id^.rPixelList <> nil) then
        freemem(id^.rPixelList);
      if (id^.rBitmap <> nil) then
        id^.rBitmap.Free;

      dispose(id);
    end;
  end;
  fImageDescriptorList.Clear;

// release color tables, but keep the list

  if (fColorTableList = nil) then
    fColorTableList := TList.Create;
  for i := 0 to (fColorTableList.Count - 1) do
  begin
    ct := fColorTableList.Items[i];
    if (ct <> nil) then
      dispose(ct);
  end;
  fColorTableList.Clear;

// once again, keep the palette list object, but not the data

  if (fPaletteList = nil) then
    fPaletteList := TList.Create;
  fPaletteList.Clear;

// don't need the zip/unzip data block

  if (fZipData <> nil) then
    dispose(fZipData);
  fZipData := nil;
end;


{ ---------------------------------------------------------------------------- }
{ READ and WRITE A GIF ------------------------------------------------------- }

{ ---------------------------------------------------------------------------- }
{ read a GIF definition from a stream }

procedure TGif.LoadFromStream(Source: TStream);
var
  done: boolean;
  b: byte;
begin

// release old image that may be here ...

  FreeImage;

// no error yet

  GIF_ErrorCode := 0;
  GIF_ErrorString := '';

// make a local copy of the source data
// memory streams are faster and easier to manipulate than file streams

  fIOStream := TMemoryStream.Create;
  Source.Position := 0;
  fIOStream.LoadFromStream(Source);

// local temp vars

  fDataStream := TMemoryStream.Create; // data to be un-zipped
  fExtension := nil; // extensions to an image


// read the signature GIF87A or GIF89A

  ReadSignature;

// read the logical screen descriptor

  ReadScreenDescriptor;

// read extensions and image data until end of file

  done := false;
  while (not done) do
  try {LDB}
    if (fIOStream.Position >= fIOStream.Size) then
      //GIF_Error(9);    {LDB}
      b := 0 {LDB}

    else
      fIOStream.Read(b, 1); {LDB} // image separator

    if (b = 0) then // just skip this?
    begin
      b := 0;
      Done := True; {LDB}
    end
    else if (b = kGifTerminator) then // got it all
    begin
      done := true;
    end
    else if (b = kGifImageSeparator) then // next bitmap
    begin
      ReadImageDescriptor;
    end
    else if (b = kGifExtensionSeparator) then // special operations
    begin
      ReadExtension(Done);
    end
    else // unknown
    begin
      GIF_Error(4);
    end;
  except {LDB}
    if GetImageCount > 0 then
      Done := True {use what images we have}
    else
      raise;
  end;

// must have an image

  if (fImageDescriptorList.Count = 0) then
    GIF_Error(18);

// no longer need the source data in memory

  fIOStream.Free;
  fDataStream.Free;
  FreeExtensionList(fExtension);

  fIOStream := nil;
  fDataStream := nil;
  fExtension := nil;
end;


function TGif.GetHeight: integer;
begin
  GetHeight := fScreenDescriptor^.rHeight;
end;

function TGif.GetWidth: integer;
begin
  GetWidth := fScreenDescriptor^.rWidth;
end;

{ ---------------------------------------------------------------------------- }
{ TRANSPARENT is assument to be the same for all images; i.e., if the first }
{ image is transparent, they they are all transparent }
{ if SetTransparent(TRUE) then set default color index for transparent color }
{ this can be changed with TransparentColor after this call }

{LDB  changed so that if any images are transparent, Transparent returns True}

function TGif.GetTransparent: boolean;
var
  b: boolean;
  gx: PGifExtensionGraphic;
  i: integer;
begin
  b := false;
  for I := 0 to (fImageDescriptorList.Count - 1) do
  begin
    gx := FindGraphicExtension(I);
    if (gx <> nil) then
      b := gx^.rTransparentValid or b;
  end;

  GetTransparent := b;
end;

{ ---------------------------------------------------------------------------- }
{ PROCEDURES TO READ A GIF FILE ---------------------------------------------- }

{ ---------------------------------------------------------------------------- }
{ read the GIF signature from the source stream }
{ this assumes the memory stream position is correct }
{ the signature is always 6 bytes, and must be either GIF87A or GIF89A }

procedure TGif.ReadSignature;
var
  s: AnsiString;
begin
  with fSignature^ do
  begin
    fIOStream.Read(rSignature, 6);
    s := rSignature;
    s := AnsiUpperCase(s);
    if ((s <> 'GIF87A') and (s <> 'GIF89A')) then
      GIF_Error(1);
  end;
end;


{ ---------------------------------------------------------------------------- }
{ read the GIF logical screen descriptor from the source stream }
{ this assumes the memory stream position is correct }
{ this always follows the GIF signature }

procedure TGif.ReadScreenDescriptor;
var
  i, n: integer;
begin
  with fScreenDescriptor^ do
  begin
    ReadSourceInteger(2, rWidth); // logical screen width
    ReadSourceInteger(2, rHeight); // logical screen height

    ReadSourceInteger(1, n); // packed bit fields
    rGlobalColorValid := ((n and $80) <> 0);
    rColorResolution := ((n shr 4) and $07) + 1;
    rSorted := ((n and $08) <> 0);

    i := (n and $07);
    if (i = 0) then
      rGlobalColorSize := 2
    else if (i = 1) then
      rGlobalColorSize := 4
    else if (i = 2) then
      rGlobalColorSize := 8
    else if (i = 3) then
      rGlobalColorSize := 16
    else if (i = 4) then
      rGlobalColorSize := 32
    else if (i = 5) then
      rGlobalColorSize := 64
    else if (i = 6) then
      rGlobalColorSize := 128
    else if (i = 7) then
      rGlobalColorSize := 256
    else
      rGlobalColorSize := 256;


    ReadSourceInteger(1, rBackgroundIndex); // background color
    ReadSourceInteger(1, rAspectRatio); // pixel aspect ratio

// read the global color table from the source stream
// this assumes the memory stream position is correct
// the global color table is only valid if a flag is set in the logical
// screen descriptor.  if the flag is set, the global color table will
// immediately follow the logical screen descriptor

    rGlobalColorTable := -1;
    if (rGlobalColorValid) then // a global color table?
      ReadColorTable(rGlobalColorSize, rGlobalColorTable)
  end;
end;

{ ---------------------------------------------------------------------------- }
{ read in any type of color table }
{ number of RGB entries is given by SIZE, and save the index into the }
{ master color table list in TABLE }
{ if SIZE is <= 0, then there is no table, and the TABLE becomes -1 }

procedure TGif.ReadColorTable(Size: integer; var Table: integer);
var
  i, n: integer;
  r, g, b: byte;
  ct: PGifColorTable;
begin
  Table := -1; // assume no table
  if (Size > 0) then // OK, a table does exist
  begin
    new(ct); // make a anew color table
    if (ct = nil) then
      OutOfMemoryError;
    n := fColorTableList.Add(ct); // save it in master list
    Table := n; // save index for a valid table

    ct^.rSize := Size;

    for i := 0 to (ct^.rSize - 1) do // read a triplet for each TColor
    begin
      fIOStream.Read(r, 1); // red
      fIOStream.Read(g, 1); // green
      fIOStream.Read(b, 1); // blue

      ct^.rColors[i] := r or (g shl 8) or (b shl 16);
    end;

// make sure we store palette handle in same index slot as the color table

    while (fPaletteList.Count < fColorTableList.Count) do
      fPaletteList.Add(nil);
    fPaletteList.Items[Table] := nil;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ read the next image descriptor }
{ the source stream position should be immediately following the }
{ special code image separator }
{ note: this routine only reads in the raw data; the LZW de-compression }
{ occurs later, after all the data has been read }
{ this probably makes for a bigger data chunk, but it doesn't much effect }
{ the speed, and it is certainly a more modular approach and is much easier }
{ to understand the mechanics later }

procedure TGif.ReadImageDescriptor;
var
  i, n: integer;
  ix: integer;
  id: PGifImageDescriptor;
  db: TGifDataBlock;
begin

// make a new image desctiptor record and add this record to main list

  new(id);
  if (id = nil) then
    OutOfMemoryError;
  if (fImageDescriptorList = nil) then
    fImageDescriptorList := TList.Create;
  ix := fImageDescriptorList.Add(id);
  id^.rIndex := ix;

// initialize data

  fillchar(id^, sizeof(TGifImageDescriptor), 0);

// init the sotrage for compressed data

  fDataStream.Clear;

// if extensions were read in earlier, save that list
// for this image descriptor
// if no extensions were read in, then we don't need this list at all

  if (fExtension <> nil) then
  begin
    id^.rExtensionList := fExtension;
    fExtension := nil;
  end;

// shortcut to the record fields

  with id^ do
  begin

// read the basic descriptor record

    ReadSourceInteger(2, rLeft); // left position
    ReadSourceInteger(2, rTop); // top position
    ReadSourceInteger(2, rWidth); // size of image
    ReadSourceInteger(2, rHeight); // size of image

    if rHeight > Height then {LDB make sure bad values don't overflow elsewhere}
      rHeight := Height;

    ReadSourceInteger(1, n); // packed bit field
    rLocalColorValid := ((n and $80) <> 0);
    rInterlaced := ((n and $40) <> 0);
    rSorted := ((n and $20) <> 0);

    i := (n and $07);
    if (i = 0) then
      rLocalColorSize := 2
    else if (i = 1) then
      rLocalColorSize := 4
    else if (i = 2) then
      rLocalColorSize := 8
    else if (i = 3) then
      rLocalColorSize := 16
    else if (i = 4) then
      rLocalColorSize := 32
    else if (i = 5) then
      rLocalColorSize := 64
    else if (i = 6) then
      rLocalColorSize := 128
    else if (i = 7) then
      rLocalColorSize := 256
    else
      rLocalColorSize := 256;


// if a local color table is defined, read it
// otherwise, use the global color table

    if (rLocalColorValid) then
      ReadColorTable(rLocalColorSize, rLocalColorTable)
    else
      rLocalColorTable := fScreenDescriptor^.rGlobalColorTable;

// _something_ must have defined by now ...

    if (rLocalColorTable < 0) then
      GIF_Error(2);

// the LZW minimum code size

    ReadSourceInteger(1, rLZWSize);

// read data blocks until the end of the list

    ReadSourceInteger(1, db.rSize);
    while (db.rSize > 0) do
    begin
      if fIOStream.Read(db.rData, db.rSize) < db.rSize then
        Gif_Error(24); {LDB}
      fDataStream.Write(db.rData, db.rSize);
      ReadSourceInteger(1, db.rSize);
    end;

// save the pixel list

    rPixelCount := rWidth * rHeight;
    if rPixelCount = 0 then {LDB}
      Gif_Error(26);
    rPixelList := allocmem(rPixelCount);
    if (rPixelList = nil) then
      OutOfMemoryError;

// uncompress the data and write the bitmap

    LZWDecode(id);
  end; // with id^
end;


{ ---------------------------------------------------------------------------- }
{ read in a group of data blocks until a zero-length block is found }
{ store the data on the give TList }

procedure TGif.ReadDataBlockList(List: TList);
var
  b: byte;
  db: PGifDataBlock;
  BytesRead: integer;
begin

// read data blocks until the end of the list

  fIOStream.Read(b, 1); // size of next block
  while (b > 0) do // more blocks to get?
  begin
    new(db); // new data block record
    db^.rSize := b;
    BytesRead := fIOStream.Read(db^.rData, db^.rSize); // read the data
    List.Add(db); // save in given list

    if BytesRead < db^.rSize then
      Gif_Error(24); {LDB}
    fIOStream.Read(b, 1); // size of next block
  end;
end;



{ ---------------------------------------------------------------------------- }
{ read in any type of extension record }
{ assume that the source position is AFTER the extension separator, }
{ but BEFORE the specific extension label }
{ the extension record we read in is stored in the master extension }
{ list; however, the indexes for these exrtensions is stored in a }
{ temporary list which will be assigned to the next image descriptor }
{ record read in.  this is because all extension blocks preceed the }
{ image descriptor to which they belong }

procedure TGif.ReadExtension(var Done: boolean);
var
  n: integer;
  b: byte;
  eb: PGifExtension;
begin

// make a list exists

  if (fExtension = nil) then
    fExtension := TList.Create;

// make a new extension record and add it to temp holding list

  new(eb);
  if (eb = nil) then
    OutOfMemoryError;
  fillchar(eb^, sizeof(TGifExtension), 0);
  fExtension.Add(eb);

// get the type of extension

  fIOStream.Read(b, 1);
  eb^.rLabel := b;

// "with eb^" gives us access to rGraphic, rText, rComment, and rApp

  with eb^ do
  begin

// a graphic extension

    if (rLabel = kGifLabelGraphic) then
    begin
      ReadSourceInteger(1, rGraphic.rBlockSize); // block size
      if (rGraphic.rBlockSize <> 4) then
        GIF_Error(5);

      ReadSourceInteger(1, n); // packed bit field
      rGraphic.rDisposal := ((n shr 2) and $07);
      rGraphic.rUserInputValid := ((n and $02) <> 0);
      rGraphic.rTransparentValid := ((n and $01) <> 0);

      ReadSourceInteger(2, rGraphic.rDelayTime); // delay time
      ReadSourceInteger(1, rGraphic.rTransparentIndex); // transparent color
      ReadSourceInteger(1, n); // block terminator
      if (n <> 0) then
        GIF_Error(7);
    end

// a comment extension

    else if (rLabel = kGifLabelComment) then
    begin
      rComment.rDataBlockList := TList.Create;
      ReadDataBlockList(rComment.rDataBlockList);
    end

// a plain text extension

    else if (rLabel = kGifLabelText) then
    begin
      ReadSourceInteger(1, rText.rBlockSize); // block size
      if (rText.rBlockSize <> 12) then
        GIF_Error(5);
      ReadSourceInteger(2, rText.rGridLeft); // grid position
      ReadSourceInteger(2, rText.rGridTop); // grid position
      ReadSourceInteger(2, rText.rGridWidth); // grid size
      ReadSourceInteger(2, rText.rGridHeight); // grid size
      ReadSourceInteger(1, rText.rCellWidth); // character cell size    {LDB}{was 2 bytes}
      ReadSourceInteger(1, rText.rCellHeight); // character cell size
      ReadSourceInteger(1, rText.rForegroundIndex); // foreground color
      ReadSourceInteger(1, rText.rBackgroundIndex); // background color
      rText.rDataBlockList := TList.Create; // list of text data blocks
      ReadDataBlockList(rText.rDataBlockList);
    end

// an application extension

    else if (rLabel = kGifLabelApplication) then
    begin
      ReadSourceInteger(1, rApp.rBlockSize); // block size
      if (rApp.rBlockSize <> 11) then //GIF_Error(5);       {LDB} allow other  blocksizes
      begin
        fIOStream.Position := fIOStream.Position + rApp.rBlockSize;
        rApp.rDataBlockList := TList.Create;
        ReadDataBlockList(rApp.rDataBlockList);
      end
      else
      begin
        fIOStream.Read(rApp.rIdentifier, 8); // application identifier
        fIOStream.Read(rApp.rAuthentication, 3); // authentication code
        rApp.rDataBlockList := TList.Create;
        ReadDataBlockList(rApp.rDataBlockList);
        if rApp.rIdentifier = 'NETSCAPE' then
          ExtractLoopCount(rApp.rDataBlockList);
      end;
    end

// unknown type

    else
    begin
      GIF_ErrorMessage('unknown extension: ' + IntToHex(rLabel, 4));
    end;
  end; // with eb^
end;


{ ---------------------------------------------------------------------------- }
{ read a 1 or 2-byte integer from the source stream }

procedure TGif.ReadSourceInteger(size: integer; out value: integer);
var
  b: byte;
  w: word;
begin
  if (size = 1) then
  begin
    fIOStream.Read(b, 1);
    value := b;
  end
  else if (size = 2) then
  begin
    fIOStream.Read(w, 2);
    value := w;
  end
  else
  begin
    GIF_Error(8);
  end;
end;

{ ---------------------------------------------------------------------------- }
{ decode the compressed data blocks into a bitmap }

procedure TGif.LZWDecode(pID: PGifImageDescriptor);
var
  pc: integer; // next compressed code parsed from input
  cc: integer; // current code to translate
  oc: integer; // old code translated
  tt: integer; // temp storage for OldCode
  Done: boolean;
begin

// init local data

  LZWInit(pID);
  LZWReset;

// do everything within the ZIP record

  with fZipData^ do
  begin

// parse next code from BitString

    pc := LZWGetCode;
    oc := pc;
    Done := False;
    while (pc <> rEndCode) and not Done do
    begin

// reset decode parameters and save first code

      if (pc = rClearCode) then
      begin
        rCurSize := rID^.rLZWSize + 1;
        rCurSlot := rEndCode + 1;
        rTopSlot := (1 shl rCurSize);
        while (pc = rClearCode) do
          pc := LZWGetCode;
        if (pc = rEndCode) then
          GIF_Error(13);
        if (pc >= rCurSlot) then
          pc := 0;
        oc := pc;
        LZWSaveCode(pc);
      end

// find a code in the table and write out translation

      else
      begin
        cc := pc;
        if (cc < rCurSlot) then
        begin
          LZWDecodeCode(cc);
          if (rCurSlot <= rTopSlot) then
          begin
            LZWSaveSlot(oc, cc);
            oc := pc;
          end;
          LZWCheckSlot;
        end

// add a new code to the decode table

        else
        begin
          if (cc <> rCurSlot) then
            GIF_Error(13);
          tt := oc;
          while (oc > rHighCode) do
            oc := rPrefix[oc];
          if (rCurSlot <= rTopSlot) then
            LZWSaveSlot(tt, oc);
          LZWCheckSlot;
          LZWDecodeCode(cc);
          oc := pc;
        end;
      end;

// write out translated bytes to the image storage

      LZWWriteBitmap;
      if fDataStream.Position < fDataStream.Size then
        pc := LZWGetCode
      else
        Done := True;
      rMaxVal := false;

    end; // while not EOI

  end; // with

// done with stack space

  LZWFinit;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWInit(pID: PGifImageDescriptor);
begin

// get a valid record?

  if (pID = nil) then
    GIF_Error(11);

// make sure we can actually decode this turkey

// if ((pID^.rLZWSize < 2) or (pID^.rLZWSize > 9)) then GIF_Error(12);

// allocate stack space

  new(fZipData);
  if (fZipData = nil) then
    OutOfMemoryError;

// init data block

  fillchar(fZipData^, sizeof(TGifZip), 0);
  fZipData^.rID := pID;
  fZipData^.rCT := fColorTableList.Items[pID^.rLocalColorTable];

// reset data stream

  fDataStream.Position := 0;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWFinit;
begin
  if (fZipData <> nil) then
    dispose(fZipData);
  fZipData := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.LZWReset;
var
  i: integer;
begin
  with fZipData^ do
  begin
    for i := 0 to (kGifCodeTableSize - 1) do
    begin
      rPrefix[i] := 0;
      rSuffix[i] := 0;
    end;

    rCurSize := rID^.rLZWSize + 1;
    rClearCode := (1 shl rID^.rLZWSize);
    rEndCode := rClearCode + 1;
    rHighCode := rClearCode - 1;
    rFirstSlot := (1 shl (rCurSize - 1)) + 2;
    rNextSlot := rFirstSlot;
    rMaxVal := false;
  end; // with
end;


{ ---------------------------------------------------------------------------- }
{ get the next code from the BitString }
{ CurrentSize specifies the number of bits to get }

function TGif.LZWGetCode: integer;
var
  n: integer;
  cc: integer;
  mask: integer;
  b: byte;
begin
  with fZipData^ do
  begin

// make sure we have enough bits

    while (rCurSize > rBits) do
    begin
      if (fDataStream.Position >= fDataStream.Size) then
        b := 0
      else
        fDataStream.Read(b, 1);
      n := b;
      n := (n shl rBits); // scoot bits over to avoid previous data
      rBitString := (rBitString or n); // put bits in the BitString
      rBits := rBits + 8; // number of bits in a byte
    end;


// get the code, then dump the bits we used from the BitString

    case rCurSize of
      0: mask := 0;
      1: mask := $0001;
      2: mask := $0003;
      3: mask := $0007;
      4: mask := $000F;
      5: mask := $001F;
      6: mask := $003F;
      7: mask := $007F;
      8: mask := $00FF;
      9: mask := $01FF;
      10: mask := $03FF;
      11: mask := $07FF;
      12: mask := $0FFF;
    else
      begin
        GIF_Error(12);
        Mask := 0; //stop warning
      end;
    end;

    cc := (rBitString and mask); // mask off bits wanted
    rBitString := (rBitString shr rCurSize); // delete bits we just took
    rBits := rBits - rCurSize; // number of bits left in BitString
  end; // with

// done

  LZWGetCode := cc;
end;

{ ---------------------------------------------------------------------------- }
{ save a code value on the code stack }

procedure TGif.LZWSaveCode(Code: integer);
begin
  with fZipData^ do
  begin
    rCodeStack[rSP] := Code;
    rSP := rSP + 1;
  end;
end;


{ ---------------------------------------------------------------------------- }
{ decode the CurrentCode into the clear-text pixel value }
{ mainly, just save the CurrentCode on the output stack, along with }
{ whatever prefixes go with it }

procedure TGif.LZWDecodeCode(var Code: integer);
begin
  with fZipData^ do
  begin
    while (Code > rHighCode) do
    begin
      LZWSaveCode(rSuffix[Code]);
      Code := rPrefix[Code];
    end;
    LZWSaveCode(Code);
  end;
end;


{ ---------------------------------------------------------------------------- }
{ save a new prefix/suffix pair }

procedure TGif.LZWSaveSlot(Prefix, Suffix: integer);
begin
  with fZipData^ do
  begin
    rPrefix[rCurSlot] := Prefix;
    rSuffix[rCurSlot] := Suffix;
    rCurSlot := rCurSlot + 1;
  end;
end;



{ ---------------------------------------------------------------------------- }
{ given current line number, compute the next line to be filled }
{ this gets a little tricky if an interlaced image }
{ what is the purpose of this interlace, anyway?  it doesn't save space, }
{ and I can't imagine it makes for any faster image disply or loading }

procedure TGif.LZWIncrPosition;
var
  n: integer;
begin
  with fZipData^ do
  begin

// if first pass, make sure CurPass was initialized

    if (rCurPass = 0) then
      rCurPass := 1;

// incr X position

    rCurX := rCurX + 1;

// bumping Y ?

    if (rCurX >= rID^.rWidth) then
    begin
      rCurX := 0;

// if not interlaced image, then just move down the page

      if (not rID^.rInterlaced) then
      begin
        rCurY := rCurY + 1;
      end

// interlaced images select the next line by some archane black-magical sheme

      else
      begin
        case rCurPass of // delta to next row on this pass
          1: n := 8;
          2: n := 8;
          3: n := 4;
          4: n := 2;
        else
          begin
            GIF_Error(21);
            n := 0; //prevent warning
          end;
        end;

        rCurY := rCurY + n;

// if past the end of the bitmap, start next pass

        if (rCurY >= rID^.rHeight) then
        begin
          rCurPass := rCurPass + 1;
          if (rCurPass = 5) then
            rCurPass := 1;
          case rCurPass of // first line for given pass
            1: n := 0;
            2: n := 4;
            3: n := 2;
            4: n := 1;
          else
            GIF_Error(21);
          end;

          rCurY := n;
        end;
      end;
    end;
  end; // with
end;

{ ---------------------------------------------------------------------------- }
{ see if it is time to add a new slot to the decoder tables }

procedure TGif.LZWCheckSlot;
begin
  with fZipData^ do
  begin
    if (rCurSlot >= rTopSlot) then
    begin
      if (rCurSize < 12) then
      begin
        rTopSlot := (rTopSlot shl 1);
        rCurSize := rCurSize + 1;
      end
      else
      begin
        rMaxVal := true;
      end;
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ empty the Codes stack and write to the Bitmap }

procedure TGif.LZWWriteBitmap;
var
  i, n: integer;
  j: longint;
  p: PByte;
begin
  with fZipData^ do
  begin
    for n := (rSP - 1) downto 0 do
    begin
      rCount := rCount + 1;

// get next code from the stack, and index into PixelList

      i := rCodeStack[n];
      j := (rCurY * rID^.rWidth) + rCurX;
      if ((0 <= j) and (j < rID^.rPixelCount)) then
      begin

// store the pixel index into PixelList

        p := PByte(PtrInt(rID^.rPixelList) + j);
        p^ := Byte(i);
      end;

      LZWIncrPosition;
    end;

    rSP := 0;
  end; // with
end;

{ ---------------------------------------------------------------------------- }
{ get the next pixel from the bitmap, and return it as an index into }
{ the colormap }

function TGif.LZWReadBitmap: integer;
var
  n: integer;
  j: longint;
  p: PByte;
begin
  with fZipData^ do
  begin
    if (rUnget) then
    begin
      n := rLast;
      rUnget := false;
    end
    else
    begin
      rCount := rCount + 1;
      j := (rCurY * rID^.rWidth) + rCurX;
      if ((0 <= j) and (j < rID^.rPixelCount)) then
      begin
        p := PByte(PtrInt(rID^.rPixelList) + j);
        n := ord(p^);
      end
      else
      begin
        n := 0;
      end;

      LZWIncrPosition;
    end;

    rLast := n;
  end; // with

  LZWReadBitmap := n;
end;

{ ---------------------------------------------------------------------------- }
{ PROCEDURES TO IMPLEMENT PROPERTIES ----------------------------------------- }

{ ---------------------------------------------------------------------------- }

function TGif.GetSignature: AnsiString;
var
  s: AnsiString;
begin
  s := fSignature^.rSignature;
  GetSignature := s;
end;


{ ---------------------------------------------------------------------------- }
{ return screen descriptor data pointer, or set a new record block }

function TGif.GetScreenDescriptor: PGifScreenDescriptor;
begin
  GetScreenDescriptor := fScreenDescriptor;
end;


{ ---------------------------------------------------------------------------- }

function TGif.GetImageCount: integer;
begin
  GetImageCount := fImageDescriptorList.Count;
end;


function TGif.GetImageDescriptor(image: integer): PGifImageDescriptor;
begin
  if ((image < 0) or (image >= fImageDescriptorList.Count)) then
    GIF_Error(15);
  GetImageDescriptor := fImageDescriptorList.Items[image];
end;


{ ---------------------------------------------------------------------------- }

function TGif.GetBitmap(image: integer): TBitmap;
var
  p: PGifImageDescriptor;
  b: TBitmap;
begin
  p := GetImageDescriptor(image);
  if (p^.rBitmap = nil) then
    MakeBitmaps;
  b := p^.rBitmap;

  GetBitmap := b;
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetColorTableCount: integer;
begin
  GetColorTableCount := fColorTableList.Count;
end;


function TGif.GetColorTable(table: integer): PGifColorTable;
begin
  if ((table < 0) or (table >= fColorTableList.Count)) then
    GIF_Error(15);
  GetColorTable := fColorTableList.Items[table];
end;

function TGif.GetImageDelay(Image: integer): integer;
var
  gx: PGifExtensionGraphic;
begin
  gx := FindGraphicExtension(Image);
  if (gx <> nil) then
  begin
    Result := gx^.rDelayTime;
    if Result < 1 then
      Result := 1;
  end
  else
    Result := 1;
end;

function TGif.GetImageDisposal(Image: integer): integer;
var
  gx: PGifExtensionGraphic;
begin
  gx := FindGraphicExtension(Image);
  if (gx <> nil) then
    Result := gx^.rDisposal and 3
  else
    Result := 0;
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetColorIndex(image, x, y: integer): integer;
var
  i, n: integer;
  id: PGifImageDescriptor;
  p: PByte;
begin
  if ((image < 0) or (image >= fImageDescriptorList.Count)) then
    GIF_Error(15);
  id := fImageDescriptorList.Items[image];
  if ((x < 0) or (x >= id^.rWidth)) then
    GIF_Error(15);
  if ((y < 0) or (y >= id^.rHeight)) then
    GIF_Error(15);

  n := (y * id^.rWidth) + x;
  p := PByte(PtrInt(id^.rPixelList) + n);
  i := ord(p^);

  GetColorIndex := i;
end;

{ ---------------------------------------------------------------------------- }
{ transparent color for each individual image.
  returns -1 if none. }

function TGif.GetTransparentIndex(image: integer): integer;
var
  i: integer;
  gx: PGifExtensionGraphic;
begin
  i := -1;
  gx := FindGraphicExtension(image);
  if (gx <> nil) and (gx^.rTransparentValid) then {LDB}
    i := gx^.rTransparentIndex;

  GetTransparentIndex := i;
end;

{ ---------------------------------------------------------------------------- }
{ transparent color for all images }
{LDB Changed to always return the standard used for the transparent color}

function TGif.GetTransparentColor: TColor;
begin
  GetTransparentColor := TransColor;
end;

procedure TGif.ExtractLoopCount(List: TList);
begin
  if List.Count > 0 then
    with PGifDataBlock(List[0])^ do
      if rSize = 3 then
        FLoopCount := rData[2] + rData[3] * 256;
end;

{ ---------------------------------------------------------------------------- }

function TGif.GetImageLeft(image: integer): integer;
var
  id: PGifImageDescriptor;
begin
  id := GetImageDescriptor(image);
  GetImageLeft := id^.rLeft;
end;

function TGif.GetImageTop(image: integer): integer;
var
  id: PGifImageDescriptor;
begin
  id := GetImageDescriptor(image);
  GetImageTop := id^.rTop;
end;

function TGif.GetImageWidth(image: integer): integer;
var
  id: PGifImageDescriptor;
begin
  id := GetImageDescriptor(image);
  GetImageWidth := id^.rWidth;
end;

function TGif.GetImageHeight(image: integer): integer;
var
  id: PGifImageDescriptor;
begin
  id := GetImageDescriptor(image);
  GetImageHeight := id^.rHeight;
end;

function TGif.GetImageDepth(image: integer): integer;
var
  id: PGifImageDescriptor;
  ct: PGifColorTable;
begin
  id := GetImageDescriptor(image);
  ct := fColorTableList.Items[id^.rLocalColorTable];
  GetImageDepth := ct^.rSize;
end;


{ ---------------------------------------------------------------------------- }
{ GENERAL INTERNAL ROUTINES -------------------------------------------------- }

{ ---------------------------------------------------------------------------- }

procedure TGif.FreeDataBlockList(var list: TList);
var
  i: integer;
  db: PGifDataBlock;
begin
  if (list <> nil) then
  begin
    for i := 0 to (list.Count - 1) do
    begin
      db := list.Items[i];
      if (db <> nil) then
        dispose(db);
    end;

    list.Free;
  end;

  list := nil;
end;

{ ---------------------------------------------------------------------------- }

procedure TGif.FreeExtensionList(var list: TList);
var
  i: integer;
  ex: PGifExtension;
begin
  if (list <> nil) then
  begin
    for i := 0 to (list.Count - 1) do
    begin
      ex := list.Items[i];
      if (ex <> nil) then
      begin
        if (ex^.rLabel = kGifLabelComment) then
          FreeDataBlockList(ex^.rComment.rDataBlockList)
        else if (ex^.rLabel = kGifLabelText) then
          FreeDataBlockList(ex^.rText.rDataBlockList)
        else if (ex^.rLabel = kGifLabelApplication) then
          FreeDataBlockList(ex^.rApp.rDataBlockList);

        dispose(ex);
      end;
    end;

    list.Free;
  end;

  list := nil;
end;


{ ---------------------------------------------------------------------------- }
{ after an image has been LZW decoded, write a bitmap from the string of pixels }

{----------------TGif.MakeBitmaps}

procedure TGif.MakeBitmaps;
type
  LayoutType = packed record
    BFH: TBitmapFileHeader;
    BIH: TBitmapInfoHeader;
  end;
  PLayoutType = ^LayoutType;
var
  id: PGifImageDescriptor;
  ct: PGifColorTable;
  FullWidth, PixelSize, FileSize: integer;
  Stream: TMemoryStream;
  PL: PLayoutType;
  Color: TColor;
  Index: PtrInt;
  Pix, P: PByte;
  I, X, Y, N: integer;
  TrIndex: integer;
begin
  for i := 0 to (fImageDescriptorList.Count - 1) do
  begin
    id := fImageDescriptorList.Items[i];
    if ((id <> nil) and (id^.rBitmap = nil)) then // don't do it again
      with id^ do
      begin
        FullWidth := rWidth * 3;
        if FullWidth and $3 <> 0 then
          FullWidth := (FullWidth and $FFFFFFFC) + $4;
        PixelSize := FullWidth * rHeight;
        FileSize := Sizeof(LayoutType) + PixelSize;
        Stream := TMemoryStream.Create;
        try
          Stream.Size := FileSize;
          PL := Stream.Memory;
          FillChar(PL^, FileSize, 0);

          with PL^.BFH do
          begin
            bfType := 19778;
            bfSize := FileSize;
            bfReserved1 := 0;
            bfReserved2 := 0;
            bfOffBits := Sizeof(LayoutType);
          end;
          with PL^.BIH do
          begin
            biSize := Sizeof(TBitmapInfoHeader);
            biWidth := rWidth;
            biHeight := rHeight;
            biPlanes := 1;
            biBitCount := 24;
            biCompression := 0;
            biSizeImage := 0;
            biXPelsPerMeter := 0;
            biYPelsPerMeter := 0;
            biClrUsed := 0;
            biClrImportant := 0;
          end;

          ct := fColorTableList.Items[rLocalColorTable];
          TrIndex := GetTransparentIndex(i);
          if (TrIndex >= 0) and (TrIndex < ct^.rSize) then
            {change transparent color to something that won't likely match any other color}
            ct^.rColors[TrIndex] := TransColor;

          N := 0;
          Pix := PByte(PtrInt(PL) + Sizeof(LayoutType));
          for Y := rHeight - 1 downto 0 do
          begin
            P := PByte(PtrInt(Pix) + (Y * FullWidth));
            for X := 0 to rWidth - 1 do
            begin
              Index := PtrInt(PByte(PtrInt(rPixelList) + N)^);
              Color := ct^.rColors[Index];
              P^ := Byte((Color shr 16) and $FF);
              Inc(P);
              P^ := Byte((Color shr 8) and $FF);
              Inc(P);
              P^ := Byte(Color and $FF);
              Inc(P);
              Inc(N);
            end;
          end;
          rBitmap := TBitmap.Create;
{$IFNDEF UseCLX}
          rBitmap.HandleType := bmDIB;
{$ENDIF}
          rBitmap.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;

// is bitmap transparent?

        if ((0 <= TrIndex) and (TrIndex < ct^.rSize)) then
        begin
          rBitmap.Transparent := true;
          rBitmap.TransparentMode := tmFixed;
          rBitmap.TransparentColor := ct^.rColors[TrIndex];
        end;
      end;
  end;
end;

{----------------TGif.GetStripBitmap}
type
  LayoutType = packed record
    BFH: TBitmapFileHeader;
    BIH: TBitmapInfoHeader;
  end;
  PLayoutType = ^LayoutType;

{$ifdef LCL}
function CreateMask(Bitmap: TBitmap; AColor: TColor): TBitmap;
var
  IntfImage: TLazIntfImage;
  x, y, stopx, stopy: Integer;
  ImgHandle, MskHandle: HBitmap;
  TransColor: TColor;
begin
  // this convertion copied from TRasterImage.CreateMask() and modified:
  IntfImage := TLazIntfImage.Create(0,0,[]);
  try
    MskHandle := CreateBitmap(Bitmap.Width, Bitmap.Height, 1, 1, nil);
    IntfImage.LoadFromBitmap(Bitmap.BitmapHandle, MskHandle);
    DeleteObject(MskHandle);

    stopx := IntfImage.Width - 1;
    stopy := IntfImage.Height - 1;

    if AColor <> clDefault then
      TransColor := ColorToRGB(AColor)
    else
      TransColor := FPColorToTColor(IntfImage.Colors[0, stopy]);

    for y := 0 to stopy do
      for x := 0 to stopx do
        IntfImage.Masked[x,y] := FPColorToTColor(IntfImage.Colors[x,y]) = TransColor;

    IntfImage.CreateBitmaps(ImgHandle, MskHandle);
    DeleteObject(ImgHandle);
    Result := TBitmap.Create;
    Result.Handle := MskHandle;
  finally
    IntfImage.Free;
    Bitmap.Free;
  end;
end;
{$endif}

function TGif.GetStripBitmap(out Mask: TBitmap): TBitmap; {LDB}
{This is a single bitmap containing all the frames.  A mask is also provided
 if the GIF is transparent.  Each Frame is set up so that it can be transparently
 blted to a background.}
var
  id: PGifImageDescriptor;
  ct: PGifColorTable;
  FullWidth, PixelSize, FileSize: integer;
  Stream, MStream: TMemoryStream;
  PL, MPL: PLayoutType;
  Color: TColor;
  Index: PtrInt;
  Pix, P, MPix, MP, PRight: PByte;
  I, X, Y, N: integer;
  TrIndex: integer;
  C: Byte;
  IsTransparent: boolean;
begin
  MStream := nil;
  Result := nil;
  Mask := nil;
  MP := nil;
  MPix := nil;
{find size needed for strip bitmap}
  FullWidth := Width * 3 * ImageCount; {3 bytes per pixel}
  if FullWidth and $3 <> 0 then {make sure it is DWord boundary}
    FullWidth := (FullWidth and $FFFFFFFC) + $4;
  PixelSize := FullWidth * Height;
  FileSize := Sizeof(LayoutType) + PixelSize;
  if (FileSize > 200000000) or Transparent and (FileSize > 100000000) then
    GIF_Error(25);
  Stream := TMemoryStream.Create;
  try
    Stream.Size := FileSize;
    PL := Stream.Memory;
    FillChar(PL^, FileSize, 0);

    with PL^.BFH do
    begin {set up the bitmap file header}
      bfType := 19778;
      bfSize := FileSize;
      bfReserved1 := 0;
      bfReserved2 := 0;
      bfOffBits := Sizeof(LayoutType);
    end;
    with PL^.BIH do
    begin {and the bitmap info header}
      biSize := Sizeof(TBitmapInfoHeader);
      biWidth := Width * ImageCount;
      biHeight := Height;
      biPlanes := 1;
      biBitCount := 24; {will use 24 bit pixel}
      biCompression := 0;
      biSizeImage := 0;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
    end;

    Pix := PByte(PtrInt(PL) + Sizeof(LayoutType)); {where pixels start}

    IsTransparent := Transparent;
    if IsTransparent then
    begin {set up a mask similarly}
      MStream := TMemoryStream.Create;
      MStream.Size := FileSize;
      MPL := MStream.Memory;
      Move(PL^, MPL^, FileSize); {for now, this is a direct copy}
      MPix := PByte(PtrInt(MPL) + Sizeof(LayoutType)); {where mask pixels start}
      FillChar(MPix^, PixelSize, $FF); {Need to make first frame totally transparent}
    end;

    for i := 0 to (fImageDescriptorList.Count - 1) do {for all the frames}
    begin
      id := fImageDescriptorList.Items[i];
      if (id <> nil) then
        with id^ do
        begin
          ct := fColorTableList.Items[rLocalColorTable];
          TrIndex := GetTransparentIndex(i);

          N := 0; {pixel index in rPixelList, the frame source pixels}
          for Y := Height - 1 downto Math.Max(Height - rHeight, ImageTop[I]) do
          begin
          {find the start of each frame row in destination.  Note that the source
           frame may be smaller than the destination and positioned according to
           imagetop and imageleft}
            P := PByte(PtrInt(Pix) + ((Y - ImageTop[i]) * FullWidth) + i * Width * 3 + ImageLeft[i] * 3);
            PRight := PByte(PtrInt(P) + Width * 3);
            if IsTransparent then {same for mask}
              MP := PByte(PtrInt(MPix) + ((Y - ImageTop[i]) * FullWidth) + i * Width * 3 + ImageLeft[i] * 3);
            for X := 0 to rWidth - 1 do
            begin
              if PtrInt(P) < PtrInt(PRight) then {prevent write beyond proper right side in case rwidth to wide}
              begin
                Index := PtrInt(PByte(PtrInt(rPixelList) + N)^); {Source pixel index in colortable}
                Color := ct^.rColors[Index]; {its color}
              {for frames after the 0th, only the non transparent pixels are written
               as writing transparent ones might cover results copied from the previous frame}
                if (Index <> trIndex) then
                begin
                  P^ := Byte((Color shr 16) and $FF);
                  Inc(P);
                  P^ := Byte((Color shr 8) and $FF);
                  Inc(P);
                  P^ := Byte(Color and $FF);
                  Inc(P);
                end
                else if i = 0 then
                begin {transparent pixel, first frame, write black}
                  P^ := 0;
                  Inc(P);
                  P^ := 0;
                  Inc(P);
                  P^ := 0;
                  Inc(P);
                end
                else
                  Inc(P, 3); {ignore transparent pixel}
                if IsTransparent then {also do the mask}
                begin
                  if Index = trIndex then
                    C := $FF {transparent part is white}
                  else
                    C := 0; {non transparent is black}
                {again for frames after the 0th, only non-transparent pixels are written}
                  if (i = 0) or (C = 0) then
                  begin
                    MP^ := C;
                    Inc(MP);
                    MP^ := C;
                    Inc(MP);
                    MP^ := C;
                    Inc(MP);
                  end
                  else
                    Inc(MP, 3);
                end;
              end;
              Inc(N); {bump source pixel index}
            end;
          end;
        end;
    {Now copy this frame to the next (unless it the last one).  This serves as a
     background for the next image.  This is all that's needed for the dtDoNothing
     disposal method but will be fixed up for dtBackground below}
      if (i < fImageDescriptorList.Count - 1) then
      begin
        for Y := Height - 1 downto 0 do
        begin {copy line by line}
          P := PByte(PtrInt(Pix) + (Y * FullWidth) + i * Width * 3);
          if IsTransparent then
            MP := PByte(PtrInt(MPix) + (Y * FullWidth) + i * Width * 3);
          Move(P^, PByte(PtrInt(P) + Width * 3)^, Width * 3);
          if IsTransparent then
            Move(MP^, PByte(PtrInt(MP) + Width * 3)^, Width * 3);
        end;
      {for dtBackground, fill the mask area occupied by the current copied image with
       white. This makes it transparent so the original background will appear here
       (although the next image will no doubt write on part of this area.}
        if IsTransparent and (ImageDisposal[i] in [2, 3]) then {dtToPrevious run as dtBackground as it seems other browsers do this}
          with id^ do
            for Y := Height - 1 downto Math.Max(Height - rHeight, ImageTop[I]) do
            begin
              MP := PByte(PtrInt(MPix) + ((Y - ImageTop[i]) * FullWidth) + (i + 1) * Width * 3 + ImageLeft[i] * 3);
              FillChar(MP^, rWidth * 3, $FF);
            end;
      end;
    end;

    Result := TBitmap.Create;
{$IFNDEF UseCLX}
    Result.HandleType := bmDIB;
{$ENDIF}
    Result.LoadFromStream(Stream); {turn the stream just formed into a TBitmap}
    if IsTransparent then
    begin
      Mask := TBitmap.Create;
      Mask.HandleType := bmDIB;
      Mask.LoadFromStream(MStream);
{$ifdef LCL}
      // setting to monochrome not yet implemented
      Mask := CreateMask(Mask, clWhite);
{$else}
      Mask.Monochrome := True; {crunch mask into a monochrome TBitmap}
{$endif}
    end;
    Stream.Free;
    MStream.Free;
  except
    Stream.Free;
    MStream.Free;
    Mask.Free;
    Result.Free;
    raise;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ find the graphic extension for an image }

function TGif.FindGraphicExtension(image: integer): PGifExtensionGraphic;
var
  n: integer;
  id: PGifImageDescriptor;
  ex: PGifExtension;
  gx: PGifExtensionGraphic;
begin
  gx := nil;
  id := fImageDescriptorList.Items[image];
  if (id^.rExtensionList <> nil) then
  begin
    for n := 0 to (id^.rExtensionList.Count - 1) do
    begin
      ex := id^.rExtensionList.Items[n];
      if ((ex^.rLabel = kGifLabelGraphic) and (gx = nil)) then
      begin
        gx := @(ex^.rGraphic);
      end;
    end;
  end;

  FindGraphicExtension := gx;
end;

{ ---------------------------------------------------------------------------- }
{ find the color within the color table; returns 0..255 }
{ return -1 if color not found }

function TGif.FindColorIndex(c: TColor; ct: PGifColorTable): integer;
var
  i, n: integer;
begin
  n := -1;
  for i := 0 to (ct^.rSize - 1) do
  begin
    if ((n < 0) and (ct^.rColors[i] = c)) then
      n := i;
  end;

  FindColorIndex := n;
end;

{ ---------------------------------------------------------------------------- }
{ RAISE AN ERROR ------------------------------------------------------------- }

procedure GIF_Error(n: integer);
begin
  GIF_ErrorCode := n;
  GIF_ErrorString := kGifErrorMessages[n];
  raise EInvalidGraphicOperation.CreateFmt('TGif: %s', [GIF_ErrorString]);
end;


procedure GIF_ErrorMessage(m: string);
begin
  GIF_ErrorCode := 6;
  GIF_ErrorString := m;
  raise EInvalidGraphicOperation.CreateFmt('TGif: %s', [GIF_ErrorString]);
end;

end.
