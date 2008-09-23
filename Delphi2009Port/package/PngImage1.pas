{Version 9.45}
{***************************************************************}
{*                    PNGImage1.PAS                            *}
{*                                                             *}
{*   Thanks to Gustavo Daud for this Pascal implementation     *}
{*   for PNG Images.                                           *}
{*                                                             *}
{*   Thanks also to Paul TOTH for his Delphi 3 Adaptation      *}
{*                                                             *}
{***************************************************************}

{*******************************************************}
{                                                       }
{       Portable Network Graphics decoder               }
{       * decode & encode png files in delphi *         }
{                                                       }
{       EMAIL: gustavodaud@uol.com.br                   }
{                                                       }
{*******************************************************}

{ Delphi 3 compatibility and french translation by Paul TOTH <tothpaul@free.fr>}

{$i htmlcons.inc}

unit PngImage1;

{$R-}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;


//Supporting code for versions prior 5
{$IFDEF VER125} {$DEFINE PRIORDELPHI5} {$ENDIF}
{$IFDEF VER120} {$DEFINE PRIORDELPHI5} {$ENDIF}
{$IFDEF VER110} {$DEFINE PRIORDELPHI5} {$DEFINE PRIORDELPHI3} {$ENDIF}
{$IFDEF VER100} {$DEFINE PRIORDELPHI5} {$DEFINE PRIORDELPHI3} {$ENDIF}
{$IFDEF VER93}  {$DEFINE PRIORDELPHI5} {$DEFINE PRIORDELPHI3} {$ENDIF}
{$IFDEF VER80}  {$DEFINE PRIORDELPHI5} {$DEFINE PRIORDELPHI3} {$ENDIF}

resourcestring
  {.$INCLUDE Portuguese.TXT}
  {$INCLUDE English.TXT}
  {. $INCLUDE French.TXT}

{Portable Network Graphics implementation}
type
  {Encoding filter}
  TFilterRow = array[0..4] of PByteArray;
  TEncodeFilter = (efNone, efSub, efUp, efAverage, efPaeth);
  TEncodeFilterSet = set of TEncodeFilter;

  {:Chunk type}
  TChunkType = Array[0..3] of char;

  {Forward declarations}
  TPNGImage = class;
  TChunkList = class;
  TChunkGAMA = class;
  TChunkIHDR = class;

  {:This class handles the chunks}
  TChunk = class
    constructor Create(AOwner: TChunkList); virtual;
    destructor Destroy; override;
  private
    fList  : TChunkList;
    fStream: TMemoryStream;

    function GetSize: Integer;

    {Returns pointer to the most common chunk types}
    function GetIHDR   : TChunkIHDR;
    function GetGAMA   : TChunkGAMA;
    {Return a pointer to the TPNGImage owner}
    function GetBitmap : TPNGImage;

  protected
    fType  : TChunkType;
    function GetIndex: Integer;
    procedure DoAction; virtual;

    property IHDR  : TChunkIHDR read GetIHDR;
    property GAMA  : TChunkGAMA read GetGama;
    property Bitmap: TPNGImage  read GetBitmap;
    property Stream: TMemoryStream read fStream;
  public
    procedure Assign(Source: TChunk); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Index: Integer read GetIndex;
    property Owner: TChunkList read fList;
    property Size: Integer read GetSize;
    (*property ChunkType: TChunkType read fType;*) //LDB will not compile in C++Builder
  end;


  {:IEND Chunk, 0 bytes length}
  TChunkIEND = class(TChunk);

  {:tEXt Chunk, dynamic size, minimum 2 bytes (null separators)}
  TChunkTEXT = Class(TChunk)
    constructor Create(AOwner: TChunkList); override;
  private
    function GetValue(Index: Integer): String;
    procedure SetValue(Index: Integer; Value: String);
  public
    property Keyword: String index 0 read GetValue write SetValue;
    property Text: String index 1 read GetValue write SetValue;
  end;

  {:zTXt Chunk, dynamic size}
  TChunkZTXT = Class(TChunk)
  private
    function GetValue(Index: Integer): String;
    procedure SetValue(Index: Integer; Value: String);
  public
    property Keyword: String index 0 read GetValue write SetValue;
    property Text: String index 1 read GetValue write SetValue;
  end;

  {:gAMA Chunk, 4 bytes length}
  TChunkGAMA = class(TChunk)
    constructor Create(AOwner: TChunkList); override;
    procedure Assign(Source: TChunk); override;
  protected
    GammaTable,
    InverseTable: Array[Byte] of Byte;
    procedure DoAction; override;
  private
    function GetValue: Cardinal;
    procedure SetValue(Value: Cardinal);
  public
    property Value: Cardinal read GetValue write SetValue;
  end;

  {:PLTE Chunk, dynamic length}
  TChunkPLTE = class(TChunk)
    destructor Destroy; Override;
  private
    fPalette: HPalette;
    function GetPalette: HPalette;
  public
    procedure SaveToStream(Stream: TStream); override;
    property Palette: HPalette read GetPalette;
  end;

  {:IHDR Chunk, 13 bytes length}
  TChunkIHDR = class(TChunk)
    procedure SaveToStream(Stream: TStream); override;
    constructor Create(AOwner: TChunkList); override;
  private
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;
    procedure SetWidth(Value: Cardinal);
    procedure SetHeight(Value: Cardinal);
    function GetValue(Index: Integer): Byte;
    procedure SetValue(Index: Integer; Value: Byte);
  public
    property Width: Cardinal read GetWidth write SetWidth;
    property Height: Cardinal read GetHeight write SetHeight;
    property BitDepth: Byte index 0 read GetValue write SetValue;
    property ColorType: Byte index 1 read GetValue write SetValue;
    property Compression: Byte index 2 read GetValue write SetValue;
    property Filter: Byte index 3 read GetValue write SetValue;
    property Interlaced: Byte index 4 read GetValue write SetValue;
  end;

  {:IDAT Chunk, dynamic size}
  TChunkIDAT = class(TChunk)
  public
    procedure SaveToStream(Stream: TStream); override;
  protected
    function GetBufferWidth: Integer;
    procedure FilterRow(Filter: Byte; CurrentRow, LastRow: pbytearray;
     offset, row_buffer_width: Integer);
    function EncodeFilterRow(row_buffer: pbytearray;
      Filter_buffers: TFilterRow; row_width, filter_width: Cardinal): Integer;
    procedure DoAction; override;
    function GetOffset: Integer;
    procedure EncodeImage;
    procedure SetupPixelFormat;
    procedure DecodeNonInterlacedRow(ImageData: Pointer; Data: pByteArray;
      RowBytes: Integer; GamaChunk: TChunkGama);
    procedure DecodeInterlacedRow(ImageData: Pointer; Data: pByteArray;
      ColStart, ColIncrement, RowBytes, Pass: Integer; GamaChunk: TChunkGama);
  end;

  {:tIME Chunk, 7 bytes}
  TChunkTIME = class(TChunk)
    constructor Create(AOwner: TChunkList); override;
    function GetDateTime: TDateTime;
  private
    procedure SetDateTime(const Value: TDateTime);
  public
    property DateTime: TDateTime read GetDateTime write SetDateTime;
  end;

  {:tRNS Chunk, dynamic length}
  TChunkTRNS = class(TChunk)
  private
    function GetRGBColor: TColor;
  public
    procedure SaveToStream(Stream: TStream); override;
    property RGBColor: TColor read GetRGBColor;
  end;


  {:Chunk class handler}
  TChunkClass = Class of TChunk;

  {:Record containg a chunk class info}
  pChunkClassInfo = ^TChunkClassInfo;
  TChunkClassInfo = record
    ChunkType:  TChunkType;
    ChunkClass: TChunkClass;
  end;

  {:This class contains the avaliable kinds of TChunk class}
  TChunkClasses = class
    destructor Destroy; Override;
  private
    fList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TChunkClassInfo;
  public
    property Count: Integer read GetCount;
    function IndexOfType(Item: TChunkType): Integer; { Paul - overload; }
    function IndexOfClass(Item: TChunkClass): Integer; { Paul - overload; }
    procedure Add(ChunkType: TChunkType; ChunkClass: TChunkClass);
    property Item[Index: Integer]: TChunkClassInfo read GetItem; default;
  end;

  {:This class contains the list of avaliable chunks for a TPNGImage }
  {:object class.                                                    }
  TChunkList = class
    constructor Create(AOwner: TPNGImage);
    destructor Destroy; override;
  private
    fImage: TPNGImage;
    fList : TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TChunk;
  public
    property Owner: TPNGImage read fImage;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TChunk read GetItem; default;
    procedure Move(Index1, Index2: Integer);
    function AddItem(Item: TChunk): TChunk; { Paul - overload; }
    function AddClass(ChunkClass: TChunkClass): TChunk; { Paul - overload; }
    function AddStream(Stream: TStream): TChunk; { Paul - overload; }
    procedure Remove(Item: TChunk);
    function IndexOfChunk(Chunk: TChunk): Integer; { Paul - overload; }
    function IndexOfClass(ChunkClass: TChunkClass): Integer; { Paul - overload; }
    procedure Clear;
  end;

  {:This format handler is able to load and save booth interlaced and
    non interlaced Portable Network Graphics images using a ZLIB
    compression decoder}
  TPNGImage = class(TBitmap)
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  private
    fMask: TBitmap;
    fEncodeFilter: TEncodeFilterSet;
    fInterlacing: Boolean;
    fChunkList: TChunkList;
    procedure SetFilter(Value: TEncodeFilterSet);
  public
    procedure Assign(Source: TPersistent); override;
    property Filter: TEncodeFilterSet read fEncodeFilter write SetFilter;
    property Interlacing: Boolean read fInterlacing write fInterlacing;
    procedure Clear;
    property Chunks: TChunkList read fChunkList;
    class procedure RegisterChunkClass(ChunkType: TChunkType;
      ChunkClass: TChunkClass);
  end;

implementation

uses
  PNGZLIB1, Math;

{ Delphi versions prior 4 missing code}
{$IFDEF PRIORDELPHI5}
Procedure ReplaceTime(Var D:TDateTime; T:TDateTime);
 begin
  D:=D+T; // this work for PNGImage only !
 end;
{$ENDIF}

{ Delphi versions prior 3 missing code}
{$IFDEF PRIORDELPHI3}
Procedure ShowMessageFmt(msg:string; fmt:array of const);
 begin
  ShowMessage(Format(msg,fmt));
 end;
{$ENDIF}

var
  {Stores the avaliable kinds of TChunk}
  ChunkClasses: TChunkClasses;

const
  FILTERBUFFERCOUNT = 5;

  {Interlacing}
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);
  PassMask: array[0..6] of Byte = ($80, $08, $88, $22, $AA, $55, $FF);

  {Color types}
  Grayscale = 0;
  RGB = 2;
  Palette = 3;
  GrayscaleAlpha = 4;
  RGBAlpha = 6;

  {Filter types}
  FILTERNONE = 0;
  FILTERSUB = 1;
  FILTERUP = 2;
  FILTERAVERAGE = 3;
  FILTERPAETH = 4;

  {Valid PNG header (first 8 bytes)}
  PNGHeader: array[0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);

type
  pCardinal = ^Cardinal;

  {Default error handler for PNG format}
  EPNGImageException = Class(Exception);

  {:IHDR Chunk}
  pIHDRChunk = ^TIHDRChunk;
  TIHDRChunk = packed record
    {Width and height give the image dimensions in pixels}
    Width, Height: Cardinal;
    {Bit depth is a single-byte integer giving the number of bits }
    {per sample or per palette index (not per pixel). Valid values}
    {are 1, 2, 4, 8, and 16, although not all values are allowed  }
    {for all color types                                          }
    BitDepth,
    {Color type is a single-byte integer that describes the }
    {interpretation of the image data. Color type codes     }
    {represent sums of the following values:                }
    {1 (palette used)                                       }
    {2 (color used)                                         }
    {4 (alpha channel used).                                }
    {Valid values are 0, 2, 3, 4, and 6.                    }
    ColorType,
    {Compression method is a single-byte integer that indicates}
    {the method used to compress the image data. At present,   }
    {only compression method 0 (deflate/inflate compression    }
    {with a sliding window of at most 32768 bytes) is defined. }
    {All standard PNG images must be compressed with this      }
    {scheme. The compression method field is provided for      }
    {possible future expansion or proprietary variants.        }
    {Decoders must check this byte and report an error if it   }
    {holds an unrecognized code                                }
    Compression,
    {Filter method is a single-byte integer that indicates the }
    {preprocessing method applied to the image data before     }
    {compression. At present, only filter method 0  (adaptive  }
    {filtering with five basic filter types) is defined.       }
    Filter,
    {Interlace method is a single-byte integer that indicates  }
    {the transmission order of the image data. Two values are  }
    {currently defined: 0 (no interlace) or 1 (Adam7 interlace)}
    Interlaced: Byte;
  end;

  {tIME Chunk}
  pTIMEChunk = ^TTimeChunk;
  TTIMEChunk = Record
    Year    : Word;
    Month   : Byte;
    Day     : Byte;
    Hour    : Byte;
    Min     : Byte;
    Sec     : Byte;
  end;

  {Pixel memory access}
  pRGBLine = ^TRGBLine;
  TRGBLine = Array[Word] of TRGBTriple;
  pRGBALine = ^TRGBALine;
  TRGBALine = Array[Word] of TRGBQuad;

  {Standard PNG header}
  TPNGHeader = Array[0..7] of Byte;

procedure ConvertBits(Source: array of Pointer; Target: Pointer;
  Count: Cardinal; Mask: Byte; FSourceBPS, FTargetBPS: Byte); forward;

{Forward declaration for the CRC check function}
function crc(chunktype: tchunktype; buf: pbytearray;
  len: Integer): Cardinal; forward;

{:swaps high and low bytes of the given 32 bit value}
function SwapLong(Value: Cardinal): Cardinal;
asm
  BSWAP EAX
end;

{:Register a new chunk kind class}
procedure RegisterNewChunkClass(ChunkType: TChunkType; ChunkClass: TChunkClass);
begin
  {Add to the list}
  ChunkClasses.Add(ChunkType, ChunkClass);
end;

{:Extracted from PNG specification, returns paeth prediction of the values}
function PaethPredictor(a, b, c: Byte): Byte;
var
  p, pa, pb, pc: Integer;
begin
  { a = left, b = above, c = upper left }
  p := a + b - c;        { initial estimate }
  pa := Abs(p - a);      { distances to a, b, c }
  pb := Abs(p - b);
  pc := Abs(p - c);
  { return nearest of a, b, c, breaking ties in order a, b, c }
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

{:Default error handler method}
procedure CallError(ErrorCode: String);
begin
  {Show the error message}
  raise EPNGImageException.CreateFmt('Portable Network Graphics format handler ' +
      'error%s%s', [#13#10#13#10, ErrorCode]);
end;

{Returns the RGB color}
function TChunkTRNS.GetRGBColor: TColor;
var
  Data: pByteArray;
begin

  {Test if the current color type is RGB}
  if IHDR.ColorType <> RGB then
    CallError(PNG_INVALID_COLOR_TYPE);

  Data := fStream.Memory;
  Result := Windows.RGB(Data^[0], Data^[1], Data^[2]);
end;

{When the chunk is being created}
constructor TChunkTIME.Create(AOwner: TChunkList);
begin
  inherited;

  {Initial size and value}
  fStream.SetSize(7); { Paul - fStream.Size := 7; }
  DateTime := Now;
end;

{:Return the value of the date and time stamped on the chunk}
function TChunkTIME.GetDateTime: TDateTime;
var
  Data    : TTimeChunk;
begin
  {Makes sure that the stream size is 7}
  if fStream.Size <> 7 then
    CallError(TIME_CORRUPTED);

  {Read the data into the record}
  Data := pTimeChunk(fStream.Memory)^;
  Data.Year := SwapLong(Data.Year);

  {Return value}
  with Data do
    {Test if time is corrupted}
    try
      if Year = 0 then Year := 2000;
      Result := EncodeDate(Year, Month, Day);
      ReplaceTime(Result, EncodeTime(Hour, Min, Sec, 0));
    except
      ShowMessageFmt('Year: %d, Month: %d, Day: %d, Hour: %d, Min: %d,' +
        'Sec: %d', [Year, Month, Day, Hour, Min, Sec]);
      CallError(TIME_CORRUPTED);
    end;

end;

{:Set the value for the date and time in the chunk}
procedure TChunkTIME.SetDateTime(const Value: TDateTime);
var
  Year,
  Month,
  Day,
  Hour,
  Min,
  Sec,
  MSec   : word;
  Temp   : Byte;
begin
  fStream.Clear;

  {Get the datetime values}
  DecodeTime(Value, Hour, Min, Sec, MSec);
  DecodeDate(Value, Year, Month, Day);

  {Write the values}
  Year := SwapLong(Year);
  fStream.Write(Year, 2);
  Temp := Month; fStream.Write(Temp, 1);
  Temp := Day;   fStream.Write(Temp, 1);
  Temp := Hour;  fStream.Write(Temp, 1);
  Temp := Min;   fStream.Write(Temp, 1);
  Temp := Sec;   fStream.Write(Sec, 1);
end;

{When the chunk is being saved}
procedure TChunkTRNS.SaveToStream(Stream: TStream);
var
  Temp: Byte;
begin
  {Clear the data contents}
  fStream.Clear;

  {Write different transparency for different color formats}
  case IHDR.ColorType of
    RGB:
    begin
      {RGB data}
      Temp := GetRValue(Bitmap.TransparentColor);  fStream.Write(Temp, 1);
      Temp := GetGValue(Bitmap.TransparentColor);  fStream.Write(Temp, 1);
      Temp := GetBValue(Bitmap.TransparentColor);  fStream.Write(Temp, 1);
    end;
  else
    exit;
  end;

  inherited;
end;

{:Return value of one of the properties}
function TChunkZTXT.GetValue(Index: Integer): String;
var
  fKeyword: Pchar;
  DSize   : Integer;
  fText   : Pchar; { Paul - Array of Char; }
  Decode  : TZDecompressionStream;
begin
  {Read the keyword}
  fKeyword := fStream.Memory;

  {Get the size of the uncompressed text and resize the holder}
  DSize := fStream.Size - Length(fKeyword) - 2;
  GetMem(fText,DSize); { Paul - SetLength(fText, DSize); }

  {Create a especial stream to decompress}
  fStream.Position := Length(fKeyword) + 2;
  Decode := TZDecompressionStream.Create(fStream);
  Decode.Read(fText[0], DSize);
  case Index of
  0:
    Result := fKeyword;
  else
    Result := ftext; { Paul - pchar(@fText[0]); }
  end;
  {Free that stream}
  Decode.Free;

end;

{:Set the value of one of the properties}
procedure TChunkZTXT.SetValue(Index: Integer; Value: String);
var
  fKeyword, fText: pchar;
  Encode         : TZCompressionStream;
  Method         : Byte;
begin
  {Test which property to set}
  case Index of
  0: begin
      {Setting keyword}
      fKeyword := pchar(Value);
      fText := pchar(Text);
    end;
  else
    begin
      {Setting text}
      fText := pchar(Value);
      fKeyword := pchar(Keyword);
    end;
  end;

  {Clear the stream for rewriting}
  fStream.Clear;
  fStream.Position := 0;
  Method := 0;

  {Write data}
  fStream.Write(fKeyword[0], Length(fKeyword) + 1); {+1 to include null character}
  fStream.Write(Method, 1);

  Encode := TZCompressionStream.Create(fStream, zcDefault);
  Encode.Write(fText[0], Length(fText));
  Encode.Free;

end;

{:When the TEXT chunk is being created}
constructor TChunkTEXT.Create(AOwner: TChunkList);
begin
  inherited;
  fType := 'tEXt';
  {Set the stream size to 2 and set the two bytes as null}
  fStream.SetSize(2); { Paul - fStream.Size := 2; }
  pByteArray(fStream.Memory)^[0] := 0;
  pByteArray(fStream.Memory)^[1] := 0;
end;

{:Return one of the properties of the chunk TEXT}
function TChunkTEXT.GetValue(Index: Integer): String;
var
  fKeyword, fText: pChar;
begin
  fKeyword := fStream.Memory;
  fText := @pByteArray(fStream.Memory)[Length(fKeyword) + 1];

  {Test which property to return}
  case Index of
  0: Result := fKeyword;
  else
     Result := fText;
  end;
end;

{:Set the value of the TEXT chunk}
procedure TChunkTEXT.SetValue(Index: Integer; Value: String);
var
  fKeyword, fText: pchar;
begin
  {Test which property to set}
  case Index of
  0: begin
      {Setting keyword}
      fKeyword := pchar(Value);
      fText := pchar(Text);
    end;
  else
    begin
      {Setting text}
      fText := pchar(Value);
      fKeyword := pchar(Keyword);
    end;
  end;

  {Clear the stream for rewriting}
  fStream.Clear;
  fStream.Position := 0;

  {Write data}
  fStream.Write(fKeyword[0], Length(fKeyword) + 1); {+1 to include null character}
  fStream.Write(fText[0], Length(fText) + 1);
end;

{:When the object is being destroyed}
destructor TChunkPLTE.Destroy;
begin
  {If the main bitmap is using the palette make it don't use it anymore}
  if Owner.Owner.Palette = fPalette then
    Owner.Owner.Palette := 0;

  {Delete the palette from the memory}
  if fPalette <> 0 then       {LDB}
    DeleteObject(fPalette);

  inherited;
end;



{Returns the palette from the image}
function TChunkPLTE.GetPalette: HPalette;
var
  MaxPalette: TMaxLogPalette;
  i: Integer;
  GamaChunk : TChunkGAMA;
begin
  GamaChunk := Gama;

  {Delete the old palette from the memory}
  if fPalette <> 0 then             {LDB}
    DeleteObject(fPalette);

  {The palette stream must be divisible by 3}
  if fStream.Size MOD 3 <> 0 then
    CallError(PNG_ERROR_INVALID_PLTE);

  {Set the MaxPalette attributes}
  with MaxPalette do
  begin
    Fillchar(MaxPalette, sizeof(MaxPalette), 0);
    palVersion := $300;
    palNumEntries := fStream.Size DIV 3;

    {Get each value}
    FOR i := 0 to palNumEntries - 1 DO
    WITH palPalEntry[i] do
    BEGIN
      peRed := pByteArray(fStream.Memory)[(i * 3)];
      {Correct red using gamma}
      if Assigned(GamaChunk) then
        peRed := GamaChunk.GammaTable[peRed];

      peGreen := pByteArray(fStream.Memory)[(i * 3) + 1];
      {Correct green using gamma}
      if Assigned(GamaChunk) then
        peGreen := GamaChunk.GammaTable[peGreen];

      peBlue := pByteArray(fStream.Memory)[(i * 3) + 2];
      {Correct red using gamma}
      if Assigned(GamaChunk) then
        peBlue := GamaChunk.GammaTable[peBlue];

      peFlags := 0;
    END;
    IF (IHDR.BitDepth = 2) and (palNumEntries < 16) then
    begin
      {Note: This is really a crazy fix for supporting 2bit}
      {images}
      palNumEntries := 16;
      copymemory(@palpalentry[4], @palpalentry[0], 21);
      copymemory(@palpalentry[8], @palpalentry[0], 21);
      copymemory(@palpalentry[12], @palpalentry[0], 21);

    end;

  end;

  {Create the palette object}
  fPalette := CreatePalette(PLogPalette(@MaxPalette)^);

  {Returns the palette handle}
  Result := fPalette;
end;

{:When the chunk is being saved}
procedure TChunkPLTE.SaveToStream(Stream: TStream);
var
  PaletteSize: Word;
  LogPalette : TMaxLogPalette;
  I          : Integer;
  GamaChunk  : TChunkGama;
begin
  GamaChunk := Gama;

  {Free the stream for rewritting}
  fStream.Clear;

  {If the image does not contains palette, exit}
  if Owner.Owner.Palette = 0 then
    exit
  else
  begin
    {If it does, retrieve the palette}

    {First discover the palette size}
    GetObject(Bitmap.Palette, SizeOf(WORD), @PaletteSize);
    {Now get the entries}
    GetPaletteEntries(Bitmap.Palette, 0, PaletteSize,
      LogPalette.palpalentry);

    {Now write the entries to the stream}
    FOR I := 0 TO PaletteSize - 1 DO
    With LogPalette do
    begin
      {Test if uses gamma}
      if Assigned(GamaChunk) then
      begin
        fStream.Write(GamaChunk.InverseTable[palPalEntry[i].peRed], 1);
        fStream.Write(GamaChunk.InverseTable[palPalEntry[i].peGreen], 1);
        fStream.Write(GamaChunk.InverseTable[palPalEntry[i].peBlue], 1);
      end
      else
      begin
        fStream.Write(palPalEntry[i].peRed, 1);
        fStream.Write(palPalEntry[i].peGreen, 1);
        fStream.Write(palPalEntry[i].peBlue, 1);
      end;
    end;

  end;

  {Call default writting}
  inherited;
end;

{:Copy interlaced data into the current image}
procedure TChunkIDAT.DecodeInterlacedRow(ImageData: Pointer; Data: pByteArray;
  ColStart, ColIncrement, RowBytes, Pass: Integer; GamaChunk: TChunkGama);
var
  J, I: Integer;
begin
  I := ColStart;
  J := 0;

  {Test for color type}
  CASE IHDR.ColorType of
    Palette, Grayscale:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      2: {2 bits per pixel, not supported by TBitmap, so move to 4 bits}
        ConvertBits([@Data[0]], ImageData, IHDR.Width, PassMask[Pass], 2, 4);
      4: {4 bits per pixel}
        ConvertBits([@Data[0]], ImageData, IHDR.Width, PassMask[Pass], 4, 4);
      1: {1 bit per pixel}
        ConvertBits([@Data[0]], ImageData, IHDR.Width, PassMask[Pass], 1, 1);
      8: {1 byte per pixel}
      repeat
        pByteArray(ImageData)^[I] := Data^[J];
        inc(J);
        inc(I, ColIncrement);
      until J >= RowBytes;
     16:  {Grayscale interlaced images with 2 bytes per sample}
      repeat
        pByteArray(ImageData)^[I] := Data^[J];
        inc(J, 2);
        inc(I, ColIncrement);
      until J >= RowBytes;
    END;
    RGB:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8:   {1 byte per R, G, B}
      repeat
        with PRGBLine(ImageData)^[I] do
        begin
          rgbtRed := Data^[J];
          rgbtGreen := Data^[J + 1];
          rgbtBlue := Data^[J + 2];
          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
        inc(J, 3);
        inc(I, ColIncrement);
      until J >= RowBytes;
     16:   {2 bytes per R, G, B}
      repeat
        with PRGBLine(ImageData)^[I] do
        begin
          rgbtRed := Data^[J];
          rgbtGreen := Data^[J + 2];
          rgbtBlue := Data^[J + 4];
          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
        inc(J, 6);
        inc(I, ColIncrement);
      until J >= RowBytes;
    end;
    RGBALPHA:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8:   {1 byte per R, G, B, Alpha}
      repeat
        with PRGBLine(ImageData)^[I] do
        begin
          rgbtRed := Data^[J];
          rgbtGreen := Data^[J + 1];
          rgbtBlue := Data^[J + 2];
          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
        inc(J, 4);
        inc(I, ColIncrement);
      until J >= RowBytes;
     16:   {2 bytes per R, G, B, Alpha}
      repeat
        with PRGBLine(ImageData)^[I] do
        begin
          rgbtRed := Data^[J];
          rgbtGreen := Data^[J + 2];
          rgbtBlue := Data^[J + 4];
          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
        inc(J, 8);
        inc(I, ColIncrement);
      until J >= RowBytes;

    END;
    GRAYSCALEALPHA:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8:   {1 byte per gray and alpha}
      repeat
        pByteArray(ImageData)^[I] := Data^[J];
        inc(J, 2);
        inc(I, ColIncrement);
      until J >= RowBytes;
     16:   {2 bytes per gray and alpha}
      repeat
        pByteArray(ImageData)^[I] := Data^[J];
        inc(J, 4);
        inc(I, ColIncrement);
      until J >= RowBytes;
    END;

  end;
end;


{:Copy non interlaced data into the current image}
procedure TChunkIDAT.DecodeNonInterlacedRow(ImageData: Pointer; Data: pByteArray;
  RowBytes: Integer; GamaChunk: TChunkGama);
var
  Col: Integer;
begin

  {Test for color type}
  case IHDR.ColorType of
    Palette, Grayscale:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      1, 4, 8:  {Simple memory copy}
        CopyMemory(ImageData, Data, RowBytes);
      2: {Pixelformat pf2bits ? not supported (pf4bits being used) }
        ConvertBits([@Data[0]], ImageData, Bitmap.Width, $FF, 2, 4);
     16: {Grayscale with 2 pixels}
      FOR Col := 0 to Bitmap.Width - 1  DO
        pByteArray(ImageData)^[Col] := Data^[Col * 2];
    END;
    RGB:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8: {1 byte for each R, G AND B values}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        with PRGBLine(ImageData)^[Col] do
        begin
          rgbtRed := Data^[Col * 3];
          rgbtGreen := Data^[1 + Col * 3];
          rgbtBlue := Data^[2 + Col * 3];

          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
     16: {2 bytes for each R, G AND B values}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        with PRGBLine(ImageData)^[Col] do
        begin
          rgbtRed := Data^[Col * 6];
          rgbtGreen := Data^[2 + Col * 6];
          rgbtBlue := Data^[4 + Col * 6];

          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;

        end;
     end;
    RGBALPHA:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8: {1 byte for each R, G, B AND ALPHA values}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        with PRGBLine(ImageData)^[Col] do
        begin
          rgbtRed := Data^[Col * 4];
          rgbtGreen := Data^[1 + Col * 4];
          rgbtBlue := Data^[2 + Col * 4];

          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
     16: {2 bytes for each R, G AND B values and 1 for ALPHA}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        with PRGBLine(ImageData)^[Col] do
        begin
          rgbtRed := Data^[Col * 8];
          rgbtGreen := Data^[2 + Col * 8];
          rgbtBlue := Data^[4 + Col * 8];

          {Gamma correction}
          if Assigned(GamaChunk) then
          begin
            rgbtRed := GamaChunk.GammaTable[rgbtRed];
            rgbtGreen := GamaChunk.GammaTable[rgbtGreen];
            rgbtBlue := GamaChunk.GammaTable[rgbtBlue];
          end;
        end;
    end;
    GRAYSCALEALPHA:
    {Test for bit depth}
    CASE IHDR.BitDepth of
      8: {1 byte for grayscale and 1 for alpha}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        pByteArray(ImageData)^[Col] := Data^[Col * 2];
     16: {2 bytes for grayscale and 1 for alpha}
      FOR Col := 0 to (Bitmap.Width - 1)  DO
        pByteArray(ImageData)^[Col] := Data^[Col * 4];
    end;

  end;
end;

{:Decode the readed image to the bitmap}
procedure TChunkIDAT.DoAction;
const
  CHAR_BIT = 8;
var
  RowBuffer        : array[Boolean] of pbytearray;
  Row_Buffer_Width : Integer;
  OddLine          : Boolean;
  Offset           : Integer;
  UseProgress      : Boolean;
  j                : Integer;
  Pass             : Integer;
  Decode           : TZDecompressionStream;
  Row              : Integer;
  PixelsThisRow    : Integer;
  RowBytes         : Integer;
  GamaChunk        : TChunkGama;
begin
  GamaChunk := Gama;

  {Create the decompression object}
  Decode := TZDecompressionStream.Create(fStream);
  Decode.Position := 0;

  rowbytes := 0;

  {Filtering is done on corresponding items within a record. Determine}
  {the number of bytes between corresponding items.                   }
  OffSet := GetOffSet;

  {Define if uses OnProgress}
  UseProgress := Assigned(Bitmap.Onprogress);

  {Retrieve the number of bytes per line}
  row_buffer_width := GetBufferWidth;

  {Allocate memory for the row buffers and fill them with zeros}
  OddLine := TRUE;
  GetMem(RowBuffer[True], row_buffer_width + 1);
  GetMem(RowBuffer[False], row_buffer_width + 1);
  ZeroMemory(RowBuffer[False], row_buffer_width + 1);

  {Set the bitmap properties}
  with Bitmap do
  begin
    {Setup pixel formats and palette}
    SetupPixelFormat;

    {Set width and height}
    Width := IHDR.Width;
    Height := IHDR.Height;
  end;

  {Interlace decode}
  if IHDR.Interlaced = 1 then
  begin
    {Each of the interlacing passes}
    FOR Pass := 0 TO 6 DO
    begin
      {Number of pixels in this row}
      pixelsthisrow := (Bitmap.width - ColumnStart[Pass] +
        + ColumnIncrement[Pass] - 1) div ColumnIncrement[Pass] ;

      {Number of bytes}
      case (IHDR.ColorType) of
      Grayscale, Palette:
        rowbytes := (pixelsthisrow * IHDR.BitDepth + CHAR_BIT - 1) div CHAR_BIT ;
      RGB:
        rowbytes := pixelsthisrow * 3 * IHDR.BitDepth div CHAR_BIT ;
      RGBAlpha:
        rowbytes := pixelsthisrow * 4 * IHDR.BitDepth div CHAR_BIT ;
      GrayscaleAlpha:
        rowbytes := pixelsthisrow * 2 * IHDR.BitDepth div CHAR_BIT ;
      end;

      Row := RowStart[Pass];
      while Row < Bitmap.Height do
      begin
        {Read line from the stream}
        Decode.Read(rowBuffer[OddLine][0], rowbytes + 1);
        {Filter the row}
        FilterRow(RowBuffer[OddLine][0], @RowBuffer[OddLine][1],
          @RowBuffer[not OddLine][1], offset, rowbytes);

        {Translate data into the image}
        DecodeInterlacedRow(Bitmap.ScanLine[Row], @RowBuffer[OddLine][1],
          ColumnStart[Pass], ColumnIncrement[Pass], RowBytes, Pass, Gamachunk);

        {Jump to the next line}
        Inc(Row, RowIncrement[Pass]);
        {Change the line}
        OddLine := not OddLine;
      end;

      {Call progress event}
      If UseProgress then
        Bitmap.OnProgress(Bitmap, psRunning, MulDiv(100, Pass, 6),
          True, Rect(0, 0, Bitmap.Width, Bitmap.Height), 'Drawing...');

    end;
  end
  {Non interlace decode}
  else if IHDR.Interlaced = 0 then
  begin
    {Pass each row}
    for j := 0 to Bitmap.Height - 1 DO
    begin
      {Decompress}
      Decode.Read(RowBuffer[OddLine][0], row_buffer_width + 1);

      {Filter the current row}
      FilterRow(RowBuffer[OddLine][0], @RowBuffer[OddLine][1],
        @RowBuffer[not OddLine][1], OffSet, row_buffer_width);

      {Translate the data into the image}
      DecodeNonInterlacedRow(Bitmap.Scanline[j], @RowBuffer[OddLine][1],
        row_buffer_width, GamaChunk);

      {Change the line}
      OddLine := not OddLine;


      {Call progress event}
      If UseProgress then
          Bitmap.OnProgress(Bitmap, psRunning, MulDiv(j, 100, Bitmap.Height),
            True, Rect(0, j - 1, Bitmap.Width, j), 'Drawing...');
    end;
  end
  else
  {Unknown interlace method}
    CallError(PNG_ERROR_INVALID_INTERLACE);

  {Free memory for the row buffers}
  FreeMem(RowBuffer[True], row_buffer_width + 1);
  FreeMem(RowBuffer[False], row_buffer_width + 1);

  {Free the decompression object}
  Decode.Free;

  {$IFDEF _SHAREWARE} Shareware {$ENDIF};
end;

{:Returns the buffer width}
function TChunkIDAT.GetBufferWidth: Integer;
const
  CHAR_BIT = 8;
var
  RowBits         : Integer;
begin
  Result := 0;

  case IHDR.ColorType of
  Grayscale, Palette:
  begin
    rowbits := IHDR.Width * IHDR.BitDepth;
    Result := (rowbits + CHAR_BIT - 1) div CHAR_BIT;
  end;
  GrayscaleAlpha:
    Result := 2 * IHDR.width * IHDR.BitDepth div CHAR_BIT ;
  RGB:
    Result := IHDR.width * 3 * IHDR.BitDepth div CHAR_BIT ;
  RGBAlpha:
    Result := IHDR.width * 4 * IHDR.BitDepth div CHAR_BIT ;
  else
    {In case we have an undetermined color type}
    CallError(PNG_ERROR_INVALID_COLOR_TYPE);
  end;

end;

{:Returns the offset for filtering}
function TChunkIDAT.GetOffset: Integer;
const
  CHAR_BIT = 8;
begin
  case IHDR.ColorType of
    Grayscale, Palette:      result := 1;
    RGB:                     result := 3 * IHDR.BitDepth div CHAR_BIT ;
    GrayscaleAlpha:          result := 2 * IHDR.BitDepth div CHAR_BIT ;
    RGBAlpha:                result := 4 * IHDR.BitDepth div CHAR_BIT ;
    else
      result := 0;
  end;
end;

{:Filter the row for encoding}
function TChunkIDAT.EncodeFilterRow(row_buffer: pbytearray;
  Filter_buffers: TFilterRow; row_width, filter_width: Cardinal): Integer;
const
  FTest: Array[0..4] of TEncodeFilter = (efNone, efSub, efUp,
    efAverage, efPaeth);
var
  ii, run, jj: Cardinal;
  longestrun : Cardinal;
 last,
 above,
 lastabove   : byte;

begin

  // Filter for each type in the filter_mask.
  if efSub in Bitmap.Filter then
  begin

    for ii := 0 to row_width - 1 do
    begin
      if (ii >= filter_width) then
        last := row_buffer^[ii-filter_width]
      else
        last := 0 ;

      filter_buffers [FILTERSUB]^[ii] := row_buffer^[ii] - last ;
    end;
  end;

  if efUp in Bitmap.Filter then
    for ii := 0 to row_width - 1 do
      filter_buffers[FILTERUP]^[ii] := row_buffer^[ii] -
        filter_buffers[FILTERNONE]^[ii] ;

  if efAverage in Bitmap.Filter then
  begin
    for ii := 0 to row_width - 1 do
    begin
      if (ii >= filter_width) then
        last := row_buffer^[ii - filter_width]
      else
        last := 0 ;
      above := filter_buffers [FILTERNONE]^[ii] ;

      filter_buffers [FILTERAVERAGE]^[ii]
        := row_buffer^[ii] - (above + last) div 2 ;
    end;
  end;

  if efPaeth in Bitmap.Filter then
  begin
    for ii := 0 to row_width - 1 do
    begin
      if (ii >= filter_width) then
      begin
        last := row_buffer^[ii-filter_width] ;
        lastabove := filter_buffers [FILTERNONE]^[ii - filter_width] ;
      end
      else
      begin
        last := 0 ;
        lastabove := 0 ;
      end;

      above := filter_buffers [FILTERNONE]^[ii] ;
      filter_buffers [FILTERPAETH]^[ii]
        := row_buffer^[ii] - PaethPredictor (last, above, lastabove) ;
    end;
  end;


  // Filter None
  // THIS MUST BE THE LAST FILTER!!!!!!!!!! We save the value
  // here to be used in the next call with the filters that require data from the
  // previous row.
  for ii := 0 to row_width - 1 do
    filter_buffers[FILTERNONE]^[ii] := row_buffer^[ii] ;

  // If we only performed FilterNone then we do not need to proceed
  // any further.
  Result := FILTERNONE ;
  if Bitmap.Filter = [efNone] then
    exit;

  // Find the best filter. We do a simple test for the
  // longest runs of the same value.

  LongestRun := 0;
  for ii := 0 to FILTERBUFFERCOUNT - 1 DO
  begin
    if FTest[ii] in Bitmap.Filter then
    begin
      run := 0;
      for jj := 4 to row_width - 1 do
      begin
        if (filter_buffers[ii]^[jj] = filter_buffers [ii]^[jj-1]) and
            (filter_buffers[ii]^[jj] = filter_buffers [ii]^[jj-2]) and
            (filter_buffers[ii]^[jj] = filter_buffers [ii]^[jj-3]) and
            (filter_buffers[ii]^[jj] = filter_buffers [ii]^[jj-4]) then
          inc(Run);
      end;

      if (run > longestrun) then
      begin
        result := ii ;
        longestrun := run ;
      end;
    end;
  end;

end;

{:Encode the actual image from the bitmap}
procedure TChunkIDAT.EncodeImage;
var
  Encode          : TZCompressionStream;
  j, offset, i    : Integer;
  row_buffer_width: Integer;
  filter_buffers  : TFilterRow;
  Filter          : byte;
  row_buffer      : pByteArray;
  Line            : Pointer;
  GamaChunk       : TChunkGama;

  function AdjustValue(Value: Byte): Byte;
  begin
    if Assigned(GamaChunk) then
      Result := GamaChunk.InverseTable[Value]
    else
      Result := Value;
  end;
begin
  GamaChunk := Gama;

  {Clear the previous IDAT memory since we will use bitmap}
  {data to write all over again}
  fStream.Clear;

  {Create a stream to handle the compression}
  Encode := TZCompressionStream.Create(fStream, zcDefault);

  {Number of bytes in each row}
  row_buffer_width := GetBufferWidth;
  offset := GetOffset;

  {Allocate memory for filtering}
  GetMem(row_buffer, row_buffer_width);
  GetMem(filter_buffers[FILTERNONE], row_buffer_width);
  if efSub in Bitmap.Filter then
    GetMem(filter_buffers[FILTERSUB], row_buffer_width);
  if efUp in Bitmap.Filter then
    GetMem(filter_buffers[FILTERUP], row_buffer_width);
  if efAverage in Bitmap.Filter then
    GetMem(filter_buffers[FILTERAVERAGE], row_buffer_width);
  if efPaeth in Bitmap.Filter then
    GetMem(filter_buffers[FILTERPAETH], row_buffer_width);
  {Fill the filternone with zeros}
  ZeroMemory(@filter_buffers[FILTERNONE][0], row_buffer_width);

  Bitmap.Interlacing := FALSE;
  
  {Testing encoding method}
  if Bitmap.Interlacing then
  {No interlacing}
  begin
  end
  else
  {Interlacing}
  begin
    {Pass each row}
    for j := 0 to Bitmap.Height - 1 do
    begin

      {Write depending on the pixel format}
      case Bitmap.PixelFormat of
      pf1bit, pf4bit, pf8bit:
        filter := EncodeFilterRow(Bitmap.ScanLine[j], filter_buffers,
          row_buffer_width, offset);
      else
      begin
        {Copy pointer to the line bytes}
        Line := Bitmap.ScanLine[j];

        {Test the pixel format}
        case Bitmap.PixelFormat of
          {3 bytes, just swap}
          pf24bit:
          FOR i := 0 to Bitmap.Width - 1 do
          begin
            Row_Buffer^[i * 3] := AdjustValue(pRGBLine(Line)^[i].rgbtRed);
            Row_Buffer^[1 + (i * 3)] := AdjustValue(pRGBLine(Line)^[i].rgbtGreen);
            Row_Buffer^[2 + (i * 3)] := AdjustValue(pRGBLine(Line)^[i].rgbtBlue);
          end;
          {4 bytes, swap and ignore last byte unused}
          pf32bit:
          FOR i := 0 to Bitmap.Width - 1 do
          begin
            Row_Buffer^[i * 4] := AdjustValue(pRGBALine(Line)^[i].rgbRed);
            Row_Buffer^[1 + (i * 4)] := AdjustValue(pRGBALine(Line)^[i].rgbGreen);
            Row_Buffer^[2 + (i * 4)] := AdjustValue(pRGBALine(Line)^[i].rgbBlue);
          end;
        end;

        {Filter the row}
        filter := EncodeFilterRow(@Row_Buffer[0], filter_buffers,
          row_buffer_width, offset);
      end;
      end;

      (*Write to stream*)
      Encode.Write(Filter, 1);
      Encode.Write(filter_buffers[Filter]^[0], row_buffer_width);
    end;
  end;

  {Free the compression stream}
  Encode.Free;
  {Free memory from the filters}
  FreeMem(row_buffer, row_buffer_width);
  FreeMem(filter_buffers[FILTERNONE], row_buffer_width);
  if efSub in Bitmap.Filter then
    FreeMem(filter_buffers[FILTERSUB], row_buffer_width);
  if efUp in Bitmap.Filter then
    FreeMem(filter_buffers[FILTERUP], row_buffer_width);
  if efAverage in Bitmap.Filter then
    FreeMem(filter_buffers[FILTERAVERAGE], row_buffer_width);
  if efPaeth in Bitmap.Filter then
    FreeMem(filter_buffers[FILTERPAETH], row_buffer_width);

  {$IFDEF _SHAREWARE} Shareware {$ENDIF};
end;

{:Adjust image pixel format}
procedure TChunkIDAT.SetupPixelFormat;
var
  PlteIndex, i     : Integer;
  GrayscalePal     : TMaxLogPalette;
  GAMACHUNK        : TChunkGAMA;
begin
  (*{In case we need an alpha channel bitmap}
  if (IHDR.ColorType = GrayscaleALpha) or
    (IHDR.ColorType = RGBAlpha) then
  begin
    {Free the old mask}
    FreeAndNil(Bitmap.fMask);

    {Create a new bitmap}
    Bitmap.fMask := TBitmap.Create;

    {Set its properties}
    with Bitmap.fMask do
    begin
      Width := IHDR.Width;
      Height := IHDR.Height;
      PixelFormat := pf8bit;
    end;
  end; *)
  {Retrieve the chunk GAMA}
  GamaChunk := Gama;

  {Set the pixel formats}
  CASE IHDR.ColorType of
    GrayScale, Palette, GrayScaleAlpha:
    CASE IHDR.BitDepth of
      1: Bitmap.PixelFormat := pf1bit;     {1 bit, 2 colors: 2^1}
      2: Bitmap.PixelFormat := pf4bit;
      4: Bitmap.PixelFormat := pf4bit;
      8: Bitmap.PixelFormat := pf8bit;     {1 byte in each pixel, 256 colors}
     16: Bitmap.PixelFormat := pf8bit;     {2 bytes per sample}
    END;
    RGB, RGBALPHA:
    CASE IHDR.BitDepth of
      8: Bitmap.PixelFormat := pf24bit;    {R, G, B values for each pixel}
     16: Bitmap.PixelFormat := pf24bit;    {Increased range of values for RGB}
    END;
  END;

  {Create the palettes for the file formats}
  CASE IHDR.ColorType of
  Grayscale, GrayscaleAlpha:
 {Create grayscale palette}
  with GrayscalePal do
  begin
    palVersion := $300;

    {Set the number of colors in palette}
    {Since the max is 256 colors 16bit per sample pixels will be}
    {averanged to 8}
    if IHDR.BitDepth = 16 then
      palNumEntries := 256
    else
      palNumEntries := (1 shl IHDR.BitDepth);

    {Set the palette colors}
    FOR i := 0 to palNumEntries - 1 DO
      WITH palPalEntry[i] do
      begin
        {Average the colors}
        {When i is 0, the color is black}
        {When i is palNumEntries, the color is white}
        peRed := MulDiv(i, 255, palNumEntries - 1);
        {Correct using gamma}
        if Assigned(GamaChunk) then
          peRed := GamaChunk.GammaTable[peRed];
        peGreen := peRed;
        peBlue := peGreen;
        peFlags := PC_NOCOLLAPSE;
      end;

    IF (IHDR.BitDepth = 2) and (palNumEntries < 16) then
    begin
      {Note: This is really a crazy totally nonsence fix for supporting 2bit}
      palNumEntries := 16;
      copymemory(@palpalentry[4], @palpalentry[0], 21);
      copymemory(@palpalentry[8], @palpalentry[0], 21);
      copymemory(@palpalentry[12], @palpalentry[0], 21);
    end;

    {Apply the bitmap palette}
    Bitmap.Palette := CreatePalette(PLogPalette(@GrayscalePal)^);
  end;
  Palette:
  BEGIN
    {Test if there is PLTE chunk, if so apply the palette}
    PlteIndex := Owner.IndexOfClass(TChunkPLTE); { Paul }
    if PlteIndex <> -1 then
      Bitmap.Palette := TChunkPLTE(Owner[PlteIndex]).Palette
    ELSE
      CallError(PNG_ERROR_NO_PALETTE);
  END;
  END;
end;

{:Filters the row using definied types}
procedure TChunkIDAT.FilterRow(Filter: Byte; CurrentRow, LastRow: pByteArray;
  offset, row_buffer_width: Integer);
var
  Col: Integer;  {Current Column}
  Left, Above, AboveLeft: Integer;
  vv, pp: Integer;
begin

  // Filter the row based upon the filter type.
  case filter of
    {No filtering, do nothing}
    FILTERNONE: begin end;
    {Sub filter}
    FILTERSUB:
    {The value is the difference from the value to the left}
    for col := offset to row_buffer_width - 1 do
      CurrentRow[col] := (CurrentRow[col] + CurrentRow[col-offset]) AND $FF;
    FILTERUP:
    {The value is the difference from the value in the previous row.}
    for col := 0 to row_buffer_width - 1 do
      CurrentRow[col] := (CurrentRow[col] + LastRow[col]) AND $FF ;
    FILTERAVERAGE:
    for col := 0 to row_buffer_width - 1 do
    begin
      above := LastRow[col];

      if (col < offset) then
        left := 0
      else
        left := CurrentRow[col-offset] ;

      CurrentRow[col] := (CurrentRow[col] + (left + above) div 2) AND $FF ;
    end;
    FILTERPAETH:
    for col := 0 to row_buffer_width - 1 do
    begin
      above := LastRow[col] ;

      if (col < offset) then
      begin
        left := 0 ;
        aboveleft := 0 ;
      end
      else
      begin
        left := CurrentRow[col-offset] ;
        aboveleft := LastRow[col-offset] ;
      end;

      vv := CurrentRow[col] ;
      pp := PaethPredictor(left, above, aboveleft) ;
      CurrentRow[col] := (pp + vv) AND $FF ;
    end;
    else
      {In case the filter is not reconized}
      CallError(PNG_ERROR_INVALID_FILTER_TYPE);
  end; {Case}

end;

{:When the chunk is going to be saved to a stream}
procedure TChunkIDAT.SaveToStream(Stream: TStream);
begin
  {Set to encode the image to the data}
  EncodeImage;
  {Then write}
  inherited;
end;

{Assign data from one gama chunk}
procedure TChunkGAMA.Assign(Source: TChunk);
begin
  inherited; // fix 1
  GammaTable := TChunkGAMA(Source).GammaTable;
  InverseTable := TChunkGAMA(Source).InverseTable;
end;

{When the object is being created}
constructor TChunkGAMA.Create(AOwner: TChunkList);
begin
  inherited;
  {Set the size of the stream and initial value}
  fStream.SetSize(4);
  Value := 1;
end;

{:Creates a gamma table for using}
procedure TChunkGAMA.DoAction;
var
  I    : Integer;
begin
  {Create gamma table and inverse gamma table (for saving)}
  FOR I := 0 TO 255 DO
  begin
    GammaTable[I] := Round(Power((I / 255), 1 / (Value / 100000 * 2.2)) * 255);
    InverseTable[Round(Power((I / 255), 1 / (Value / 100000 * 2.2)) * 255)] := I;
  end;
end;


{Returns the Gama value}
function TChunkGAMA.GetValue: Cardinal;
begin
  Result := SwapLong(pCardinal(fStream.Memory)^);
end;

{Sets the Gama value}
procedure TChunkGAMA.SetValue(Value: Cardinal);
begin
  pCardinal(fStream.Memory)^ := SwapLong(Value);
end;

{:When the chunk is being saved}
procedure TChunkIHDR.SaveToStream(Stream: TStream);
begin
  {Set the IHDR chunk properties}
    Compression := 0; {The only compression method avaliable}
    Filter := 0;      {The only filter scheme avaliable}

    if Owner.Owner.Interlacing then  {Interlace method}
      Interlaced := 1           {ADAM 7}
    else
      Interlaced := 0;          {NONE}

    Width := Owner.Owner.Width;
    Height := Owner.Owner.Height;

    {Color type}
    case Owner.Owner.PixelFormat of
      pf1bit, pf4bit, pf8bit:
      begin
        {Palette}
        ColorType := PALETTE;
        {Bit depth}
        case Owner.Owner.PixelFormat of
          pf1bit: BitDepth := 1;
          pf4bit: BitDepth := 4;
          pf8bit: BitDepth := 8;
        end;

      end;
      else
      begin
        {R, G, B}
        Owner.Owner.PixelFormat := pf24bit;
        ColorType := RGB;
        BitDepth := 8;
      end;
    end;

  inherited;
end;

{Get values for the other properties}
function TChunkIHDR.GetValue(Index: Integer): Byte;
begin
  case Index of
    0: {Bit depth}   Result := pIHDRChunk(fStream.Memory)^.BitDepth;
    1: {Color type}  Result := pIHDRChunk(fStream.Memory)^.ColorType;
    2: {Compression} Result := pIHDRChunk(fStream.Memory)^.Compression;
    3: {Filter}      Result := pIHDRChunk(fStream.Memory)^.Filter;
    4: {Interlaced}  Result := pIHDRChunk(fStream.Memory)^.Interlaced;
    else {Avoid warning}
      Result := 0;
  end;

end;

{Set value for the other properties}
procedure TChunkIHDR.SetValue(Index: Integer; Value: Byte);
begin
  case Index of
    0: {Bit depth}   pIHDRChunk(fStream.Memory)^.BitDepth := Value;
    1: {Color type}  pIHDRChunk(fStream.Memory)^.ColorType := Value;
    2: {Compression} pIHDRChunk(fStream.Memory)^.Compression := Value;
    3: {Filter}      pIHDRChunk(fStream.Memory)^.Filter := Value;
    4: {Interlaced}  pIHDRChunk(fStream.Memory)^.Interlaced := Value;
  end;
end;

{Returns the image height}
function TChunkIHDR.GetHeight: Cardinal;
begin
  Result := SwapLong(pIHDRChunk(fStream.Memory)^.Height);
end;

{Returns the image width}
function TChunkIHDR.GetWidth: Cardinal;
begin
  Result := SwapLong(pIHDRChunk(fStream.Memory)^.Width);
end;

{Sets the image height}
procedure TChunkIHDR.SetHeight(Value: Cardinal);
begin
  pIHDRChunk(fStream.Memory)^.Height := SwapLong(Value);

  {Changes the image size}
  if Owner.Owner.Height <> Int(Value) then
    Owner.Owner.Height := Value;
end;

{Sets the image width}
procedure TChunkIHDR.SetWidth(Value: Cardinal);
begin
  pIHDRChunk(fStream.Memory)^.Width := SwapLong(Value);

  {Changes the image size}
  if Owner.Owner.Width <> Int(Value) then
    Owner.Owner.Width := Value;
end;

{:When the object is being created}
constructor TChunkIHDR.Create(AOwner: TChunkList);
begin
  inherited;
  {Resize the IHDR chunk}
  fStream.SetSize(13);
end;

{:Returns the index of the chunk class}
function TChunkClasses.IndexOfClass(Item: TChunkClass): Integer; { Paul }
var
  i: Integer;
begin
  {If none found, return -1}
  Result := -1;

  {Test each class}
  if Count > 0 then
    FOR i := 0 to Count - 1 DO
      if Self.Item[I].ChunkClass = Item then
      begin
        Result := i;
        break;
      end;
end;

{:Returns the index of the given chunk type}
function TChunkClasses.IndexOfType(Item: TChunkType): Integer; { Paul }
var
  i: Integer;
begin
  {If none found, return -1}
  Result := -1;

  {Test each class}
  if Count > 0 then
    FOR i := 0 to Count - 1 DO
      if Self.Item[I].ChunkType = Item then
      begin
        Result := i;
        break;
      end;
end;

{:When the object is being destroyed}
destructor TChunkClasses.Destroy;
var
  i: Integer;
begin
  {Free each registered chunk class}
  if Count > 0 then
  FOR i := 0 TO Count - 1 DO
    Dispose(pChunkClassInfo(fList[i]));

  {Free the list}
  if Assigned(fList) then fList.free;

  inherited;
end;

{:Returns an item from the list}
function TChunkClasses.GetItem(Index: Integer): TChunkClassInfo;
begin
  {Test if the index is valid}
  if (Index < 0) or (Index > Count - 1) then
    CallError(PNG_ERROR_INVALID_CHUNK_CLASS_INDEX);

    Result := pChunkClassInfo(fList[Index])^;

end;


{Returns the number of items in the list}
function TChunkClasses.GetCount: Integer;
begin
  {If the list object exists, then return the count from it}
  {otherwise returns 0                                     }
  if Assigned(fList) then
    Result := fList.Count
  else
    Result := 0;
end;

{:Add a new chunk class to the list of classes}
procedure TChunkClasses.Add(ChunkType: TChunkType;
  ChunkClass: TChunkClass);
var
  NewItem: pChunkClassInfo;
begin
  {Create the list if it does not exists}
  if not Assigned(fList) then
    fList := TList.Create;

  {Allocate memory for the new item}
  New(NewItem);

  {Set the new item properties}
  NewItem^.ChunkType := ChunkType;
  NewItem^.ChunkClass := ChunkClass;

  {Add to the list}
  fList.Add(NewItem);

end;

{Do the action when the chunk is read}
procedure TChunk.DoAction;
begin
  inherited;
end;

{Returns a pointer to the png image owner}
function TChunk.GetBitmap: TPNGImage;
begin
  Result := Owner.Owner;
end;

{Returns a pointer to the GAMA}
function TChunk.GetGAMA: TChunkGAMA;
var
  Pos: Integer;
begin
  {Position of the chunk}
  Pos := Owner.IndexOfClass(TChunkGAMA); { Paul }

  {Returns nil if the chunk does not exists}
  if Pos = -1 then
    Result := nil
  else
    Result := TChunkGAMA(Owner[Pos]);

end;

{Returns a pointer to the IHDR}
function TChunk.GetIHDR: TChunkIHDR;
begin
  Result := TChunkIHDR(Owner[0]);
end;

{:Assign from another chunk}
procedure TChunk.Assign(Source: TChunk);
begin
  {Clear the current stream}
  fStream.Clear;
  {Copy data from the other stream}
  fStream.CopyFrom(Source.fStream, 0);

  {Copy the chunk name}
  fType := Source.fType;
end;

{:Returns the chunk size}
function TChunk.GetSize: Integer;
begin
  Result := fStream.Size;
end;

{:Saves the chunk data to the stream}
procedure TChunk.SaveToStream(Stream: TStream);
var
  ChunkLen: Cardinal;
  ChunkCRC: Cardinal;
begin
  {The chunk is not safe-to-copy}
  (*if ChunkType[3] = LowerCase(ChunkType[3]) then *)
  if fType[3] = LowerCase(fType[3]) then  //LDB  C++Builder fix
    exit;

  {First the chunk length}
  ChunkLen := SwapLong(fStream.Size);
  Stream.Write(ChunkLen, 4);

  {Now write the chunk type}
  Stream.Write(fType, 4);

  {Write the chunk data}
  Stream.CopyFrom(fStream, 0);

  {Calculate and write the CRC}
  ChunkCRC := SwapLong(CRC(fType, fStream.Memory, fStream.Size));
  Stream.Write(ChunkCRC, 4);
end;

{Retrieve the chunk index inside the list}
function TChunk.GetIndex: Integer;
begin
  Result := Owner.IndexOfChunk(Self); { Paul }
end;

{:Called when the object is being created}
constructor TChunk.Create(AOwner: TChunkList);
var
  ClassPos: Integer;
begin
  {Create the stream containg the memory data}
  fStream := TMemoryStream.Create;
  fList := AOwner;

  {Default class name}
  ClassPos := ChunkClasses.IndexOfClass(TChunkClass(ClassType)); { Paul }
  if ClassPos <> -1 then
    fType := ChunkClasses[ClassPos].ChunkType;
end;

{:Called when the object is being destroyed}
destructor TChunk.Destroy;
begin
  {Free the stream containing the memory data}
  fStream.Free;

  inherited;
end;

{:Move one chunk position in the list}
procedure TChunkList.Move(Index1, Index2: Integer);
begin
  {Test for index}
  if (Index1 < 0) or (Index1 >= Count) then
    CallError(PNG_ERROR_INVALID_CHUNK_INDEX);

  FList.Move(Index1, Index2);
end;

{Returns the number of items in the list (Used with Count property)}
function TChunkList.GetCount: Integer;
begin
  Result := fList.Count;
end;

{Returns an item from the list (Used with Item property)}
function TChunkList.GetItem(Index: Integer): TChunk;
begin
  {Test if the chunk index is valid}
  if (Index < 0) or (Index > Count - 1) then
    CallError(PNG_ERROR_INVALID_CHUNK_INDEX);

  {If so, return the item}
  Result := fList[Index];
end;

{:Removes a chunk}
procedure TChunkList.Remove(Item: TChunk);
begin
  {Makes sure that the list contains the chunk}
  if Item.Owner <> Self then
    CallError(CHUNK_NOT_CHILD);

  {Delete the chunk}
  FList.Delete(Item.Index);
  Item.Free;

end;

{:Add a chunk to the list when the chunk object ALREADY EXISTS}
function TChunkList.AddItem(Item: TChunk): TChunk; { Paul }
begin
  {Add the item to the list}
  fList.Add(Item);
  Result := Item;
end;


{:Returns the index of the first chunk of the type in the parameter}
function TChunkList.IndexOfClass(ChunkClass: TChunkClass): Integer; { Paul }
var
  I: Integer;
begin
  {Returns -1 if none found}
  Result := -1;

  {If there are items in the list, test each item}
  if Count > 0 then
    FOR i := 0 TO Count - 1 DO
      if Item[I].ClassType = ChunkClass then
      begin
        result := i;
        break;
      end;
end;

{:Returns the position of a chunk inside the list}
function TChunkList.IndexOfChunk(Chunk: TChunk): Integer; { Paul }
begin
  Result := fList.IndexOf(Chunk);
end;

{:Add a chunk to the list when the chunk object DOES NOT EXISTS
  but it already knows which chunk class to create}
function TChunkList.AddClass(ChunkClass: TChunkClass): TChunk; { Paul }
begin
  Result := AddItem(ChunkClass.Create(Self));
end;

{:Add a chunk to the list when the chunk data needs to be readed
 from a stream.                                                }
function TChunkList.AddStream(Stream: TStream): TChunk; { Paul }
var
  CLength: Cardinal;
  CType  : TChunkType;
  CCRC   : Cardinal;
  i, p   : Integer;
begin
  {First read the chunk length}
  Stream.Read(CLength, 4);
  CLength := SwapLong(CLength);
  {Now read the chunk type}
  Stream.Read(CType, 4);

  {Look for chunk classes supporting the given chunk type}
  i := ChunkClasses.IndexOfType(CType); { Paul }

  {Test if the chunk is critical but unknown}
  if ((Byte(CType[0]) AND $20) = 0) and (i = -1) then
    CallError(PNG_ERROR_UNKOWN_CRITICAL_CHUNK);

  {If the chunk type exists in the list, then create an object  }
  {using the class found, otherwise use the generic TChunk class}
  if i <> - 1 then
    Result := ChunkClasses[I].ChunkClass.Create(Self)
  else
    Result := TChunk.Create(Self);

  {Copy the chunk type}
  Result.fType := CType;

  {Read the data if the chunk contains data}
  if CLength > 0 then
    Result.fStream.CopyFrom(Stream, CLength);

  {Read the CRC for checking}
  Stream.Read(CCRC, 4);
  CCRC := SwapLong(CCRC);

  {Test if the CRC is valid}
  if CRC(CType, Result.fStream.Memory, CLength) <> CCRC then
    CallError(PNG_ERROR_CHUNK_INVALID_CRC);

  {If there are already IDAT chunks, then mix the actual IDAT}
  {being readed with the previous IDAT}
  if (Result is TChunkIDAT) then
    p := IndexOfClass(TChunkIDAT) { Paul }
  else
    p := -1;

  if (Result is TChunkIDAT) and (p <> -1) then
  begin
    {Copy data to the old stream}
    Item[p].fStream.CopyFrom(Result.fStream, 0);

    {Free the actual IDAT stream and returns the last}
    Result.Free;
    Result := Item[p];
  end
  else {Add the item to the list}
    Result := AddItem(Result); { Paul }
end;

{:Clear all the chunks in the list}
procedure TChunkList.Clear;
var
  i: Integer;
begin
  {If there are items in the list, delete each one}
  if Count > 0 then
    FOR i := Count - 1 DOWNTO 0 DO
    BEGIN
      {Free the chunk and delete from the list}
      Item[i].Free;
      FList.Delete(I);
    END;

end;

{:Called when the object is being created}
constructor TChunkList.Create(AOwner: TPNGImage);
begin
  {Copy the TPNGImage owner pointer}
  fImage := AOwner;

  {Create the TList}
  fList := TList.Create;
end;

{:Called when the object is being destroyed}
destructor TChunkList.Destroy;
begin
  {Clear and free the TList}
  Clear;
  fList.Free;

  inherited;
end;


{:Special override for assigning other TPNGImages}
procedure TPNGImage.Assign(Source: TPersistent);
var
  SourcePNG: TPNGImage;
  i, j     : Integer;
begin
  {If the source is also a TPNGImage, copy the chunks}
  if Source is TPNGImage then
  begin
    SourcePNG := TPNGImage(Source);
    {Clear current chunks}
    Chunks.Clear;

    {Copy the chunks}
    if SourcePNG.Chunks.Count > 0 then
    FOR i := 0 TO SourcePNG.Chunks.Count - 1 DO
    begin
      j := Chunkclasses.IndexOfType(SourcePNG.Chunks[i].fType); { Paul }
      {If the class is a know class, create it using that class}
      {otherwise with the default TChunk class}
      if j <> -1 then
        Chunks.AddItem(Chunkclasses[j].ChunkClass.Create(Chunks)).Assign(SourcePNG.Chunks[i]) { Paul }
      else
        Chunks.AddItem(TChunk.Create(Chunks)).Assign(SourcePNG.Chunks[i]); { Paul }
    end;

    {Copy other info}
    Filter := SourcePNG.fEncodeFilter;
    Interlacing := SourcePNG.fInterlacing;
  end;

  inherited;

end;

{:Called when the object is being created}
constructor TPNGImage.Create;
begin
  inherited;
  fMask := nil;

  {Create the list of chunks object}
  fChunkList := TChunkList.Create(Self);
  fInterlacing := FALSE;

  Filter := [efNone, efSub, efAverage, efPaeth];

  {Create the standard chunks}
  Clear;
end;

{:Called when the object is being destroyed}
destructor TPNGImage.Destroy;
begin
  {Free the mask if assigned}
  if Assigned(fMask) then
    fMask.Free;

  {Destroy the list of chunks object}
  fChunkList.Free;
  inherited;
end;

{Set the filters that are going to be used when encoding}
procedure TPNGImage.SetFilter(Value: TEncodeFilterSet);
begin
  {efNone is the only value that the set must have}
  if not (efNone in Value) then
    Include(Value, efNone);

  fEncodeFilter := Value;
end;

{:Clears the current image}
procedure TPNGImage.Clear;
begin
  {Clear the current chunks}
  Chunks.Clear;

  with TChunkIHDR(Chunks.AddClass(TChunkIHDR)) do { Paul }
  begin
    Width := 0;
    Height := 0;
    BitDepth := 2;
    ColorType := 3;
    Compression := 0;
    Filter := 0;
    Interlaced := 0;
  end;

  {Clears the palette}
  Palette := 0;

  {Add IDAT chunk}
  Chunks.AddClass(TChunkIDAT); { Paul }

  {Add IEND chunk}
  Chunks.AddClass(TChunkIEND); { Paul }
end;

{:Saves the current PNG image to the stream}
procedure TPNGImage.SaveToStream(Stream: TStream);
var
  i: Integer;
begin
  {Do the actual writting}
  with Stream do
  begin
    {Write the valid header}
    Write(PNGHeader, 8);

    {If there are no chunks, then create the standard ones}
    if Chunks.Count = 0 then
      Clear;

    {Ensure that there is a IHDR chunk}
    if (Chunks.Count = 0) or (not (Chunks[0] is TChunkIHDR)) then
      Chunks.Move(Chunks.AddClass(TChunkIHDR).Index, 0); { Paul }

    {PLTE chunk needed}
    if ((PixelFormat = pf1bit) or (PixelFormat = pf4bit) or
      (PixelFormat = pf8bit)) and (Chunks.IndexOfClass(TChunkPLTE) = -1) then { Paul }
      Chunks.Move(Chunks.AddClass(TChunkPLTE).Index, 1); { Paul }

    {If the image needs TRNS chunk}
    if Transparent then
      Chunks.Move(Chunks.AddClass(TChunkTRNS).Index, 1) { Paul }
    {If transparency is not being used, delete the transparency chunk(s)}
    else if Chunks.IndexOfClass(TChunkTRNS) <> -1 then { Paul }
      repeat
        Chunks.Remove(Chunks[Chunks.IndexOfClass(TChunkTRNS)]); { Paul }
      until Chunks.IndexOfClass(TChunkTRNS) = -1; { Paul }

    {Make sures that there is a IEND chunk}
    if Chunks.IndexOfClass(TChunkIEND) = -1 then { Paul }
      Chunks.AddClass(TChunkIEND); { Paul }

    {Make sures that there is a IDAT chunk}
    if Chunks.IndexOfClass(TChunkIDAT) = -1 then { Paul }
      Chunks.Move(Chunks.AddClass(TChunkIDAT).Index, 1); { Paul }

    {Write each chunk}
    FOR i := 0 to Chunks.Count -1 DO
      Chunks[i].SaveToStream(Stream);

  end;
end;

{:Loads a PNG image from the stream}
procedure TPNGImage.LoadFromStream(Stream: TStream);
var
  ReadHeader: TPNGHeader;
  i         : Integer;
begin

  {Clear the current chunks}
  Clear;
  Chunks.Clear;

  {Do the actual reading}
  with Stream do
  begin
    {Read the PNG file header for checking}
    Read(ReadHeader, 8);
    if not CompareMem(@ReadHeader, @PNGHeader, 8) then
      CallError(PNG_ERROR_INVALID_HEADER);

    {Read the chunks}
    while (not (Chunks.AddStream(Stream) is TChunkIEND)) and { Paul }
      not (Stream.Position = Stream.Size) do
    begin end;

    {Test if IHDR is the first chunk}
    if (Chunks.Count = 0) or not (Chunks[0] is TChunkIHDR) then
      CallError(PNG_ERROR_IHDR_NOT_FIRST);

    {Test if there is IDAT chunk, if so, decode it}
    if Chunks.IndexOfClass(TChunkIDAT) = -1 then { Paul }
      CallError(PNG_ERROR_NO_IDAT);

    {Execute each chunks action}
      FOR i := 0 to Chunks.Count - 1 DO
        Chunks[i].DoAction;
        
    {Test if there is tRNS chunk, if so, apply the transparency}
    if Chunks.IndexOfClass(TChunkTRNS) <> -1 then { Paul }
    case TChunkIHDR(Chunks[0]).ColorType of
      RGB:
      begin
        TransparentColor :=
          TChunkTRNS(Chunks[Chunks.IndexOfClass(TChunkTRNS)]).GetRGBColor; { Paul }
        Transparent := TRUE;
      end;
    end;

 end; {with}

end;

{:Register a new chunk class}
class procedure TPNGImage.RegisterChunkClass(ChunkType: TChunkType;
  ChunkClass: TChunkClass);
begin
  RegisterNewChunkClass(ChunkType, ChunkClass);
end;

procedure ConvertBits(Source: array of Pointer; Target: Pointer;
  Count: Cardinal; Mask: Byte; FSourceBPS, FTargetBPS: Byte);
var
  SourceRun, TargetRun: PByte;
  Value, BitRun,TargetMask, SourceMask,  SourceShift, TargetShift, MaxInSample,
  MaxOutSample,  SourceBPS, TargetBPS: Byte;
  Done: Cardinal;
begin
  SourceRun := Source[0]; TargetRun := Target;
    BitRun := $80; SourceBPS := FSourceBPS; TargetBPS := FTargetBPS;
    SourceMask := Byte(not ((1 shl (8 - SourceBPS)) - 1));
    MaxInSample := (1 shl SourceBPS) - 1;
    TargetMask := (1 shl (8 - TargetBPS)) - 1;
    MaxOutSample := (1 shl TargetBPS) - 1;
    SourceShift := 8;  TargetShift := 8 - TargetBPS;   Done := 0;
    while Done < Count do
    begin
      if Boolean(Mask and BitRun) then
      begin
        Dec(SourceShift, SourceBPS);
        Value := (SourceRun^ and SourceMask) shr SourceShift;
        Value := MulDiv(Value, MaxOutSample, MaxInSample);
        TargetRun^ := (TargetRun^ and TargetMask) or (Value shl TargetShift);
        if SourceShift = 0 then
        begin
          SourceShift := 8;
          Inc(SourceRun);
        end;
        asm
          MOV CL, [SourceBPS]
          ROR BYTE PTR [SourceMask], CL
        end;
      end;
      asm
        ROR BYTE PTR [BitRun], 1
        MOV CL, [TargetBPS]
        ROR BYTE PTR [TargetMask], CL
      end;
      if TargetShift = 0 then
        TargetShift := 8 - TargetBPS
      else
        Dec(TargetShift, TargetBPS);
      Inc(Done);
      if (Done mod (8 div TargetBPS)) = 0 then Inc(TargetRun);
    end;
end;

var
(* Table of CRCs of all 8-bit messages. *)
crc_table: array[0..255] of Cardinal;
(* Flag: has the table been computed? Initially false. *)
crc_table_computed: Integer = 0;

(*: Make the table for a fast CRC. *)
procedure make_crc_table;
var
  c   : Cardinal;
  n, k: Integer;
begin

  for n := 0 to 255 do
  begin
     c := n;
     for k := 0 to 7 do
     begin
       if boolean(c and 1) then
         c := $edb88320 xor (c shr 1)
       else
         c := c shr 1;
     end;
     crc_table[n] := c;
  end;

  crc_table_computed := 1;
end;

(*: Update a running CRC with the bytes buf[0..len-1]--the CRC
   should be initialized to all 1's, and the transmitted value
   is the 1's complement of the final running CRC (see the
   crc() routine below)). *)
function update_crc(chunktype: tchunktype; crc: Cardinal; buf: pByteArray;
  len: Integer): Cardinal;
var
  c: Cardinal absolute crc;
  n: Integer;
begin
  if not boolean(crc_table_computed) then
    make_crc_table;

  for n := 0 to 3 do
    c := crc_table[(c XOR ord(chunktype[n])) AND $ff] XOR (c SHR 8);

  for n := 0 to Len - 1 do
    c := crc_table[(c XOR buf[n]) AND $ff] XOR (c SHR 8);

  Result := C;
end;

(*: Return the CRC of the bytes buf[0..len-1]. *)
function crc(chunktype: tchunktype; buf: pbytearray; len: Integer): Cardinal;
begin
  result := update_crc(chunktype, Cardinal($ffffffff), buf, len) xor Cardinal($ffffffff); { Paul }
end;


{When the compiled unit is being initialized}


initialization
  ChunkClasses := TChunkClasses.Create;

  {Register the chunk classes}
  RegisterNewChunkClass('IEND', TChunkIEND);
  RegisterNewChunkClass('IHDR', TChunkIHDR);
  RegisterNewChunkClass('gAMA', TChunkGAMA);
  RegisterNewChunkClass('IDAT', TChunkIDAT);
  RegisterNewChunkClass('PLTE', TChunkPLTE);
  RegisterNewChunkClass('tEXt', TChunkTEXT);
  RegisterNewChunkClass('tRNS', TChunkTRNS);
  RegisterNewChunkClass('tIME', TChunkTRNS);

  {Register the graphical class}
  TPicture.RegisterFileFormat('PNG', 'Portable Network Graphics', TPNGImage);

{When the compiled unit is being finalized}
finalization
  ChunkClasses.Free;

  {Unregister the graphical class}
  TPicture.UnregisterGraphicClass(TPNGImage);

end.





