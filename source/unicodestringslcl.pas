//unit unicodestringslcl;
//
//{$mode objfpc}{$H+}
//
//interface
//
//uses
//  Classes, SysUtils;

{$ifdef SECTION_INTERFACE}
type

  TUnicodeStrings = class;

  { TUnicodeStringsEnumerator class }

  TUnicodeStringsEnumerator = class
  private
    FStrings: TUnicodeStrings;
    FPosition: integer;
  public
    constructor Create(AStrings: TUnicodeStrings);
    function GetCurrent: UnicodeString;
    function MoveNext: boolean;
    property Current: UnicodeString read GetCurrent;
  end;

  { TStrings class }

  TUnicodeStrings = class(TPersistent)
  private
    FSpecialCharsInited: boolean;
    FQuoteChar: UnicodeChar;
    FDelimiter: UnicodeChar;
    FNameValueSeparator: UnicodeChar;
    FUpdateCount: integer;
    FAdapter: IStringsAdapter;
    FLBS: TTextLineBreakStyle;
    FStrictDelimiter: boolean;
//    function GetCommaText: UnicodeString;
    function GetName(Index: integer): UnicodeString;
    function GetValue(const Name: UnicodeString): UnicodeString;
    function GetLBS: TTextLineBreakStyle;
    procedure SetLBS(AValue: TTextLineBreakStyle);
    procedure ReadData(Reader: TReader);
//    procedure SetCommaText(const Value: UnicodeString);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: UnicodeString);
    procedure SetDelimiter(c: UnicodeChar);
    procedure SetQuoteChar(c: UnicodeChar);
    procedure SetNameValueSeparator(c: UnicodeChar);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: UnicodeString; Data: integer);
//    procedure Error(const Msg: pstring; Data: integer);
    function Get(Index: integer): UnicodeString; virtual; abstract;
    function GetCapacity: integer; virtual;
    function GetCount: integer; virtual; abstract;
    function GetObject(Index: integer): TObject; virtual;
    function GetTextStr: UnicodeString; virtual;
    procedure Put(Index: integer; const S: UnicodeString); virtual;
    procedure PutObject(Index: integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: integer); virtual;
    procedure SetTextStr(const Value: UnicodeString); virtual;
    procedure SetUpdateState(Updating: boolean); virtual;
    property UpdateCount: integer read FUpdateCount;
    function DoCompareText(const s1, s2: UnicodeString): PtrInt; virtual;
//    function GetDelimitedText: UnicodeString;
//    procedure SetDelimitedText(const AValue: UnicodeString);
    function GetValueFromIndex(Index: integer): UnicodeString;
    procedure SetValueFromIndex(Index: integer; const Value: UnicodeString);
    procedure CheckSpecialChars;
  public
    destructor Destroy; override;
    function Add(const S: UnicodeString): integer; virtual;
    function AddObject(const S: UnicodeString; AObject: TObject): integer; virtual;
    procedure Append(const S: UnicodeString);
    procedure AddStrings(TheStrings: TUnicodeStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Obj: TObject): boolean; override;
//    function Equals(TheStrings: TUnicodeStrings): boolean; // overload;
    procedure Exchange(Index1, Index2: integer); virtual;
    function GetEnumerator: TUnicodeStringsEnumerator;
//    function GetText: PUnicodeChar; virtual;
    function IndexOf(const S: UnicodeString): integer; virtual;
    function IndexOfName(const Name: UnicodeString): integer; virtual;
    function IndexOfObject(AObject: TObject): integer; virtual;
    procedure Insert(Index: integer; const S: UnicodeString); virtual; abstract;
    procedure InsertObject(Index: integer; const S: UnicodeString; AObject: TObject);
    procedure LoadFromFile(const FileName: String); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: integer); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(TheText: PUnicodeChar); virtual;
    procedure GetNameValue(Index: integer; Out AName, AValue: UnicodeString);
    function ExtractName(const S: UnicodeString): UnicodeString;
    property TextLineBreakStyle: TTextLineBreakStyle read GetLBS write SetLBS;
    property Delimiter: UnicodeChar read FDelimiter write SetDelimiter;
//    property DelimitedText: UnicodeString read GetDelimitedText write SetDelimitedText;
    property StrictDelimiter: boolean read FStrictDelimiter write FStrictDelimiter;
    property QuoteChar: UnicodeChar read FQuoteChar write SetQuoteChar;
    property NameValueSeparator: UnicodeChar read FNameValueSeparator write SetNameValueSeparator;
    property ValueFromIndex[Index: integer]: UnicodeString
      read GetValueFromIndex write SetValueFromIndex;
    property Capacity: integer read GetCapacity write SetCapacity;
//    property CommaText: UnicodeString read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
    property Names[Index: integer]: UnicodeString read GetName;
    property Objects[Index: integer]: TObject read GetObject write PutObject;
    property Values[const Name: UnicodeString]: UnicodeString read GetValue write SetValue;
    property Strings[Index: integer]: UnicodeString read Get write Put; default;
    property Text: UnicodeString read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
  end;

  ///

  TUnicodeStringList = class;
  TUnicodeStringListSortCompare = function(List: TUnicodeStringList; Index1, Index2: Integer): Integer;

  PUnicodeStringItem = ^TUnicodeStringItem;
  TUnicodeStringItem = record
    FString: UnicodeString;
    FObject: TObject;
  end;

  PUnicodeStringItemList = ^TUnicodeStringItemList;
  TUnicodeStringItemList = array[0..MaxListSize] of TUnicodeStringItem;

  TUnicodeStringList = class(TUnicodeStrings)
  private
    FList: PUnicodeStringItemList;
    FCount: integer;
    FCapacity: integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FDuplicates: TDuplicates;
    FCaseSensitive: boolean;
    FSorted: boolean;
    FOwnsObjects: boolean;
    procedure ExchangeItems(Index1, Index2: integer);
    procedure Grow;
    procedure QuickSort(L, R: integer; CompareFn: TUnicodeStringListSortCompare);
    procedure SetSorted(Value: boolean);
    procedure SetCaseSensitive(b: boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure CheckError(Index: integer);
    function Get(Index: integer): UnicodeString; override;
    function GetCapacity: integer; override;
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: UnicodeString); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer); override;
    procedure SetUpdateState(Updating: boolean); override;
    procedure InsertItem(Index: integer; const S: UnicodeString); virtual; overload;
    procedure InsertItem(Index: integer; const S: UnicodeString; O: TObject); virtual; overload;
    function DoCompareText(const s1, s2: UnicodeString): PtrInt; override;

  public
    destructor Destroy; override;
    function Add(const S: UnicodeString): integer; override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Exchange(Index1, Index2: integer); override;
    function Find(const S: UnicodeString; var Index: integer): boolean; virtual;
    function IndexOf(const S: UnicodeString): integer; override;
    procedure Insert(Index: integer; const S: UnicodeString); override;
    procedure Sort; virtual;
    procedure CustomSort(CompareFn: TUnicodeStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: boolean read FSorted write SetSorted;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
  end;


Const
  // Ratio of Pointer and Word Size.
  WordRatio = SizeOf(Pointer) Div SizeOf(Word);

{$endif}

//implementation

{$ifdef SECTION_IMPLEMENTATION}

{****************************************************************************}
{*                        TUnicodeStringsEnumerator                                *}
{****************************************************************************}

constructor TUnicodeStringsEnumerator.Create(AStrings: TUnicodeStrings);
begin
  inherited Create;
  FStrings := AStrings;
  FPosition := -1;
end;

function TUnicodeStringsEnumerator.GetCurrent: UnicodeString;
begin
  Result := FStrings[FPosition];
end;

function TUnicodeStringsEnumerator.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FStrings.Count;
end;

{****************************************************************************}
{*                             TUnicodeStrings                                     *}
{****************************************************************************}

// Function to quote text. Should move maybe to sysutils !!
// Also, it is not clear at this point what exactly should be done.

{ //!! is used to mark unsupported things. }

function QuoteString(const S: UnicodeString; Quote: UnicodeString): UnicodeString;
var
  I, J: integer;
begin
  J := 0;
  Result := S;
  for i := 1 to length(s) do
  begin
    Inc(j);
    if S[i] = Quote then
    begin
      System.Insert(Quote, Result, J);
      Inc(j);
    end;
  end;
  Result := Quote + Result + Quote;
end;

{
  For compatibility we can't add a Constructor to TucStrings to initialize
  the special characters. Therefore we add a routine which is called whenever
  the special chars are needed.
}

procedure TUnicodeStrings.CheckSpecialChars;
begin
  if not FSpecialCharsInited then
  begin
    FQuoteChar := '"';
    FDelimiter := ',';
    FNameValueSeparator := '=';
    FSpecialCharsInited := True;
    FLBS := DefaultTextLineBreakStyle;
  end;
end;

function TUnicodeStrings.GetLBS: TTextLineBreakStyle;
begin
  CheckSpecialChars;
  Result := FLBS;
end;

procedure TUnicodeStrings.SetLBS(AValue: TTextLineBreakStyle);
begin
  CheckSpecialChars;
  FLBS := AValue;
end;

procedure TUnicodeStrings.SetDelimiter(c: UnicodeChar);
begin
  CheckSpecialChars;
  FDelimiter := c;
end;

procedure TUnicodeStrings.SetQuoteChar(c: UnicodeChar);
begin
  CheckSpecialChars;
  FQuoteChar := c;
end;

procedure TUnicodeStrings.SetNameValueSeparator(c: UnicodeChar);
begin
  CheckSpecialChars;
  FNameValueSeparator := c;
end;
{
function TUnicodeStrings.GetCommaText: UnicodeString;
var
  C1, C2: UnicodeChar;
  FSD: boolean;
begin
  CheckSpecialChars;
  FSD := StrictDelimiter;
  C1 := Delimiter;
  C2 := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  StrictDelimiter := False;
  try
    Result := GetDelimitedText;
  finally
    Delimiter := C1;
    QuoteChar := C2;
    StrictDelimiter := FSD;
  end;
end;

function TUnicodeStrings.GetDelimitedText: UnicodeString;
var
  I: integer;
  p: PUnicodeChar;
  c: set of Char;
  S: UnicodeString;
begin
  CheckSpecialChars;
  Result := '';
  if StrictDelimiter then
    c := [#0, Delimiter]
  else
    c := [#0..' ', QuoteChar, Delimiter];
  for i := 0 to Count - 1 do
  begin
    S := Strings[i];
    p := PUnicodeChar(S);
    while not (p^ in c) do
      Inc(p);
    // strings in list may contain #0
    if (p <> PUnicodeChar(S) + length(S)) and not StrictDelimiter then
      Result := Result + QuoteString(S, QuoteChar)
    else
      Result := Result + S;
    if I < Count - 1 then
      Result := Result + Delimiter;
  end;
  if (Length(Result) = 0) and (Count = 1) then
    Result := QuoteChar + QuoteChar;
end;
}
procedure TUnicodeStrings.GetNameValue(Index: integer; Out AName, AValue: UnicodeString);
var
  L: longint;
begin
  CheckSpecialChars;
  AValue := Strings[Index];
  L := Pos(FNameValueSeparator, AValue);
  if L <> 0 then
  begin
    AName := Copy(AValue, 1, L - 1);
    System.Delete(AValue, 1, L);
  end
  else
    AName := '';
end;

function TUnicodeStrings.ExtractName(const s: UnicodeString): UnicodeString;
var
  L: longint;
begin
  CheckSpecialChars;
  L := Pos(FNameValueSeparator, S);
  if L <> 0 then
    Result := Copy(S, 1, L - 1)
  else
    Result := '';
end;

function TUnicodeStrings.GetName(Index: integer): UnicodeString;
var
  V: UnicodeString;
begin
  GetNameValue(Index, Result, V);
end;

function TUnicodeStrings.GetValue(const Name: UnicodeString): UnicodeString;
var
  L: longint;
  N: UnicodeString;
begin
  Result := '';
  L := IndexOfName(Name);
  if L <> -1 then
    GetNameValue(L, N, Result);
end;

function TUnicodeStrings.GetValueFromIndex(Index: integer): UnicodeString;
var
  N: UnicodeString;
begin
  GetNameValue(Index, N, Result);
end;

procedure TUnicodeStrings.SetValueFromIndex(Index: integer; const Value: UnicodeString);
begin
  if (Value = '') then
    Delete(Index)
  else begin
    if (Index < 0) then
      Index := Add('');
    CheckSpecialChars;
    Strings[Index] := GetName(Index) + FNameValueSeparator + Value;
  end;
end;

procedure TUnicodeStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(UTF8Decode(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;
{
procedure TUnicodeStrings.SetDelimitedText(const AValue: UnicodeString);
var
  i, j: integer;
  aNotFirst: boolean;
begin
  CheckSpecialChars;
  BeginUpdate;
  i := 1;
  j := 1;
  aNotFirst := False;
  try
    Clear;
    if StrictDelimiter then
    begin
      // Easier, faster loop.
      while I <= Length(AValue) do
      begin
        if (AValue[I] in [FDelimiter, #0]) then
        begin
          Add(Copy(AValue, J, I - J));
          J := I + 1;
        end;
        Inc(i);
      end;
      if (Length(AValue) > 0) then
        Add(Copy(AValue, J, I - J));
    end
    else begin
      while i <= length(AValue) do
      begin
        // skip delimiter
        if aNotFirst and (i <= length(AValue)) and (AValue[i] = FDelimiter) then
          Inc(i);
        // skip spaces
        while (i <= length(AValue)) and (Ord(AValue[i]) <= Ord(' ')) do
          Inc(i);
        // read next UnicodeString
        if i <= length(AValue) then
        begin
          if AValue[i] = FQuoteChar then
          begin
            // next UnicodeString is quoted
            j := i + 1;
            while (j <= length(AValue)) and ((AValue[j] <> FQuoteChar) or
                ((j + 1 <= length(AValue)) and (AValue[j + 1] = FQuoteChar))) do
            begin
              if (j <= length(AValue)) and (AValue[j] = FQuoteChar) then
                Inc(j, 2)
              else
                Inc(j);
            end;
            // j is position of closing quote
            Add(StringReplace(Copy(AValue, i + 1, j - i - 1),
              FQuoteChar + FQuoteChar, FQuoteChar, [rfReplaceAll]));
            i := j + 1;
          end
          else begin
            // next UnicodeString is not quoted
            j := i;
            while (j <= length(AValue)) and (Ord(AValue[j]) > Ord(' ')) and
              (AValue[j] <> FDelimiter) do
              Inc(j);
            Add(Copy(AValue, i, j - i));
            i := j;
          end;
        end
        else begin
          if aNotFirst then
            Add('');
        end;
        // skip spaces
        while (i <= length(AValue)) and (Ord(AValue[i]) <= Ord(' ')) do
          Inc(i);
        aNotFirst := True;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.SetCommaText(const Value: UnicodeString);
var
  C1, C2: UnicodeChar;
begin
  CheckSpecialChars;
  C1 := Delimiter;
  C2 := QuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    Delimiter := C1;
    QuoteChar := C2;
  end;
end;
}
procedure TUnicodeStrings.SetStringsAdapter(const Value: IStringsAdapter);
begin
end;

procedure TUnicodeStrings.SetValue(const Name, Value: UnicodeString);
var
  L: longint;
begin
  CheckSpecialChars;
  L := IndexOfName(Name);
  if L = -1 then
    Add(Name + FNameValueSeparator + Value)
  else
    Strings[L] := Name + FNameValueSeparator + Value;
end;

procedure TUnicodeStrings.WriteData(Writer: TWriter);
var
  i: integer;
begin
  Writer.WriteListBegin;
  for i := 0 to Count - 1 do
    Writer.WriteString(UTF8Encode(Strings[i]));
  Writer.WriteListEnd;
end;

procedure TUnicodeStrings.DefineProperties(Filer: TFiler);
var
  HasData: boolean;
begin
  if Assigned(Filer.Ancestor) then
    // Only serialize if UnicodeString list is different from ancestor
    if Filer.Ancestor.InheritsFrom(TUnicodeStrings) then
      HasData := not Equals(TUnicodeStrings(Filer.Ancestor))
    else
      HasData := True
  else
    HasData := Count > 0;
  Filer.DefineProperty('Strings', ReadData, WriteData, HasData);
end;

procedure TUnicodeStrings.Error(const Msg: UnicodeString; Data: integer);
begin
  raise EStringListError.CreateFmt(UTF8Encode(Msg), [Data]) at get_caller_addr(get_frame);
end;

//procedure TUnicodeStrings.Error(const Msg: pstring; Data: integer);
//begin
//  raise EStringListError.CreateFmt(Msg^, [Data]) at get_caller_addr(get_frame);
//end;

function TUnicodeStrings.GetCapacity: integer;
begin
  Result := Count;
end;

function TUnicodeStrings.GetObject(Index: integer): TObject;
begin
  Result := nil;
end;

function TUnicodeStrings.GetTextStr: UnicodeString;
var
  P: PUnicodeChar;
  I, L, NLS: longint;
  S, NL: UnicodeString;
begin
  CheckSpecialChars;
  // Determine needed place
  case FLBS of
    tlbsLF: NL := #10;
    tlbsCRLF: NL := #13#10;
    tlbsCR: NL := #13;
  end;
  L := 0;
  NLS := Length(NL);
  for I := 0 to Count - 1 do
    L := L + Length(Strings[I]) + NLS;
  Setlength(Result, L);
  P := Pointer(Result);
  for i := 0 to Count - 1 do
  begin
    S := Strings[I];
    L := Length(S);
    if L <> 0 then
      System.Move(Pointer(S)^, P^, L * sizeof(UnicodeChar));
    P := P + L;
    for L := 1 to NLS do
    begin
      P^ := NL[L];
      Inc(P);
    end;
  end;
end;

procedure TUnicodeStrings.Put(Index: integer; const S: UnicodeString);
var
  Obj: TObject;
begin
  Obj := Objects[Index];
  Delete(Index);
  InsertObject(Index, S, Obj);
end;

procedure TUnicodeStrings.PutObject(Index: integer; AObject: TObject);
begin
  // Empty.
end;

procedure TUnicodeStrings.SetCapacity(NewCapacity: integer);
begin
  // Empty.
end;

function GetNextLine(const Value: UnicodeString; out S: UnicodeString; var P: integer): boolean;
var
  PS: PUnicodeChar;
  IP, L: integer;
begin
  L := Length(Value);
  S := '';
  Result := False;
  if ((L - P) < 0) then
    exit;
  if ((L - P) = 0) and (not (Value[P] in [#10, #13])) then
  begin
    s := Value[P];
    Inc(P);
    Exit(True);
  end;
  PS := PUnicodeChar(Value) + P - 1;
  IP := P;
  while ((L - P) >= 0) and (not (PS^ in [#10, #13])) do
  begin
    P := P + 1;
    Inc(PS);
  end;
  SetLength(S, P - IP);
  System.Move(Value[IP], Pointer(S)^, (P - IP) * sizeof(UnicodeChar));
  if (P <= L) and (Value[P] = #13) then
    Inc(P);
  if (P <= L) and (Value[P] = #10) then
    Inc(P); // Point to character after #10(#13)
  Result := True;
end;

procedure TUnicodeStrings.SetTextStr(const Value: UnicodeString);
var
  S: UnicodeString;
  P: integer;
begin
  try
    beginUpdate;
    Clear;
    P := 1;
    while GetNextLine(Value, S, P) do
      Add(S);
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.SetUpdateState(Updating: boolean);
begin
end;

destructor TUnicodeStrings.Destroy;
begin
  inherited Destroy;
end;

function TUnicodeStrings.Add(const S: UnicodeString): integer;
begin
  Result := Count;
  Insert(Count, S);
end;

function TUnicodeStrings.AddObject(const S: UnicodeString; AObject: TObject): integer;
begin
  Result := Add(S);
  Objects[Result] := AObject;
end;

procedure TUnicodeStrings.Append(const S: UnicodeString);
begin
  Add(S);
end;

procedure TUnicodeStrings.AddStrings(TheStrings: TUnicodeStrings);
var
  Runner: longint;
begin
  try
    beginupdate;
    for Runner := 0 to TheStrings.Count - 1 do
      self.AddObject(Thestrings[Runner], TheStrings.Objects[Runner]);
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.Assign(Source: TPersistent);
var
  S: TUnicodeStrings;
begin
  if Source is TUnicodeStrings then
  begin
    S := TUnicodeStrings(Source);
    BeginUpdate;
    try
      Clear;
      FSpecialCharsInited := S.FSpecialCharsInited;
      FQuoteChar := S.FQuoteChar;
      FDelimiter := S.FDelimiter;
      FNameValueSeparator := S.FNameValueSeparator;
      FLBS := S.FLBS;
      AddStrings(S);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TUnicodeStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TUnicodeStrings.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TUnicodeStrings.Equals(Obj: TObject): boolean;
var
  TheStrings: TUnicodeStrings absolute Obj;
  Runner, Nr: longint;
begin
  if Obj is TUnicodeStrings then
  begin
    Nr := Self.Count;
    Result := Nr = TheStrings.Count;
    if Result then
      for Runner := 0 to Nr - 1 do
      begin
        Result := Strings[Runner] = TheStrings[Runner];
        if not Result then
          Break;
      end;
  end
  else
    Result := inherited Equals(Obj);
end;

procedure TUnicodeStrings.Exchange(Index1, Index2: integer);
var
  Obj: TObject;
  Str: UnicodeString;
begin
  try
    beginUpdate;
    Obj := Objects[Index1];
    Str := Strings[Index1];
    Objects[Index1] := Objects[Index2];
    Strings[Index1] := Strings[Index2];
    Objects[Index2] := Obj;
    Strings[Index2] := Str;
  finally
    EndUpdate;
  end;
end;

function TUnicodeStrings.GetEnumerator: TUnicodeStringsEnumerator;
begin
  Result := TUnicodeStringsEnumerator.Create(Self);
end;
{
function TUnicodeStrings.GetText: PUnicodeChar;
begin
  Result := StrNew(PUnicodeChar(Self.Text));
end;
}
function TUnicodeStrings.DoCompareText(const s1, s2: UnicodeString): PtrInt;
begin
  Result := UnicodeCompareText(s1, s2);
end;

function TUnicodeStrings.IndexOf(const S: UnicodeString): integer;
begin
  Result := 0;
  while (Result < Count) and (DoCompareText(Strings[Result], S) <> 0) do
    Result := Result + 1;
  if Result = Count then
    Result := -1;
end;

function TUnicodeStrings.IndexOfName(const Name: UnicodeString): integer;
var
  len: longint;
  S: UnicodeString;
begin
  CheckSpecialChars;
  Result := 0;
  while (Result < Count) do
  begin
    S := Strings[Result];
    len := pos(FNameValueSeparator, S) - 1;
    if (len > 0) and (DoCompareText(Name, Copy(S, 1, Len)) = 0) then
      exit;
    Inc(Result);
  end;
  Result := -1;
end;

function TUnicodeStrings.IndexOfObject(AObject: TObject): integer;
begin
  Result := 0;
  while (Result < Count) and (Objects[Result] <> AObject) do
    Result := Result + 1;
  if Result = Count then
    Result := -1;
end;

procedure TUnicodeStrings.InsertObject(Index: integer; const S: UnicodeString; AObject: TObject);

begin
  Insert(Index, S);
  Objects[Index] := AObject;
end;

procedure TUnicodeStrings.LoadFromFile(const FileName: String);
var
  TheStream: TFileStream;
begin
  TheStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TUnicodeStrings.LoadFromStream(Stream: TStream);
{
   Borlands method is no good, since a pipe for
   instance doesn't have a size.
   So we must do it the hard way.
}
const
  BufSize = 1024;
  MaxGrow = 1 shl 29;
var
  Buffer: ansistring;
  BytesRead, BufLen, I, BufDelta: longint;
begin
  // reread into a buffer
  try
    beginupdate;
    Buffer := '';
    BufLen := 0;
    I := 1;
    repeat
      BufDelta := BufSize * I;
      SetLength(Buffer, BufLen + BufDelta);
      BytesRead := Stream.Read(Buffer[BufLen + 1], BufDelta);
      Inc(BufLen, BufDelta);
      if I < MaxGrow then
        I := I shl 1;
    until BytesRead <> BufDelta;
    SetLength(Buffer, BufLen - BufDelta + BytesRead);
    SetTextStr(UTF8Decode(Buffer));
    SetLength(Buffer, 0);
  finally
    EndUpdate;
  end;
end;

procedure TUnicodeStrings.Move(CurIndex, NewIndex: integer);
var
  Obj: TObject;
  Str: UnicodeString;
begin
  BeginUpdate;
  Obj := Objects[CurIndex];
  Str := Strings[CurIndex];
  Delete(Curindex);
  InsertObject(NewIndex, Str, Obj);
  EndUpdate;
end;

procedure TUnicodeStrings.SaveToFile(const FileName: String);
var
  TheStream: TFileStream;
begin
  TheStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(TheStream);
  finally
    TheStream.Free;
  end;
end;

procedure TUnicodeStrings.SaveToStream(Stream: TStream);
var
  S: UnicodeString;
begin
  S := Text;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TUnicodeStrings.SetText(TheText: PUnicodeChar);
var
  S: UnicodeString;
begin
  if TheText <> nil then
    S := TheText
  else
    S := '';
  SetTextStr(S);
end;

{****************************************************************************}
{*                             TUnicodeStringList                                  *}
{****************************************************************************}

procedure TUnicodeStringList.ExchangeItems(Index1, Index2: integer);
var
  P1, P2: Pointer;
begin
  P1 := Pointer(Flist^[Index1].FString);
  P2 := Pointer(Flist^[Index1].FObject);
  Pointer(Flist^[Index1].Fstring) := Pointer(Flist^[Index2].Fstring);
  Pointer(Flist^[Index1].FObject) := Pointer(Flist^[Index2].FObject);
  Pointer(Flist^[Index2].Fstring) := P1;
  Pointer(Flist^[Index2].FObject) := P2;
end;

procedure TUnicodeStringList.Grow;
var
  NC: integer;
begin
  NC := FCapacity;
  if NC >= 256 then
    NC := NC + (NC div 4)
  else if NC = 0 then
    NC := 4
  else
    NC := NC * 4;
  SetCapacity(NC);
end;

procedure TUnicodeStringList.QuickSort(L, R: integer; CompareFn: TUnicodeStringListSortCompare);
var
  Pivot, vL, vR: integer;
begin
  if R - L <= 1 then
  begin // a little bit of time saver
    if L < R then
      if CompareFn(Self, L, R) > 0 then
        ExchangeItems(L, R);
    Exit;
  end;
  vL := L;
  vR := R;
  Pivot := L + Random(R - L); // they say random is best
  while vL < vR do
  begin
    while (vL < Pivot) and (CompareFn(Self, vL, Pivot) <= 0) do
      Inc(vL);
    while (vR > Pivot) and (CompareFn(Self, vR, Pivot) > 0) do
      Dec(vR);
    ExchangeItems(vL, vR);
    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;
  if Pivot - 1 >= L then
    QuickSort(L, Pivot - 1, CompareFn);
  if Pivot + 1 <= R then
    QuickSort(Pivot + 1, R, CompareFn);
end;

procedure TUnicodeStringList.InsertItem(Index: integer; const S: UnicodeString);
begin
  Changing;
  if FCount = Fcapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TUnicodeStringItem));
  Pointer(Flist^[Index].Fstring) := nil;  // Needed to initialize...
  Flist^[Index].FString := S;
  Flist^[Index].Fobject := nil;
  Inc(FCount);
  Changed;
end;

procedure TUnicodeStringList.InsertItem(Index: integer; const S: UnicodeString; O: TObject);
begin
  Changing;
  if FCount = Fcapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TUnicodeStringItem));
  Pointer(Flist^[Index].Fstring) := nil;  // Needed to initialize...
  Flist^[Index].FString := S;
  Flist^[Index].FObject := O;
  Inc(FCount);
  Changed;
end;

procedure TUnicodeStringList.SetSorted(Value: boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      sort;
    FSorted := Value;
  end;
end;

procedure TUnicodeStringList.Changed;
begin
  if (FUpdateCount = 0) then
    if Assigned(FOnChange) then
      FOnchange(Self);
end;

procedure TUnicodeStringList.Changing;
begin
  if FUpdateCount = 0 then
    if Assigned(FOnChanging) then
      FOnchanging(Self);
end;

procedure TUnicodeStringList.CheckError(Index: integer);
begin
  if (Index < 0) or (Index >= Fcount) then
    raise Exception.Create(Format('Index %d out of range in TUnicodeStringList.', [Index]));
end;

function TUnicodeStringList.Get(Index: integer): UnicodeString;
begin
  CheckError(Index);
  Result := Flist^[Index].FString;
end;

function TUnicodeStringList.GetCapacity: integer;
begin
  Result := FCapacity;
end;

function TUnicodeStringList.GetCount: integer;
begin
  Result := FCount;
end;

function TUnicodeStringList.GetObject(Index: integer): TObject;
begin
  CheckError(Index);
  Result := Flist^[Index].FObject;
end;

procedure TUnicodeStringList.Put(Index: integer; const S: UnicodeString);
begin
  if Sorted then
    raise Exception.Create('List is sorted in TUnicodeStringList.Put.');
  CheckError(Index);
  Changing;
  Flist^[Index].FString := S;
  Changed;
end;

procedure TUnicodeStringList.PutObject(Index: integer; AObject: TObject);
begin
  CheckError(Index);
  Changing;
  Flist^[Index].FObject := AObject;
  Changed;
end;

procedure TUnicodeStringList.SetCapacity(NewCapacity: integer);
var
  NewList: Pointer;
  MSize: longint;
begin
  Assert(NewCapacity >= 0);
  if NewCapacity > FCapacity then
  begin
    GetMem(NewList, NewCapacity * SizeOf(TUnicodeStringItem));
    if NewList = nil then
      raise Exception.Create('NewList is Nil in TUnicodeStringList.SetCapacity.');
    if Assigned(FList) then
    begin
      MSize := FCapacity * Sizeof(TUnicodeStringItem);
      System.Move(FList^, NewList^, MSize);
      FillQWord(PChar(NewList)[MSize], (NewCapacity - FCapacity) * (SizeOf(TUnicodeStringItem) div SizeOf(QWord)), 0);
      FreeMem(FList, MSize);
    end;
    Flist := NewList;
    FCapacity := NewCapacity;
  end
  else if NewCapacity < FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      FreeMem(FList);
      FList := nil;
    end
    else begin
      GetMem(NewList, NewCapacity * SizeOf(TUnicodeStringItem));
      System.Move(FList^, NewList^, NewCapacity * SizeOf(TUnicodeStringItem));
      FreeMem(FList);
      FList := NewList;
    end;
    FCapacity := NewCapacity;
  end;
end;

procedure TUnicodeStringList.SetUpdateState(Updating: boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

destructor TUnicodeStringList.Destroy;
var
  I: longint;
begin
  FOnChange := nil;
  FOnChanging := nil;
  // This will force a dereference. Can be done better...
  for I := 0 to FCount - 1 do
    FList^[I].FString := '';
  FCount := 0;
  SetCapacity(0);
  inherited Destroy;
end;

function TUnicodeStringList.Add(const S: UnicodeString): integer;
begin
  if not Sorted then
    Result := FCount
  else
  if Find(S, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: raise Exception.Create('TUnicodeStringList.Duplicates does not allow duplicates.');
    end;
  InsertItem(Result, S);
end;

procedure TUnicodeStringList.Clear;
var
  I: longint;
begin
  if FCount = 0 then
    Exit;
  Changing;
  if FOwnsObjects then
  begin
    for I := 0 to FCount - 1 do
    begin
      Flist^[I].FString := '';
      FreeAndNil(Flist^[i].FObject);
    end;
  end
  else begin
    for I := 0 to FCount - 1 do
      Flist^[I].FString := '';
  end;
  FCount := 0;
  SetCapacity(0);
  Changed;
end;

procedure TUnicodeStringList.Delete(Index: integer);
begin
  CheckError(Index);
  Changing;
  Flist^[Index].FString := '';
  if FOwnsObjects then
    FreeAndNil(Flist^[Index].FObject);
  Dec(FCount);
  if Index < FCount then
    System.Move(Flist^[Index + 1], Flist^[Index], (Fcount - Index) * SizeOf(TUnicodeStringItem));
  Changed;
end;

procedure TUnicodeStringList.Exchange(Index1, Index2: integer);
begin
  CheckError(Index1);
  CheckError(Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  changed;
end;

procedure TUnicodeStringList.SetCaseSensitive(b: boolean);
begin
  if b <> FCaseSensitive then
  begin
    FCaseSensitive := b;
    if FSorted then
      sort;
  end;
end;

function TUnicodeStringList.DoCompareText(const s1, s2: UnicodeString): PtrInt;
begin
  if FCaseSensitive then
    Result := UnicodeCompareStr(s1, s2)
  else
    Result := UnicodeCompareText(s1, s2);
end;

function TUnicodeStringList.Find(const S: UnicodeString; var Index: integer): boolean;
var
  L, R, I: integer;
  CompareRes: PtrInt;
begin
  Result := False;
  // Use binary search.
  L := 0;
  R := Count - 1;
  while (L <= R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := DoCompareText(S, Flist^[I].FString);
    if (CompareRes > 0) then
      L := I + 1
    else begin
      R := I - 1;
      if (CompareRes = 0) then
      begin
        Result := True;
        if (Duplicates <> dupAccept) then
          L := I; // forces end of while loop
      end;
    end;
  end;
  Index := L;
end;

function TUnicodeStringList.IndexOf(const S: UnicodeString): integer;
begin
  if not Sorted then
    Result := inherited indexOf(S)
  else
  // faster using binary search...
  if not Find(S, Result) then
    Result := -1;
end;

procedure TUnicodeStringList.Insert(Index: integer; const S: UnicodeString);
begin
  if Sorted then
    raise Exception.Create('Operation not allowed on sorted list');
  if (Index < 0) or (Index > Fcount) then
    raise Exception.Create(Format('Index %d out of range in TUnicodeStringList.', [Index]));
  InsertItem(Index, S);
end;

procedure TUnicodeStringList.CustomSort(CompareFn: TUnicodeStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, CompareFn);
    Changed;
  end;
end;

function StringListAnsiCompare(List: TUnicodeStringList; Index1, Index: integer): integer;
begin
  Result := List.DoCompareText(List.FList^[Index1].FString,
    List.FList^[Index].FString);
end;

procedure TUnicodeStringList.Sort;
begin
  CustomSort(@StringListAnsiCompare);
end;

{$endif}

//end.

