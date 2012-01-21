unit WideStringsLcl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TWideStrings = class;

  { TWideStringsEnumerator class }

  TWideStringsEnumerator = class
  private
    FStrings: TWideStrings;
    FPosition: integer;
  public
    constructor Create(AStrings: TWideStrings);
    function GetCurrent: WideString;
    function MoveNext: boolean;
    property Current: WideString read GetCurrent;
  end;

  { TStrings class }

  TWideStrings = class(TPersistent)
  private
    FSpecialCharsInited: boolean;
    FQuoteChar: WideChar;
    FDelimiter: WideChar;
    FNameValueSeparator: WideChar;
    FUpdateCount: integer;
    FAdapter: IStringsAdapter;
    FLBS: TTextLineBreakStyle;
    FStrictDelimiter: boolean;
//    function GetCommaText: WideString;
    function GetName(Index: integer): WideString;
    function GetValue(const Name: WideString): WideString;
    function GetLBS: TTextLineBreakStyle;
    procedure SetLBS(AValue: TTextLineBreakStyle);
    procedure ReadData(Reader: TReader);
//    procedure SetCommaText(const Value: WideString);
    procedure SetStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: WideString);
    procedure SetDelimiter(c: WideChar);
    procedure SetQuoteChar(c: WideChar);
    procedure SetNameValueSeparator(c: WideChar);
    procedure WriteData(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: WideString; Data: integer);
    procedure Error(const Msg: pstring; Data: integer);
    function Get(Index: integer): WideString; virtual; abstract;
    function GetCapacity: integer; virtual;
    function GetCount: integer; virtual; abstract;
    function GetObject(Index: integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: integer; const S: WideString); virtual;
    procedure PutObject(Index: integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: boolean); virtual;
    property UpdateCount: integer read FUpdateCount;
    function DoCompareText(const s1, s2: WideString): PtrInt; virtual;
//    function GetDelimitedText: WideString;
//    procedure SetDelimitedText(const AValue: WideString);
    function GetValueFromIndex(Index: integer): WideString;
    procedure SetValueFromIndex(Index: integer; const Value: WideString);
    procedure CheckSpecialChars;
  public
    destructor Destroy; override;
    function Add(const S: WideString): integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(TheStrings: TWideStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: integer); virtual; abstract;
    procedure EndUpdate;
//    function Equals(Obj: TObject): boolean; overload;
    function Equals(TheStrings: TWideStrings): boolean; // overload;
    procedure Exchange(Index1, Index2: integer); virtual;
    function GetEnumerator: TWideStringsEnumerator;
//    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): integer; virtual;
    function IndexOfName(const Name: WideString): integer; virtual;
    function IndexOfObject(AObject: TObject): integer; virtual;
    procedure Insert(Index: integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: WideString); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: integer); virtual;
    procedure SaveToFile(const FileName: WideString); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(TheText: PWideChar); virtual;
    procedure GetNameValue(Index: integer; Out AName, AValue: WideString);
    function ExtractName(const S: WideString): WideString;
    property TextLineBreakStyle: TTextLineBreakStyle read GetLBS write SetLBS;
    property Delimiter: WideChar read FDelimiter write SetDelimiter;
//    property DelimitedText: WideString read GetDelimitedText write SetDelimitedText;
    property StrictDelimiter: boolean read FStrictDelimiter write FStrictDelimiter;
    property QuoteChar: WideChar read FQuoteChar write SetQuoteChar;
    property NameValueSeparator: WideChar read FNameValueSeparator write SetNameValueSeparator;
    property ValueFromIndex[Index: integer]: WideString
      read GetValueFromIndex write SetValueFromIndex;
    property Capacity: integer read GetCapacity write SetCapacity;
//    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
    property Names[Index: integer]: WideString read GetName;
    property Objects[Index: integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Strings[Index: integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read FAdapter write SetStringsAdapter;
  end;

  ///

  TWideStringList = class;
  TWideStringListSortCompare = function(List: TWideStringList; Index1, Index2: Integer): Integer;

  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

  TWideStringList = class(TWideStrings)
  private
    FList: PWideStringItemList;
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
    procedure QuickSort(L, R: integer; CompareFn: TWideStringListSortCompare);
    procedure SetSorted(Value: boolean);
    procedure SetCaseSensitive(b: boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure CheckError(Index: integer);
    function Get(Index: integer): WideString; override;
    function GetCapacity: integer; override;
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: WideString); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer); override;
    procedure SetUpdateState(Updating: boolean); override;
    procedure InsertItem(Index: integer; const S: WideString); virtual; overload;
    procedure InsertItem(Index: integer; const S: WideString; O: TObject); virtual; overload;
    function DoCompareText(const s1, s2: WideString): PtrInt; override;

  public
    destructor Destroy; override;
    function Add(const S: WideString): integer; override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure Exchange(Index1, Index2: integer); override;
    function Find(const S: WideString; out Index: integer): boolean; virtual;
    function IndexOf(const S: WideString): integer; override;
    procedure Insert(Index: integer; const S: WideString); override;
    procedure Sort; virtual;
    procedure CustomSort(CompareFn: TWideStringListSortCompare); virtual;
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


implementation

{****************************************************************************}
{*                        TWideStringsEnumerator                                *}
{****************************************************************************}

constructor TWideStringsEnumerator.Create(AStrings: TWideStrings);
begin
  inherited Create;
  FStrings := AStrings;
  FPosition := -1;
end;

function TWideStringsEnumerator.GetCurrent: WideString;
begin
  Result := FStrings[FPosition];
end;

function TWideStringsEnumerator.MoveNext: boolean;
begin
  Inc(FPosition);
  Result := FPosition < FStrings.Count;
end;

{****************************************************************************}
{*                             TWideStrings                                     *}
{****************************************************************************}

// Function to quote text. Should move maybe to sysutils !!
// Also, it is not clear at this point what exactly should be done.

{ //!! is used to mark unsupported things. }

function QuoteString(const S: WideString; Quote: WideString): WideString;
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

procedure TWideStrings.CheckSpecialChars;
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

function TWideStrings.GetLBS: TTextLineBreakStyle;
begin
  CheckSpecialChars;
  Result := FLBS;
end;

procedure TWideStrings.SetLBS(AValue: TTextLineBreakStyle);
begin
  CheckSpecialChars;
  FLBS := AValue;
end;

procedure TWideStrings.SetDelimiter(c: WideChar);
begin
  CheckSpecialChars;
  FDelimiter := c;
end;

procedure TWideStrings.SetQuoteChar(c: WideChar);
begin
  CheckSpecialChars;
  FQuoteChar := c;
end;

procedure TWideStrings.SetNameValueSeparator(c: WideChar);
begin
  CheckSpecialChars;
  FNameValueSeparator := c;
end;
{
function TWideStrings.GetCommaText: WideString;
var
  C1, C2: WideChar;
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

function TWideStrings.GetDelimitedText: WideString;
var
  I: integer;
  p: PWideChar;
  c: set of Char;
  S: WideString;
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
    p := PWideChar(S);
    while not (p^ in c) do
      Inc(p);
    // strings in list may contain #0
    if (p <> PWideChar(S) + length(S)) and not StrictDelimiter then
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
procedure TWideStrings.GetNameValue(Index: integer; Out AName, AValue: WideString);
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

function TWideStrings.ExtractName(const s: WideString): WideString;
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

function TWideStrings.GetName(Index: integer): WideString;
var
  V: WideString;
begin
  GetNameValue(Index, Result, V);
end;

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  L: longint;
  N: WideString;
begin
  Result := '';
  L := IndexOfName(Name);
  if L <> -1 then
    GetNameValue(L, N, Result);
end;

function TWideStrings.GetValueFromIndex(Index: integer): WideString;
var
  N: WideString;
begin
  GetNameValue(Index, N, Result);
end;

procedure TWideStrings.SetValueFromIndex(Index: integer; const Value: WideString);
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

procedure TWideStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;
{
procedure TWideStrings.SetDelimitedText(const AValue: WideString);
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
        // read next WideString
        if i <= length(AValue) then
        begin
          if AValue[i] = FQuoteChar then
          begin
            // next WideString is quoted
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
            // next WideString is not quoted
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

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  C1, C2: WideChar;
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
procedure TWideStrings.SetStringsAdapter(const Value: IStringsAdapter);
begin
end;

procedure TWideStrings.SetValue(const Name, Value: WideString);
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

procedure TWideStrings.WriteData(Writer: TWriter);
var
  i: integer;
begin
  Writer.WriteListBegin;
  for i := 0 to Count - 1 do
    Writer.WriteString(Strings[i]);
  Writer.WriteListEnd;
end;

procedure TWideStrings.DefineProperties(Filer: TFiler);
var
  HasData: boolean;
begin
  if Assigned(Filer.Ancestor) then
    // Only serialize if WideString list is different from ancestor
    if Filer.Ancestor.InheritsFrom(TWideStrings) then
      HasData := not Equals(TWideStrings(Filer.Ancestor))
    else
      HasData := True
  else
    HasData := Count > 0;
  Filer.DefineProperty('Strings', @ReadData, @WriteData, HasData);
end;

procedure TWideStrings.Error(const Msg: WideString; Data: integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]) at get_caller_addr(get_frame);
end;

procedure TWideStrings.Error(const Msg: pstring; Data: integer);
begin
  raise EStringListError.CreateFmt(Msg^, [Data]) at get_caller_addr(get_frame);
end;

function TWideStrings.GetCapacity: integer;
begin
  Result := Count;
end;

function TWideStrings.GetObject(Index: integer): TObject;
begin
  Result := nil;
end;

function TWideStrings.GetTextStr: WideString;
var
  P: PWideChar;
  I, L, NLS: longint;
  S, NL: WideString;
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
      System.Move(Pointer(S)^, P^, L * sizeof(WideChar));
    P := P + L;
    for L := 1 to NLS do
    begin
      P^ := NL[L];
      Inc(P);
    end;
  end;
end;

procedure TWideStrings.Put(Index: integer; const S: WideString);
var
  Obj: TObject;
begin
  Obj := Objects[Index];
  Delete(Index);
  InsertObject(Index, S, Obj);
end;

procedure TWideStrings.PutObject(Index: integer; AObject: TObject);
begin
  // Empty.
end;

procedure TWideStrings.SetCapacity(NewCapacity: integer);
begin
  // Empty.
end;

function GetNextLine(const Value: WideString; out S: WideString; var P: integer): boolean;
var
  PS: PWideChar;
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
  PS := PWideChar(Value) + P - 1;
  IP := P;
  while ((L - P) >= 0) and (not (PS^ in [#10, #13])) do
  begin
    P := P + 1;
    Inc(PS);
  end;
  SetLength(S, P - IP);
  System.Move(Value[IP], Pointer(S)^, (P - IP) * sizeof(WideChar));
  if (P <= L) and (Value[P] = #13) then
    Inc(P);
  if (P <= L) and (Value[P] = #10) then
    Inc(P); // Point to character after #10(#13)
  Result := True;
end;

procedure TWideStrings.SetTextStr(const Value: WideString);
var
  S: WideString;
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

procedure TWideStrings.SetUpdateState(Updating: boolean);
begin
end;

destructor TWideStrings.Destroy;
begin
  inherited Destroy;
end;

function TWideStrings.Add(const S: WideString): integer;
begin
  Result := Count;
  Insert(Count, S);
end;

function TWideStrings.AddObject(const S: WideString; AObject: TObject): integer;
begin
  Result := Add(S);
  Objects[Result] := AObject;
end;

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWideStrings.AddStrings(TheStrings: TWideStrings);
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

procedure TWideStrings.Assign(Source: TPersistent);
var
  S: TWideStrings;
begin
  if Source is TWideStrings then
  begin
    S := TWideStrings(Source);
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

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TWideStrings.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;
{
function TWideStrings.Equals(Obj: TObject): boolean;
begin
  if Obj is TWideStrings then
    Result := Equals(TWideStrings(Obj))
  else
    Result := inherited Equals(Obj);
end;
}
function TWideStrings.Equals(TheStrings: TWideStrings): boolean;
var
  Runner, Nr: longint;
begin
  Result := False;
  Nr := Self.Count;
  if Nr <> TheStrings.Count then
    exit;
  for Runner := 0 to Nr - 1 do
    if Strings[Runner] <> TheStrings[Runner] then
      exit;
  Result := True;
end;

procedure TWideStrings.Exchange(Index1, Index2: integer);
var
  Obj: TObject;
  Str: WideString;
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

function TWideStrings.GetEnumerator: TWideStringsEnumerator;
begin
  Result := TWideStringsEnumerator.Create(Self);
end;
{
function TWideStrings.GetText: PWideChar;
begin
  Result := StrNew(PWideChar(Self.Text));
end;
}
function TWideStrings.DoCompareText(const s1, s2: WideString): PtrInt;
begin
  Result := CompareText(s1, s2);
end;

function TWideStrings.IndexOf(const S: WideString): integer;
begin
  Result := 0;
  while (Result < Count) and (DoCompareText(Strings[Result], S) <> 0) do
    Result := Result + 1;
  if Result = Count then
    Result := -1;
end;

function TWideStrings.IndexOfName(const Name: WideString): integer;
var
  len: longint;
  S: WideString;
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

function TWideStrings.IndexOfObject(AObject: TObject): integer;
begin
  Result := 0;
  while (Result < Count) and (Objects[Result] <> AObject) do
    Result := Result + 1;
  if Result = Count then
    Result := -1;
end;

procedure TWideStrings.InsertObject(Index: integer; const S: WideString; AObject: TObject);

begin
  Insert(Index, S);
  Objects[Index] := AObject;
end;

procedure TWideStrings.LoadFromFile(const FileName: WideString);
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

procedure TWideStrings.LoadFromStream(Stream: TStream);
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
    SetTextStr(Buffer);
    SetLength(Buffer, 0);
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Move(CurIndex, NewIndex: integer);
var
  Obj: TObject;
  Str: WideString;
begin
  BeginUpdate;
  Obj := Objects[CurIndex];
  Str := Strings[CurIndex];
  Delete(Curindex);
  InsertObject(NewIndex, Str, Obj);
  EndUpdate;
end;

procedure TWideStrings.SaveToFile(const FileName: WideString);
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

procedure TWideStrings.SaveToStream(Stream: TStream);
var
  S: WideString;
begin
  S := Text;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TWideStrings.SetText(TheText: PWideChar);
var
  S: WideString;
begin
  if TheText <> nil then
    S := TheText
  else
    S := '';
  SetTextStr(S);
end;

{****************************************************************************}
{*                             TWideStringList                                  *}
{****************************************************************************}

procedure TWideStringList.ExchangeItems(Index1, Index2: integer);
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

procedure TWideStringList.Grow;
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

procedure TWideStringList.QuickSort(L, R: integer; CompareFn: TWideStringListSortCompare);
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

procedure TWideStringList.InsertItem(Index: integer; const S: WideString);
begin
  Changing;
  if FCount = Fcapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TWideStringItem));
  Pointer(Flist^[Index].Fstring) := nil;  // Needed to initialize...
  Flist^[Index].FString := S;
  Flist^[Index].Fobject := nil;
  Inc(FCount);
  Changed;
end;

procedure TWideStringList.InsertItem(Index: integer; const S: WideString; O: TObject);
begin
  Changing;
  if FCount = Fcapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TWideStringItem));
  Pointer(Flist^[Index].Fstring) := nil;  // Needed to initialize...
  Flist^[Index].FString := S;
  Flist^[Index].FObject := O;
  Inc(FCount);
  Changed;
end;

procedure TWideStringList.SetSorted(Value: boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      sort;
    FSorted := Value;
  end;
end;

procedure TWideStringList.Changed;
begin
  if (FUpdateCount = 0) then
    if Assigned(FOnChange) then
      FOnchange(Self);
end;

procedure TWideStringList.Changing;
begin
  if FUpdateCount = 0 then
    if Assigned(FOnChanging) then
      FOnchanging(Self);
end;

procedure TWideStringList.CheckError(Index: integer);
begin
  if (Index < 0) or (Index >= Fcount) then
    raise Exception.Create(Format('Index %d out of range in TWideStringList.', [Index]));
end;

function TWideStringList.Get(Index: integer): WideString;
begin
  CheckError(Index);
  Result := Flist^[Index].FString;
end;

function TWideStringList.GetCapacity: integer;
begin
  Result := FCapacity;
end;

function TWideStringList.GetCount: integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: integer): TObject;
begin
  CheckError(Index);
  Result := Flist^[Index].FObject;
end;

procedure TWideStringList.Put(Index: integer; const S: WideString);
begin
  if Sorted then
    raise Exception.Create('List is sorter in TucStringList.Put.');
  CheckError(Index);
  Changing;
  Flist^[Index].FString := S;
  Changed;
end;

procedure TWideStringList.PutObject(Index: integer; AObject: TObject);
begin
  CheckError(Index);
  Changing;
  Flist^[Index].FObject := AObject;
  Changed;
end;

procedure TWideStringList.SetCapacity(NewCapacity: integer);
var
  NewList: Pointer;
  MSize: longint;
begin
  Assert(NewCapacity >= 0);
  if NewCapacity > FCapacity then
  begin
    GetMem(NewList, NewCapacity * SizeOf(TWideStringItem));
    if NewList = nil then
      raise Exception.Create('NewList is Nil in TucStringList.SetCapacity.');
    if Assigned(FList) then
    begin
      MSize := FCapacity * Sizeof(TWideStringItem);
      System.Move(FList^, NewList^, MSize);
      FillWord(PWideChar(NewList)[MSize], (NewCapacity - FCapacity) * WordRatio, 0);
      FreeMem(Flist, MSize);
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
      GetMem(NewList, NewCapacity * SizeOf(TWideStringItem));
      System.Move(FList^, NewList^, NewCapacity * SizeOf(TWideStringItem));
      FreeMem(FList);
      FList := NewList;
    end;
    FCapacity := NewCapacity;
  end;
end;

procedure TWideStringList.SetUpdateState(Updating: boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

destructor TWideStringList.Destroy;
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

function TWideStringList.Add(const S: WideString): integer;
begin
  if not Sorted then
    Result := FCount
  else
  if Find(S, Result) then
    case DUplicates of
      DupIgnore: Exit;
      DupError: raise Exception.Create('WideString list does not allow duplicates.');
    end;
  InsertItem(Result, S);
end;

procedure TWideStringList.Clear;
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

procedure TWideStringList.Delete(Index: integer);
begin
  CheckError(Index);
  Changing;
  Flist^[Index].FString := '';
  if FOwnsObjects then
    FreeAndNil(Flist^[Index].FObject);
  Dec(FCount);
  if Index < FCount then
    System.Move(Flist^[Index + 1], Flist^[Index], (Fcount - Index) * SizeOf(TWideStringItem));
  Changed;
end;

procedure TWideStringList.Exchange(Index1, Index2: integer);
begin
  CheckError(Index1);
  CheckError(Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  changed;
end;

procedure TWideStringList.SetCaseSensitive(b: boolean);
begin
  if b <> FCaseSensitive then
  begin
    FCaseSensitive := b;
    if FSorted then
      sort;
  end;
end;

function TWideStringList.DoCompareText(const s1, s2: WideString): PtrInt;
begin
  if FCaseSensitive then
    Result := AnsiCompareStr(s1, s2)
  else
    Result := AnsiCompareText(s1, s2);
end;

function TWideStringList.Find(const S: WideString; out Index: integer): boolean;
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

function TWideStringList.IndexOf(const S: WideString): integer;
begin
  if not Sorted then
    Result := inherited indexOf(S)
  else
  // faster using binary search...
  if not Find(S, Result) then
    Result := -1;
end;

procedure TWideStringList.Insert(Index: integer; const S: WideString);
begin
  if Sorted then
    raise Exception.Create('Operation not allowed on sorted list');
  if (Index < 0) or (Index > Fcount) then
    raise Exception.Create(Format('Index %d out of range in TWideStringList.', [Index]));
  InsertItem(Index, S);
end;

procedure TWideStringList.CustomSort(CompareFn: TWideStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, CompareFn);
    Changed;
  end;
end;

function StringListAnsiCompare(List: TWideStringList; Index1, Index: integer): integer;
begin
  Result := List.DoCompareText(List.FList^[Index1].FString,
    List.FList^[Index].FString);
end;

procedure TWideStringList.Sort;
begin
  CustomSort(@StringListAnsiCompare);
end;


end.

